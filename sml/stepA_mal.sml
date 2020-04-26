(***** core.sml *****)

structure Core = struct

exception MalException of string

fun error msg = raise MalException msg

end

(***** avl trees *****)

signature TABLE =
sig
    type key
    type 'data Table

    val empty   : 'data Table
    val isEmpty : 'data Table -> bool
    val count   : 'data Table -> int
    val insert  : 'data Table -> (key * 'data) -> 'data Table
    val remove  : 'data Table -> key -> 'data Table
    val lookup  : 'data Table -> key -> 'data option
    val fold    : (key * 'data * 'a -> 'a) -> 'a -> 'data Table -> 'a
    val alist   : 'data Table -> (key * 'data) list
end

functor Table (type key'
               val compare : key' * key' -> order)
        :> TABLE where type key = key' =
struct

type key = key'

datatype 'data Tree = Empty
                    | Node of (key * 'data) * int * 'data Tree * 'data Tree

type 'a Table = 'a Tree

val empty = Empty

fun isEmpty Empty = true
  | isEmpty _ = false

fun height Empty = 0
  | height (Node (_, h, _, _)) = h

fun makeNode e lt rt = Node (e, 1 + Int.max (height lt, height rt), lt, rt)

fun balance Empty = 0
  | balance (Node (_, _, lt, rt)) = (height lt) - (height rt)

fun rotateR (Node (e, h, Node (le, lh, llt, lrt), rt)) =
    makeNode le llt (makeNode e lrt rt)
  | rotateR x = x

fun rotateL (Node (e, _, lt, Node (re, _, rlt, rrt))) =
    makeNode re (makeNode e lt rlt) rrt
  | rotateL x = x

fun rebalance Empty = Empty
  | rebalance (parent as Node (e, h, lt, rt)) =
    case (balance parent) of
        2  => (if (balance lt) = ~1
               then rotateR (makeNode e (rotateL lt) rt)
               else rotateR parent)
      | ~2 => (if (balance lt) = 1
               then rotateL (makeNode e lt (rotateR rt))
               else rotateL parent)
      |  _  => parent

fun insert Empty a = makeNode a Empty Empty
  | insert (Node (e as (k1, _), h, lt, rt)) (a as (k, _)) =
    case compare (k, k1) of
        EQUAL   => makeNode a lt rt
      | LESS    => rebalance (makeNode e (insert lt a) rt)
      | GREATER => rebalance (makeNode e lt (insert rt a))

fun popMin Empty = (Empty, Empty)
  | popMin (node as Node (e, _, Empty, rt)) = (node, rt)
  | popMin (node as Node (e, _, lt, rt)) =
    let val (mnode, mlt) = popMin lt
    in (mnode, rebalance (makeNode e mlt rt))
    end

fun popMax Empty = (Empty, Empty)
  | popMax (node as Node (e, _, lt, Empty)) = (node, lt)
  | popMax (node as Node (e, _, lt, rt)) =
    let val (mnode, mrt) = popMax rt
    in (mnode, rebalance (makeNode e lt mrt))
    end

fun remove Empty k = Empty
  | remove (Node (e as (k1, _), _, lt, rt)) k =
    case compare (k, k1) of
        EQUAL   => (case (lt, rt) of
                        (Empty, _) => rt
                      | (_, Empty) => lt
                      | _ => (case popMin rt of
                                  (Empty, _) => Core.error "Table remove exception"
                                | (Node (me, _, _, _), mrt) => rebalance (makeNode me lt mrt)))
      | LESS    => rebalance (makeNode e (remove lt k) rt)
      | GREATER => rebalance (makeNode e lt (remove rt k))

fun lookup Empty _ = NONE
  | lookup (Node ((k1,d1), _, lt, rt)) k =
    case compare (k, k1) of
        EQUAL   => SOME d1
      | LESS    => lookup lt k
      | GREATER => lookup rt k

fun fold ff arg Empty = arg
  | fold ff arg (Node ((k,v), _, lt, rt)) = ff (k, v, (fold ff (fold ff arg rt) lt))

fun count tr = fold (fn (_,_,va) => va + 1) 0 tr

fun alist tree =
    let fun lst Empty ls = ls
          | lst (Node (e, _, lt, rt)) ls = lst lt (e :: (lst rt ls))
    in
        lst tree []
    end

end

(***** types.sml *****)

structure SDic = Table (type key' = string
                        val compare = String.compare)

structure Mal = struct

datatype Value = Nil
               | String of string
               | Symbol of string
               | Keyword of string
               | Int of int
               | Real of real
               | Bool of bool
               | Atom of (Value ref * Value)
               | List of (Value list * Value)
               | Vector of (Value vector * Value)
               | Hashmap of ((Value * Value) SDic.Table * Value)
               | Function of (string * (Value list -> Value) * Value)
               | Closure of (string list * string option * Value list * EnvType ref * Value)
               | Macro of (string list * string option * Value list * EnvType ref * Value)

and EnvType = None
            | Env of {data: Value SDic.Table, outer: EnvType}

exception Throw of Value

val Symbols : Value SDic.Table ref = ref SDic.empty

fun makeKeyword name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val sym = Keyword name
                    in Symbols := SDic.insert syms (name, sym);
                       sym
                    end
          | SOME sym => sym
    end

fun makeSymbol name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val sym = Symbol name
                    in Symbols := SDic.insert syms (name, sym);
                       sym
                    end
          | SOME sym => sym
    end

val Quote = makeSymbol "quote"
val Quasiquote = makeSymbol "quasiquote"
val Unquote = makeSymbol "unquote"
val Splice_unquote = makeSymbol "splice-unquote"
val Deref = makeSymbol "deref"
val With_meta = makeSymbol "with-meta"

val Concat = makeSymbol "concat"
val Cons = makeSymbol "cons"

end

(***** env.sml *****)

structure Env = struct

fun make out = let val out' = !out
                   val env = Mal.Env { data = SDic.empty, outer = out' }
               in
                   ref env
               end

fun set env key value =
    let
        fun set' Mal.None key value =
                 Mal.Env {data = SDic.insert SDic.empty (key, value), outer = Mal.None}
          | set' (Mal.Env {data = dt, outer = ot}) key value =
                 Mal.Env {data = SDic.insert dt (key, value), outer = ot}

        val env' = !env
    in
        env := set' env' key value
    end

fun get env key =
    let
        fun get' Mal.None key = Core.error ("'" ^ key ^ "' not found")
          | get' (Mal.Env {data = dt, outer = ot}) key = (case SDic.lookup dt key of
                                                              NONE => get' ot key
                                                            | SOME v => v)

        val env' = !env
    in
        get' env' key
    end

fun find env key =
    let
        fun find' Mal.None _ = Mal.Nil
          | find' (Mal.Env {data = dt, outer = ot}) key = (case SDic.lookup dt key of
                                                               NONE => find' ot key
                                                             | SOME v => v)

        val env' = !env
    in
        find' env' key
    end

end

(***** printer.sml *****)

structure Printer = struct

fun pr_str mval readable =
    let
        fun prn_str Mal.Nil = "nil"
          | prn_str (Mal.String str) = if readable
                                       then "\"" ^ (String.toCString str) ^ "\""
                                       else str
          | prn_str (Mal.Symbol str) = str
          | prn_str (Mal.Keyword str) = str
          | prn_str (Mal.Int num) = prn_num (Int.toString num)
          | prn_str (Mal.Real num) = prn_num (Real.toString num)
          | prn_str (Mal.Bool bv) = if bv then "true" else "false"
          | prn_str (Mal.Atom (at, _)) = "(atom " ^ (prn_str (!at)) ^ ")"
          | prn_str (Mal.List (lst, _)) = "(" ^ (prn_lst lst) ^ ")"
          | prn_str (Mal.Vector (vec, _)) = "[" ^ (prn_vec vec 0) ^ "]"
          | prn_str (Mal.Hashmap (al, _)) = "{" ^ (prn_map (SDic.alist al)) ^ "}"
          | prn_str (Mal.Function (name, _, _)) = "#<function '" ^ name ^ "'>"
          | prn_str (Mal.Closure _) = "#<closure>"
          | prn_str (Mal.Macro _) = "#<macro>"

        and prn_lst [] = ""
          | prn_lst (hd::[]) = prn_str hd
          | prn_lst (hd::tl) = (prn_str hd) ^ " " ^ (prn_lst tl)

        and prn_vec vec i =
            let
                val len = Vector.length vec
                fun prn i = if i + 1 = len then prn_str (Vector.sub (vec, i))
                            else prn_str (Vector.sub (vec, i)) ^ " " ^ (prn (i + 1))
            in
                if len = 0 then "" else (prn 0)
            end

        and prn_map [] = ""
          | prn_map ((s,(k,v))::[]) = s ^ " " ^ (prn_str v)
          | prn_map ((s,(k,v))::tl) = s ^ " " ^ (prn_str v) ^ " " ^ (prn_map tl)

        and prn_num str =
            if size str > 0 andalso String.sub (str, 0) = #"~"
            then "-" ^ String.substring (str, 1, (size str) - 1)
            else str
    in
        prn_str mval
    end

end

(***** reader.sml *****)

structure Reader = struct

type reader = { line : string, col :int }

fun make_reader str col = { line = str, col = col }

fun is_white_space ch = ch <= #" " orelse ch = #","

fun skip_white_space str =
  let
      val len = size str

      fun skip i =
        if i >= len then len
        else if is_white_space (String.sub (str, i)) then skip (i + 1)
        else i
  in
      skip 0
  end

fun skip_comment str i =
  let
      val len = size str

      fun skip i cr =
        if i >= len then len
        else case String.sub (str, i) of
                 #"\n" => i + 1
               | #"\r" => if cr then i else skip (i + 1) true
               | _     => if cr then i else skip (i + 1) false
  in
      skip i false
  end

fun skip_string str i =
  let
      val len = size str

      fun skip i esc =
        if i >= len then len
        else if esc then skip (i + 1) false
        else case String.sub (str, i) of
                 #"\"" => i + 1
               | #"\\" => skip (i + 1) true
               | _     => skip (i + 1) false
  in
      skip i false
  end

fun skip_atom str i =
  let
      val len = size str

      fun skip i =
        if i >= len then len
        else let val ch = String.sub (str, i) in
                 if is_white_space ch then i
                 else if Char.contains "[]{}()'`\";" ch then i
                 else skip (i + 1)
             end
  in
      skip i
  end

fun read_token str =
  let val len = size str
      val wsl = skip_white_space str
  in
      if wsl = len then (NONE, len)
      else case String.sub(str, wsl) of
               #"~"  => if wsl + 1 < len andalso String.sub(str, wsl + 1) = #"@"
                        then (SOME "~@", wsl + 2)
                        else (SOME "~", wsl + 1)
             | #";"  => let val idx = skip_comment str wsl
                        in (SOME (String.substring(str, wsl, idx - wsl)), idx) end
             | #"\"" => let val idx = skip_string str (wsl + 1)
                        in (SOME (String.substring(str, wsl, idx - wsl)), idx) end
             | _     => if Char.contains "[]{}()'`^@" (String.sub (str, wsl))
                        then (SOME (String.substring(str, wsl, 1)), wsl + 1)
                        else let val idx = skip_atom str wsl
                             in (SOME (String.substring(str, wsl, idx - wsl)), idx) end
  end

fun next_token rdr =
  let
      val {line, col} = rdr
      val len = size line
  in
      if len = 0 then (NONE, make_reader "" col)
      else
          let
              val (tkn, idx) = read_token line
          in
              case tkn of
                  NONE => (NONE, make_reader "" (col + idx))
                | SOME str => (tkn, make_reader (String.substring (line, idx, len - idx)) (col + idx))
          end
  end

datatype Num = INT | REAL | NONUM

fun is_number str =
  let
      val len = size str

      fun dig3 i = if i >= len then REAL
                   else let val ch = String.sub(str, i) in
                            if Char.isDigit ch then dig3 (i + 1)
                            else NONUM
                        end

      fun sig2 i = if i >= len then NONUM
                   else let val ch = String.sub(str, i) in
                            if Char.isDigit ch then dig3 (i + 1)
                            else NONUM
                        end

      fun exp i = if i >= len then NONUM
                  else let val ch = String.sub(str, i) in
                           if Char.isDigit ch then dig3 (i + 1)
                           else if ch = #"+" orelse ch = #"-" then sig2 (i + 1)
                           else NONUM
                       end

      fun dig2 i = if i >= len then REAL
                   else let val ch = String.sub(str, i) in
                            if Char.isDigit ch then dig2 (i + 1)
                            else if ch = #"e" orelse ch = #"E" then exp (i + 1)
                            else NONUM
                        end

      fun dot1 i = if i >= len then REAL
                   else let val ch = String.sub(str, i) in
                            if Char.isDigit ch then dig2 (i + 1)
                            else if ch = #"e" orelse ch = #"E" then exp (i + 1)
                            else NONUM
                        end

      fun dig1 i = if i >= len then INT
                   else let val ch = String.sub(str, i) in
                            if Char.isDigit ch then dig1 (i + 1)
                            else if ch = #"." then dot1 (i + 1)
                            else if ch = #"e" orelse ch = #"E" then exp (i + 1)
                            else NONUM
                        end

      fun dot i = if i >= len then NONUM
                  else let val ch = String.sub(str, i) in
                           if Char.isDigit ch then dig2 (i + 1)
                           else NONUM
                       end

      fun sig1 i = if i >= len then NONUM
                   else let val ch = String.sub(str, i) in
                            if Char.isDigit ch then dig1 (i + 1)
                            else if ch = #"." then dot (i + 1)
                            else NONUM
                        end
  in
      if len = 0 then NONUM
      else let val ch = String.sub(str, 0) in
               if Char.isDigit ch then dig1 1
               else if ch = #"+" orelse ch = #"-" then sig1 1
               else if ch = #"." then dot 1
               else NONUM
           end
  end

fun get_string str =
  let
      val len = size str
      val chr = String.sub (str, len - 1)
      val sstr = if len < 2 orelse chr <> #"\""
                 then Core.error "unbalanced string"
                 else String.substring (str, 1, len - 2)
  in
      case String.fromCString sstr of
          NONE => Mal.String sstr
        | SOME estr => Mal.String estr
  end

fun get_atom str =
  case is_number str of
      INT => (case Int.fromString str of
                  NONE => Core.error "invalid number"
                | SOME num => Mal.Int num)
    | REAL => (case Real.fromString str of
                   NONE => Core.error "invalid number"
                 | SOME num => Mal.Real num)
    | NONUM => (case str of
                    "nil" => Mal.Nil
                  | "true" => Mal.Bool true
                  | "false" => Mal.Bool false
                  | _ => if String.isPrefix ":" str
                         then Mal.makeKeyword str
                         else Mal.makeSymbol str)

fun read_list rdr =
  let
      fun next rdr lst =
        let
            val (tkn, rdr2) = next_token rdr
        in
            case tkn of
                NONE => Core.error "unbalanced list"
              | SOME ")" => (SOME (Mal.List (rev lst, Mal.Nil)), rdr2)
              | SOME "]" => Core.error "unbalanced list"
              | SOME "}" => Core.error "unbalanced list"
              | _ => let
                        val (va, rdr2) = read_form rdr
                     in
                         case va of
                             NONE => (NONE, rdr2)
                           | SOME frm => next rdr2 (frm :: lst)
                     end
        end
  in
      next rdr []
  end

and read_vector rdr =
  let
      fun next rdr lst =
        let
            val (tkn, rdr2) = next_token rdr
        in
            case tkn of
                NONE => Core.error "unbalanced vector"
              | SOME "]" => (SOME (Mal.Vector (Vector.fromList (rev lst), Mal.Nil)), rdr2)
              | SOME ")" => Core.error "unbalanced vector"
              | SOME "}" => Core.error "unbalanced vector"
              | _ => let
                        val (va, rdr2) = read_form rdr
                     in
                         case va of
                             NONE => (NONE, rdr2)
                           | SOME frm => next rdr2 (frm :: lst)
                     end
        end
  in
      next rdr []
  end

and read_hashmap rdr =
  let
      fun next rdr avl key =
        let
            val (tkn, rdr2) = next_token rdr
        in
            case tkn of
                NONE => Core.error "unbalanced hashmap"
              | SOME "}" => if Option.isSome key
                            then Core.error "hashmap - even number of elements is required"
                            else (SOME (Mal.Hashmap (avl, Mal.Nil)), rdr2)
              | SOME ")" => Core.error "unbalanced hashmap"
              | SOME "]" => Core.error "unbalanced hashmap"
              | _ => let
                        val (frm, rdr2) = read_form rdr
                     in
                         case frm of
                             NONE => Core.error "unbalanced hashmap"
                           | SOME v => (case key of
                                            NONE => (case v of
                                                         Mal.String _ => next rdr2 avl frm
                                                       | Mal.Symbol _ => next rdr2 avl frm
                                                       | Mal.Keyword _ => next rdr2 avl frm
                                                       | Mal.Int _ => next rdr2 avl frm
                                                       | _ => Core.error "hashmap - only string/symbol/keyword/integer is expeted for key")
                                          | SOME k => next rdr2 (SDic.insert avl (Printer.pr_str k true, (k, v))) NONE)
                     end
        end
  in
      next rdr SDic.empty NONE
  end

and read_form rdr =
  let
      val (tkn, rdr2) = next_token rdr

      fun make_list2 x y = Mal.List ([x, y], Mal.Nil)

      fun make_list3 x y z = Mal.List ([x, y, z], Mal.Nil)

      fun get_form rdr = let val (frm, rdr2) = read_form rdr in
                             case frm of
                                 NONE => Core.error "form is expected"
                               | SOME form => (form, rdr2)
                         end

      fun form_expand sym rdr = let val (frm, rdr2) = get_form rdr in
                                    (SOME (make_list2 sym frm), rdr2)
                                end
  in
      case tkn of
          NONE => (NONE, rdr2)
        | SOME str => case String.sub(str, 0) of
                          #";"  => read_form rdr2
                        | #"("  => read_list rdr2
                        | #"["  => read_vector rdr2
                        | #"{"  => read_hashmap rdr2
                        | #"\"" => (SOME (get_string str), rdr2)
                        | #"'"  => form_expand Mal.Quote rdr2
                        | #"`"  => form_expand Mal.Quasiquote rdr2
                        | #"~"  => if String.isPrefix "~@" str
                                   then form_expand Mal.Splice_unquote rdr2
                                   else form_expand Mal.Unquote rdr2
                        | #"@"  => form_expand Mal.Deref rdr2
                        | #"^"  => let val (frm1, rdr3) = get_form rdr2
                                       val (frm2, rdr4) = get_form rdr3
                                   in
                                       (SOME (make_list3 Mal.With_meta frm2 frm1), rdr4)
                                   end
                        | _     => (SOME (get_atom str), rdr2)
  end

fun read_str str =
  let val (ast, _) = read_form (make_reader str 0)
  in ast
  end

end

(***** eval.sml *****)

structure Eval = struct

fun m_add arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x + y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x + y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x + (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) + y)
      | _ => Core.error "'+' invalid arguments"

fun m_sub arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x - y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x - y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x - (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) - y)
      | _ => Core.error "'-' invalid arguments"

fun m_mul arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x * y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x * y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x * (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) * y)
      | _ => Core.error "'*' invalid arguments"

fun m_div arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x div y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x / y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x / (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) / y)
      | _ => Core.error "'/' invalid arguments"

fun m_lt arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x < y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x < y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x < (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) < y)
      | _ => Core.error "'<' invalid arguments"

fun m_lte arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x <= y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x <= y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x <= (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) <= y)
      | _ => Core.error "'<' invalid arguments"

fun m_gt arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x > y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x > y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x > (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) > y)
      | _ => Core.error "'<' invalid arguments"

fun m_gte arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x >= y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x >= y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x >= (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) >= y)
      | _ => Core.error "'<' invalid arguments"

fun al_key (s, (k, d)) = s
fun al_val (s, (k, d)) = d

fun m_equal (Mal.Bool x,     Mal.Bool y)     = Mal.Bool (x = y)
  | m_equal (Mal.Int x,      Mal.Int y)      = Mal.Bool (x = y)
  | m_equal (Mal.Real x,     Mal.Real y)     = Mal.Bool (Real.== (x, y))
  | m_equal (Mal.String x,   Mal.String y)   = Mal.Bool (x = y)
  | m_equal (Mal.Symbol x,   Mal.Symbol y)   = Mal.Bool (x = y)
  | m_equal (Mal.Keyword x,  Mal.Keyword y)  = Mal.Bool (x = y)
  | m_equal (Mal.Nil,        Mal.Nil)        = Mal.Bool true
  | m_equal (Mal.Atom (x, _), Mal.Atom (y, _)) =
        m_equal (!x, !y)
  | m_equal (Mal.Atom (x, _), y) =
        m_equal (!x, y)
  | m_equal (x, Mal.Atom (y, _)) =
        m_equal (x, !y)
  | m_equal (Mal.List (lst1, _), Mal.List (lst2, _)) =
        let
            fun eql [] [] = true
              | eql [] _ = false
              | eql _ [] = false
              | eql (hd1::tl1) (hd2::tl2) = (case m_equal (hd1, hd2) of
                                                 Mal.Bool true => eql tl1 tl2
                                               | _ => false)
        in
            Mal.Bool (eql lst1 lst2)
        end
  | m_equal (Mal.Vector (v1, _), Mal.Vector (v2, _)) =
        let
            val len = Vector.length v1
            fun eql i = if i >= len then true
                        else (case m_equal (Vector.sub (v1, i), Vector.sub (v2, i)) of
                                  Mal.Bool true => eql (i + 1)
                                | _ => false)
        in
            if (Vector.length v2) <> len then Mal.Bool false
            else Mal.Bool (eql 0)
        end
  | m_equal (Mal.Vector (v1, _), Mal.List (lst2, _)) =
        let
            val len = Vector.length v1
            fun eql i lst = if i >= len then (null lst)
                            else if null lst then false
                            else (case m_equal (Vector.sub (v1, i), hd lst) of
                                      Mal.Bool true => eql (i + 1) (tl lst)
                                    | _ => false)
        in
            Mal.Bool (eql 0 lst2)
        end
  | m_equal (lst1 as (Mal.List _), v2 as (Mal.Vector _)) = m_equal (v2, lst1)
  | m_equal (Mal.Hashmap (hm1, _), Mal.Hashmap (hm2, _)) =
        let
            fun eql [] [] = true
              | eql [] _ = false
              | eql _ [] = false
              | eql (hd1::tl1) (hd2::tl2) = (al_key hd1) = (al_key hd2) andalso
                                            case m_equal ((al_val hd1), (al_val hd2)) of
                                                Mal.Bool true => eql tl1 tl2
                                              | _ => false
        in
            Mal.Bool (if (SDic.count hm1) <> (SDic.count hm2)
                      then false
                      else eql (SDic.alist hm1) (SDic.alist hm2))
        end
  | m_equal _ = Mal.Bool false

fun m_list args = Mal.List (args, Mal.Nil)

fun m_list_p (Mal.List _) = Mal.Bool true
  | m_list_p _ = Mal.Bool false

fun m_vector args = Mal.Vector (Vector.fromList args, Mal.Nil)

fun m_vector_p (Mal.Vector _) = Mal.Bool true
  | m_vector_p _ = Mal.Bool false

fun m_assoc ((Mal.Hashmap (hm, _)) :: args) =
        let fun fold (k :: v :: tl) hm = fold tl (case k of
                                                      Mal.String _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                    | Mal.Symbol _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                    | Mal.Keyword _ => SDic.insert hm (Printer.pr_str k true,  (k, v))
                                                    | Mal.Int _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                    | _ => Core.error "assoc - only string/symbol/keyword/integer is expeted for key")
              | fold [] hm = hm
              | fold _ _ = Core.error "assoc - even number of arguments are required"
        in
            Mal.Hashmap ((fold args hm), Mal.Nil)
        end
  | m_assoc _ = Core.error "assoc - hashmap is expected"

fun m_dissoc ((Mal.Hashmap (hm, _)) :: args) =
        Mal.Hashmap (foldl (fn (arg, hm) => (case arg of
                                                 Mal.String _ => SDic.remove hm (Printer.pr_str arg true)
                                               | Mal.Symbol _ => SDic.remove hm (Printer.pr_str arg true)
                                               | Mal.Keyword _ => SDic.remove hm (Printer.pr_str arg true)
                                               | Mal.Int _ => SDic.remove hm (Printer.pr_str arg true)
                                               | _ => Core.error "dissoc - only string/symbol/keyword/integer is expeted for key")) hm args,
                     Mal.Nil)
  | m_dissoc _ = Core.error "dissoc - hashmap is expected"

fun m_hashmap args =
    let fun fold (k :: v :: tl) hm = fold tl (case k of
                                                  Mal.String _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                | Mal.Symbol _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                | Mal.Keyword _ => SDic.insert hm (Printer.pr_str k true,  (k, v))
                                                | Mal.Int _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                | _ => Core.error "hash-map - only string/symbol/keyword/integer is expeted for key")
          | fold [] hm = hm
          | fold _ _ = Core.error "hash-map - even number of arguments are required"
    in
        Mal.Hashmap (fold args SDic.empty, Mal.Nil)
    end

fun m_map_p (Mal.Hashmap _) = Mal.Bool true
  | m_map_p _ = Mal.Bool false

fun m_get (Mal.Hashmap (hm, _)) key =
        let
            val value = case key of
                            Mal.String _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Symbol _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Keyword _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Int _ => SDic.lookup hm (Printer.pr_str key true)
                          | _ => Core.error "get - only string/symbol/keyword/integer is expeted for key"
        in
            case value of
                SOME (k,v) => v
              | NONE => Mal.Nil
        end
  | m_get Mal.Nil _ = Mal.Nil
  | m_get _ _ = Core.error "get - hashmap is expected"

fun m_contains_p (Mal.Hashmap (hm, _)) key =
        let
            val value = case key of
                            Mal.String _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Symbol _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Keyword _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Int _ => SDic.lookup hm (Printer.pr_str key true)
                          | _ => Core.error "contains - only string/symbol/keyword/integer is expeted for key"
        in
            case value of
                SOME _ => Mal.Bool true
              | NONE => Mal.Bool false
        end
  | m_contains_p _ _ = Core.error "contains - hashmap is expected"

fun m_keys (Mal.Hashmap (hm, _)) = Mal.List (map (#1 o #2) (SDic.alist hm), Mal.Nil)
  | m_keys _ = Core.error "keys - hashmap is expected"

fun m_vals (Mal.Hashmap (hm, _)) = Mal.List (map (#2 o #2) (SDic.alist hm), Mal.Nil)
  | m_vals _ = Core.error "vals - hashmap is expected"

fun m_empty_p (Mal.List (lst, _)) = Mal.Bool (null lst)
  | m_empty_p (Mal.Vector (vec, _)) = Mal.Bool ((Vector.length vec) = 0)
  | m_empty_p (Mal.Hashmap (hm, _)) = Mal.Bool (SDic.isEmpty hm)
  | m_empty_p Mal.Nil = Mal.Bool true
  | m_empty_p _ = Core.error "empty? - wrong type of argument"

fun m_sequential_p (Mal.List _) = Mal.Bool true
  | m_sequential_p (Mal.Vector _) = Mal.Bool true
  | m_sequential_p _ = Mal.Bool false

fun m_nil_p Mal.Nil = Mal.Bool true
  | m_nil_p _ = Mal.Bool false

fun m_true_p (Mal.Bool true) = Mal.Bool true
  | m_true_p _ = Mal.Bool false

fun m_false_p (Mal.Bool false) = Mal.Bool true
  | m_false_p _ = Mal.Bool false

fun m_symbol_p (Mal.Symbol _) = Mal.Bool true
  | m_symbol_p _ = Mal.Bool false

fun m_keyword_p (Mal.Keyword _) = Mal.Bool true
  | m_keyword_p _ = Mal.Bool false

fun m_string_p (Mal.String _) = Mal.Bool true
  | m_string_p _ = Mal.Bool false

fun m_number_p (Mal.Int _) = Mal.Bool true
  | m_number_p (Mal.Real _) = Mal.Bool true
  | m_number_p _ = Mal.Bool false

fun m_fn_p (Mal.Function _) = Mal.Bool true
  | m_fn_p (Mal.Closure _) = Mal.Bool true
  | m_fn_p _ = Mal.Bool false

fun m_macro_p (Mal.Macro _) = Mal.Bool true
  | m_macro_p _ = Mal.Bool false

fun m_symbol (Mal.String "") = Core.error "symbol - non empty string is required"
  | m_symbol (Mal.String ":") = Core.error "symbol - wrong name of symbol"
  | m_symbol (Mal.String str) =
        let
            val str' = if String.isPrefix ":" str then (String.extract (str, 1, NONE)) else str
            val str'' = String.translate (fn (ch) => if (Char.ord ch) <= 32 then "-" else (String.str ch)) str'
        in
            case Reader.is_number str'' of
                Reader.NONUM => Mal.makeSymbol str''
             | _ => Core.error "symbol - wrong name of symbol"
        end
  | m_symbol _ = Core.error "symbol - string is expected"

fun m_keyword (Mal.String "") = Core.error "keyword - non empty string is required"
  | m_keyword (Mal.String str) =
        let
            val str' = if String.isPrefix ":" str then str else (":" ^ str)
            val str'' = String.translate (fn (ch) => if (Char.ord ch) <= 32 then "-" else (String.str ch)) str'
        in
            Mal.makeKeyword str''
        end
  | m_keyword (kw as (Mal.Keyword _)) = kw
  | m_keyword _ = Core.error "keyword - string is expected"

fun m_count (Mal.List (lst, _)) = Mal.Int (length lst)
  | m_count (Mal.Vector (vec, _)) = Mal.Int (Vector.length vec)
  | m_count (Mal.Hashmap (hm, _)) = Mal.Int (SDic.count hm)
  | m_count Mal.Nil = Mal.Int 0
  | m_count _ = Core.error "count - invalid argument"

fun m_prn [] = (print "\n"; Mal.Nil)
  | m_prn [arg] = (print (Printer.pr_str arg true);
                   print "\n";
                   Mal.Nil)
  | m_prn (arg :: tl) = (print (Printer.pr_str arg true);
                         print " ";
                         m_prn tl)

fun m_println [] = (print "\n"; Mal.Nil)
  | m_println [arg] = (print (Printer.pr_str arg false);
                       print "\n";
                       Mal.Nil)
  | m_println (arg :: tl) = (print (Printer.pr_str arg false);
                             print " ";
                             m_println tl)

fun m_str [] = ""
  | m_str [arg] = Printer.pr_str arg false
  | m_str (arg :: tl) = (Printer.pr_str arg false) ^ (m_str tl)

fun m_pr_str [] = ""
  | m_pr_str [arg] = (Printer.pr_str arg true)
  | m_pr_str (arg :: tl) = (Printer.pr_str arg true) ^ " " ^ (m_pr_str tl)

fun m_read_string (Mal.String str) =
        (case Reader.read_str str of
             SOME exp => exp
           | NONE => Core.error "read-string - invalid expresion")
  | m_read_string _ = Core.error "read-string - string is required"

fun m_slurp (Mal.String filename) =
    let
        fun getContent inp lines =
            case TextIO.inputLine inp of
                SOME ln => getContent inp (ln :: lines)
              | NONE => rev lines

        fun readContent filename =
            let
                val inp = TextIO.openIn filename
                val lns = getContent inp []
            in
                TextIO.closeIn inp;
                String.concat lns
            end
            handle _ => Core.error ("slurp - unable read file: " ^ filename)
    in
        Mal.String (readContent filename)
    end
  | m_slurp _ = (Core.error "slurp - string is required")

fun m_cons any (Mal.List (lst, _)) = Mal.List (any :: lst, Mal.Nil)
  | m_cons any Mal.Nil = Mal.List ([any], Mal.Nil)
  | m_cons any (Mal.Vector (vec, _)) = Mal.List (any :: (Vector.foldr (op ::) [] vec), Mal.Nil)
  | m_cons _ _ = Core.error "cons - wrong arguments"

fun m_conj (Mal.List (ls, _) :: args) =
        Mal.List (foldl (op ::) ls args, Mal.Nil)
  | m_conj (Mal.Vector (vec, _) :: args) =
        Mal.Vector (Vector.fromList (Vector.foldr (op ::) args vec), Mal.Nil)
  | m_conj _ = Core.error "conj - list/vector is expected"

fun m_concat [] = Mal.List ([], Mal.Nil)
  | m_concat lst =
        Mal.List (foldr (fn (el, va) =>
                            (case el of
                                 Mal.Nil => va
                               | Mal.List ([], _) => va
                               | Mal.List (lst, _) => lst @ va
                               | Mal.Vector (vec, _) => Vector.foldr (op ::) va vec
                               | _ => Core.error "concat - wrong parameters"))
                        []
                        lst,
                  Mal.Nil)

fun m_nth (Mal.List (lst, _)) (Mal.Int i) =
        (List.nth (lst, i)
         handle Subscript => Core.error "nth - subscript is out of range")
  | m_nth (Mal.Vector (vec, _)) (Mal.Int i) =
        (Vector.sub (vec, i)
         handle Subscript => Core.error "nth - subscript is out of range")
  | m_nth _ (Mal.Int _) = Core.error "nth - vector/list is required"
  | m_nth _  _ = Core.error "nth - integer is required in second argument"

fun m_first Mal.Nil = Mal.Nil
  | m_first (Mal.List ([], _)) = Mal.Nil
  | m_first (Mal.List (lst, _)) = hd lst
  | m_first (Mal.Vector (vec, _)) = if (Vector.length vec) = 0 then Mal.Nil
                                    else Vector.sub (vec, 0)
  | m_first _ = Core.error "first - list/vector is expected"

fun m_rest Mal.Nil = Mal.List ([], Mal.Nil)
  | m_rest (Mal.List ([], _)) = Mal.List ([], Mal.Nil)
  | m_rest (Mal.List (lst, _)) = Mal.List (tl lst, Mal.Nil)
  | m_rest (Mal.Vector (vec, _)) = if (Vector.length vec) <= 1 then Mal.List ([], Mal.Nil)
                                   else Mal.List (tl (Vector.foldr (op ::) [] vec), Mal.Nil)
  | m_rest _ = Core.error "rest - list/vector is expected"

fun m_seq (Mal.List ([], _)) = Mal.Nil
  | m_seq (lst as Mal.List _) = lst
  | m_seq (Mal.Vector (vec, _)) = if (Vector.length vec) = 0 then Mal.Nil
                                  else Mal.List (Vector.foldr (op ::) [] vec, Mal.Nil)
  | m_seq (Mal.String "") = Mal.Nil
  | m_seq (Mal.String str) = Mal.List (map (Mal.String o String.str)
                                           (String.explode str),
                                       Mal.Nil)
  | m_seq Mal.Nil = Mal.Nil
  | m_seq _ = Core.error "seq - wrong argument"

fun m_atom value = Mal.Atom (ref value, Mal.Nil)

fun m_atom_p (Mal.Atom _) = Mal.Bool true
  | m_atom_p _ = Mal.Bool false

fun m_deref (Mal.Atom (at, _)) = !at
  | m_deref _ = Core.error "deref - atom is expected"

fun m_reset (Mal.Atom (at, _)) value = (at := value; value)
  | m_reset _ _ = Core.error "reset! - atom is expected"

fun m_with_meta (Mal.List (lst, _)) meta = Mal.List (lst, meta)
  | m_with_meta (Mal.Vector (vec, _)) meta = Mal.Vector (vec, meta)
  | m_with_meta (Mal.Hashmap (hm, _)) meta = Mal.Hashmap (hm, meta)
  | m_with_meta (Mal.Atom (at, _)) meta = let val v = !at in Mal.Atom (ref v, meta) end
  | m_with_meta (Mal.Function (name, ff, _)) meta = Mal.Function (name, ff, meta)
  | m_with_meta (Mal.Closure (vars, rest, exps, env, _)) meta = Mal.Closure (vars, rest, exps, env, meta)
  | m_with_meta (Mal.Macro (vars, rest, exps, env, _)) meta = Mal.Macro (vars, rest, exps, env, meta)
  | m_with_meta _ _ = Core.error "with-meta - invalid argument"

fun m_meta (Mal.List (_, meta)) = meta
  | m_meta (Mal.Vector (_, meta)) = meta
  | m_meta (Mal.Hashmap (_, meta)) = meta
  | m_meta (Mal.Atom (_, meta)) = meta
  | m_meta (Mal.Function (_, _, meta)) = meta
  | m_meta (Mal.Closure (_, _, _, _, meta)) = meta
  | m_meta (Mal.Macro (_, _, _, _, meta)) = meta
  | m_meta _ = Core.error "meta - invalid argument"

fun makeFun_1 ff [arg] = ff arg
  | makeFun_1 _ _ = Core.error "function requires one argument"

fun makeFun_2 ff [arg1, arg2] = ff arg1 arg2
  | makeFun_2 _ _ = Core.error "function requires one argument"

fun makeFun_1N ff arg lst =
    case lst of
        [] => Core.error "function requires at least one argument"
      | [elm] => ff (arg, elm)
      | (hd::tl) => foldl (fn (a1,a2) => ff (a2,a1)) hd tl


fun makeBFun_2N ff lst =
    let fun test a1 a2 [] = ff (a1, a2)
          | test a1 a2 (a3::tl) = case ff (a1, a2) of
                                      (Mal.Bool false) => Mal.Bool false
                                    | (Mal.Bool true) => test a2 a3 tl
                                    | _ =>  Mal.Bool false
    in
        case lst of
            [] => Core.error "function requires at least two arguments"
          | [elm] => Core.error "function requires at least two arguments"
          | (a1::a2::tl) => test a1 a2 tl
    end

fun eval ast env =
    case macroexpand ast env of
        Mal.List (Mal.Symbol "def!" :: args, _) =>
            (* def! *)
            (case args of
                 (Mal.Symbol name :: arg :: []) =>
                     let val value = eval arg env
                     in Env.set env name value;
                        value
                     end
               | (Mal.Symbol name :: []) =>
                     (Env.set env name Mal.Nil;
                      Mal.Nil)
               | _ => Core.error "def! - wrong syntax")

     | Mal.List (Mal.Symbol "let*" :: args, _) =>
           (* let* *)
           (case args of
                (Mal.List (defs, _) :: exps) =>
                    let val env' = Env.make env
                    in
                        eval_lst_env defs env';
                        foldl (fn (el,va) => eval el env') Mal.Nil exps
                    end
              | (Mal.Vector (defs, _) :: exps) =>
                    let val env' = Env.make env
                    in
                        eval_vec_env defs env';
                        foldl (fn (el,va) => eval el env') Mal.Nil exps
                    end
              | _ => Core.error "let* - wrong syntax")

     | Mal.List (Mal.Symbol "do" :: args, _) =>
           (* do *)
           foldl (fn (exp,va) => eval exp env) Mal.Nil args

     | Mal.List (Mal.Symbol "if" :: args, _) =>
           (* if *)
           (case args of
                (tst :: ex1 :: []) =>
                    (case eval tst env of
                         Mal.Nil => Mal.Nil
                       | Mal.Bool false => Mal.Nil
                       | _ => eval ex1 env)
              | (tst :: ex1 :: ex2 :: []) =>
                    (case eval tst env of
                         Mal.Nil => eval ex2 env
                       | Mal.Bool false => eval ex2 env
                       | _ => eval ex1 env)
              | _ => Core.error "if - wrong syntax")

     | Mal.List (Mal.Symbol "fn*" :: args, _) =>
           (* fn* *)
           (case args of
                Mal.List (args', _) :: exps =>
                    let val (vars, rest) = get_lst_args args'
                    in Mal.Closure (vars, rest, exps, env, Mal.Nil)
                    end
              | (Mal.Vector (args', _) :: exps) =>
                    let val (vars, rest) = get_lst_args (Vector.foldr (op ::) [] args')
                    in Mal.Closure (vars, rest, exps, env, Mal.Nil)
                    end
              | _ => Core.error "fn* - wrong syntax")

     | Mal.List (Mal.Symbol "quote" :: args, _) =>
           (* quote *)
           (case args of
                [arg] => arg
              | _ => Core.error "quote - wrong syntax")

     | Mal.List (Mal.Symbol "quasiquote" :: args, _) =>
           (* quasiquote *)
           (case args of
                [arg] => eval (quasiquote arg) env
              | _ => Core.error "quasiquote - wrong syntax")

     | Mal.List (Mal.Symbol "defmacro!" :: args, _) =>
           (* defmacro! *)
           (case args of
                [(Mal.Symbol sym), arg] =>
                    (case eval arg env of
                        Mal.Closure (vs, rv, ex, en, _) =>
                            let val mac = (Mal.Macro (vs, rv, ex, en, Mal.Nil))
                            in
                                Env.set env sym mac;
                                mac
                            end
                      | _ => Core.error "defmacro - closure is required")
              | _ => Core.error "defmacro - wrong syntax")

     | Mal.List (Mal.Symbol "macroexpand" :: args, _) =>
           (* macroexpand *)
           (case args of
                [arg] => macroexpand arg env
              | _ => Core.error "macroexpand - wrong syntax")

     | Mal.List (Mal.Symbol "try*" :: args, _) =>
           (* try*/catch* *)
           (case args of
                [arg, Mal.List ([Mal.Symbol "catch*", Mal.Symbol sym, exp], _)] =>
                    let
                        fun catch_any arg =
                            let val env' = Env.make env
                                val _ = Env.set env' sym arg
                            in
                                eval exp env'
                            end
                        fun catch msg = catch_any (Mal.String msg)
                    in
                        (eval arg env
                         handle Core.MalException msg => catch (msg)
                              | Mal.Throw arg => catch_any arg
                              | Subscript => catch "Exception - subscript is out range"
                              | Overflow => catch "Exception - overflow"
                              | Div => catch "Exception - division by zero"
                              | _ => catch "Exception")
                    end
              | [arg] => eval arg env
              | _ => Core.error "try*/catch* - wrong syntax")

     | ast' as Mal.List (_ :: _, _) =>
           (* function apply *)
           (case eval_ast ast' env of
                Mal.List (Mal.Function (_, f, _) :: args, _) => f args
              | Mal.List (Mal.Closure (vars, rest, exps, cenv, _) :: args, _) =>
                    let val env' = Env.make cenv
                    in eval_args vars rest args env env';
                       foldl (fn (exp,va) => eval exp env') Mal.Nil exps
                    end
              | _ => Core.error "function or closure is ecpected")
     | ast' =>
           eval_ast ast' env

and quasiquote (v as (Mal.Vector (vec, _))) =
        if (Vector.length vec) = 0 then v
        else quasiquote (Mal.List (Vector.foldr (op ::) [] vec, Mal.Nil))
  | quasiquote (Mal.List ((Mal.Symbol "unquote") :: arg :: [], _)) = arg
  | quasiquote (Mal.List (Mal.List ((Mal.Symbol "splice-unquote") :: arg :: [], _) :: rest, _)) =
        Mal.List (Mal.Concat :: arg :: [quasiquote (Mal.List (rest, Mal.Nil))], Mal.Nil)
  | quasiquote (Mal.List (arg :: rest, _)) =
        Mal.List (Mal.Cons :: (quasiquote arg) :: [quasiquote (Mal.List (rest, Mal.Nil))], Mal.Nil)
  | quasiquote any = Mal.List ([Mal.Quote, any], Mal.Nil)

and eval_lst_env [] env = ()
  | eval_lst_env (Mal.Symbol sym :: exp :: defs) env =
        (Env.set env sym (eval exp env);
         eval_lst_env defs env)
  | eval_lst_env _ _ = Core.error "let* - wrong symbols definition"

and eval_vec_env vec env =
    if Vector.length vec mod 2 <> 0 then
        Core.error "let* - wrong symbols definition"
    else
        Vector.foldli (fn (i, el, va) => if i mod 2 = 0 then
                                             case Vector.sub (vec, i) of
                                                 (Mal.Symbol symb) => symb
                                               | _ => Core.error "let* - wrong symbols definition"
                                         else
                                             ((Env.set env va (eval (Vector.sub (vec, i)) env)); ""))
                       "" vec

and get_lst_args args =
    let
        fun get_args [] true  args   = Core.error "fn* - invalid parameters"
          | get_args [] false args   = (rev args, NONE)
          | get_args [arg] true args = (case arg of
                                            Mal.Symbol sym => (rev args, SOME sym)
                                          | _ => Core.error "fn* - invalid parameters")
          | get_args (arg :: tl) true  args = Core.error "fn* - invalid parameters"
          | get_args (arg :: tl) false args = (case arg of
                                                   Mal.Symbol sym =>
                                                       if sym = "&"
                                                       then get_args tl true args
                                                       else get_args tl false (sym :: args)
                                                 | _ => Core.error "fn* - invalid parameters")
    in
        get_args args false []
    end

and eval_args [] NONE [] env cenv = ()
  | eval_args [] (SOME sym) [] env cenv =
        (Env.set cenv sym (Mal.List ([], Mal.Nil)); ())
  | eval_args [] NONE pars env cenv = Core.error "invalid number of closure arguments"
  | eval_args [] (SOME sym) pars env cenv =
        (Env.set cenv sym (Mal.List (pars, Mal.Nil)); ())
  | eval_args (arg :: args) rest (par :: pars) env cenv =
        (Env.set cenv arg par;
         eval_args args rest pars env cenv)
  | eval_args _ _ _ _ _ = Core.error "invalid number of closure arguments"

and macroexpand ast env =
    case ast of
        Mal.List ((Mal.Symbol sym) :: args, _) =>
            (case Env.find env sym of
                 Mal.Macro (vars, rest, exps, cenv, _) =>
                     let val env' = Env.make cenv
                         val _ = eval_args vars rest args env env'
                         val ast' = foldl (fn (exp,va) => eval exp env') Mal.Nil exps
                     in
                         macroexpand ast' env
                     end
               | _ => ast)
      | _ => ast

and eval_ast ast env =
    case ast of
        Mal.Symbol name => Env.get env name
      | Mal.List (lst, _) =>
        Mal.List (map (fn exp => eval exp env) lst, Mal.Nil)
      | Mal.Vector (vec, _) =>
        Mal.Vector (Vector.map (fn exp => eval exp env) vec, Mal.Nil)
      | Mal.Hashmap (avl, _) =>
        Mal.Hashmap (SDic.fold (fn (k, (k1, v1), m) =>
                                   SDic.insert m (k, (k1, (eval v1 env))))
                               SDic.empty avl,
                    Mal.Nil)
      | v => v

fun apply (Mal.Closure (vars, rest, exps, env, _)) args =
        let val env' = Env.make env
        in
            eval_args vars rest args env env';
            foldl (fn (ex, va) => eval ex env') Mal.Nil exps
        end
  | apply (Mal.Function (_, f, _)) args = f args
  | apply _ _ = Core.error "apply - wrong parameters"

fun m_swap ((Mal.Atom (at, _)) :: (Mal.Function (_, f, _)) :: args) =
        let val v1 = !at
            val v2 = f (v1 :: args)
        in
            at := v2; v2
        end
  | m_swap ((Mal.Atom (at, _)) :: (cl as (Mal.Closure _)) :: args) =
        let val v1 = !at
            val v2 = apply cl (v1 :: args)
        in
            at := v2; v2
        end
  | m_swap _ = Core.error "swap! - wrong parameters"

fun m_eval env ast = eval ast env

fun m_apply (ff :: args) =
    if null args then apply ff args
    else (case List.last args of
              Mal.List (lst, _) =>
                  let val len = length args
                      val args' = List.revAppend ((tl (rev args)), lst)
                  in
                      apply ff args'
                  end
            | Mal.Vector (vec, _) =>
                  let
                      val lst = Vector.foldr (op ::) [] vec
                      val args' = List.revAppend ((tl (rev args)), lst)
                  in
                      apply ff args'
                  end
            | _ => apply ff args)
  | m_apply _ = Core.error "apply - wrong arguments"

fun m_map [ff, Mal.List (lst, _)] =
        Mal.List (map (fn (el) => apply ff [el]) lst, Mal.Nil)
  | m_map [ff, Mal.Vector (vec, Mal.Nil)] =
        Mal.List (rev (Vector.foldl (fn (el,va) => (apply ff [el]) :: va) [] vec), Mal.Nil)
  | m_map [ff, Mal.Nil] = Mal.List ([], Mal.Nil)
  | m_map _ = Core.error "map - wrong arguments"

fun m_throw arg = raise Mal.Throw arg

fun m_readline (Mal.String str) =
    (print str;
     case TextIO.inputLine TextIO.stdIn of
         SOME line => Mal.String (String.translate (fn (ch) => case ch of
                                                                   #"\r" => ""
                                                                 | #"\n" => ""
                                                                 | _ => String.str ch)
                                                   line)
       | NONE => Mal.Nil)
  | m_readline _ = Core.error "readline - string is expected"

fun m_time_ms [] =
    let
        val tm = Time.toMilliseconds (Time.now ())
        val ms = tm mod 864000000
    in
        Mal.Int (Int.fromLarge ms)
    end
  | m_time_ms _ = Core.error "time_ms = no arguments are required"

fun makeEnv () =
    let
        val env = ref Mal.None
    in
        Env.set env "+" (Mal.Function ("+", makeFun_1N m_add (Mal.Int 0), Mal.Nil));
        Env.set env "-" (Mal.Function ("-", makeFun_1N m_sub (Mal.Int 0), Mal.Nil));
        Env.set env "*" (Mal.Function ("*", makeFun_1N m_mul (Mal.Int 1), Mal.Nil));
        Env.set env "/" (Mal.Function ("/", makeFun_1N m_div (Mal.Int 1), Mal.Nil));

        Env.set env "<"  (Mal.Function ("<",  makeBFun_2N m_lt,  Mal.Nil));
        Env.set env "<=" (Mal.Function ("<=", makeBFun_2N m_lte, Mal.Nil));
        Env.set env ">"  (Mal.Function (">",  makeBFun_2N m_gt,  Mal.Nil));
        Env.set env ">=" (Mal.Function (">=", makeBFun_2N m_gte, Mal.Nil));

        Env.set env "="  (Mal.Function ("=",  makeBFun_2N m_equal, Mal.Nil));

        Env.set env "list"      (Mal.Function ("list",      m_list,                 Mal.Nil));
        Env.set env "list?"     (Mal.Function ("list?",     makeFun_1 m_list_p,     Mal.Nil));
        Env.set env "vector"    (Mal.Function ("vector",    m_vector,               Mal.Nil));
        Env.set env "vector?"   (Mal.Function ("vector?",   makeFun_1 m_vector_p,   Mal.Nil));
        Env.set env "hash-map"  (Mal.Function ("hash-map",  m_hashmap,              Mal.Nil));
        Env.set env "map?"      (Mal.Function ("vector?",   makeFun_1 m_map_p,      Mal.Nil));
        Env.set env "assoc"     (Mal.Function ("assoc",     m_assoc,                Mal.Nil));
        Env.set env "dissoc"    (Mal.Function ("dissoc",    m_dissoc,               Mal.Nil));
        Env.set env "get"       (Mal.Function ("get",       makeFun_2 m_get,        Mal.Nil));
        Env.set env "contains?" (Mal.Function ("contains?", makeFun_2 m_contains_p, Mal.Nil));
        Env.set env "keys"      (Mal.Function ("keys",      makeFun_1 m_keys,       Mal.Nil));
        Env.set env "vals"      (Mal.Function ("vals",      makeFun_1 m_vals,       Mal.Nil));

        Env.set env "empty?"      (Mal.Function ("empty?",      makeFun_1 m_empty_p,      Mal.Nil));
        Env.set env "sequential?" (Mal.Function ("sequential?", makeFun_1 m_sequential_p, Mal.Nil));

        Env.set env "nil?"     (Mal.Function ("nil?",     makeFun_1 m_nil_p,     Mal.Nil));
        Env.set env "true?"    (Mal.Function ("true?",    makeFun_1 m_true_p,    Mal.Nil));
        Env.set env "false?"   (Mal.Function ("false?",   makeFun_1 m_false_p,   Mal.Nil));
        Env.set env "symbol?"  (Mal.Function ("symbol?",  makeFun_1 m_symbol_p,  Mal.Nil));
        Env.set env "keyword?" (Mal.Function ("keyword?", makeFun_1 m_keyword_p, Mal.Nil));
        Env.set env "string?"  (Mal.Function ("string?",  makeFun_1 m_string_p,  Mal.Nil));
        Env.set env "number?"  (Mal.Function ("number?",  makeFun_1 m_number_p,  Mal.Nil));
        Env.set env "fn?"      (Mal.Function ("fn?",      makeFun_1 m_fn_p,      Mal.Nil));
        Env.set env "macro?"   (Mal.Function ("macro?",   makeFun_1 m_macro_p,   Mal.Nil));

        Env.set env "symbol"  (Mal.Function ("symbol",  makeFun_1 m_symbol,  Mal.Nil));
        Env.set env "keyword" (Mal.Function ("keyword", makeFun_1 m_keyword, Mal.Nil));

        Env.set env "count"  (Mal.Function ("count",  makeFun_1 m_count, Mal.Nil));

        Env.set env "cons"   (Mal.Function ("cons",   makeFun_2 m_cons,  Mal.Nil));
        Env.set env "conj"   (Mal.Function ("conj",   m_conj,            Mal.Nil));
        Env.set env "concat" (Mal.Function ("concat", m_concat,          Mal.Nil));

        Env.set env "nth"    (Mal.Function ("nth",    makeFun_2 m_nth,   Mal.Nil));
        Env.set env "first"  (Mal.Function ("first",  makeFun_1 m_first, Mal.Nil));
        Env.set env "rest"   (Mal.Function ("rest",   makeFun_1 m_rest,  Mal.Nil));
        Env.set env "seq"    (Mal.Function ("seq",    makeFun_1 m_seq,   Mal.Nil));

        Env.set env "atom"   (Mal.Function ("atom",   makeFun_1 m_atom,   Mal.Nil));
        Env.set env "atom?"  (Mal.Function ("atom?",  makeFun_1 m_atom_p, Mal.Nil));
        Env.set env "deref"  (Mal.Function ("deref",  makeFun_1 m_deref,  Mal.Nil));
        Env.set env "reset!" (Mal.Function ("reset!", makeFun_2 m_reset,  Mal.Nil));
        Env.set env "swap!"  (Mal.Function ("swap!",  m_swap,             Mal.Nil));

        Env.set env "with-meta" (Mal.Function ("with-meta", makeFun_2 m_with_meta, Mal.Nil));
        Env.set env "meta"      (Mal.Function ("meta",      makeFun_1 m_meta,     Mal.Nil));

        Env.set env "prn"     (Mal.Function ("prn",     m_prn,                 Mal.Nil));
        Env.set env "println" (Mal.Function ("println", m_println,             Mal.Nil));
        Env.set env "str"     (Mal.Function ("str",     Mal.String o m_str,    Mal.Nil));
        Env.set env "pr-str"  (Mal.Function ("pr-str",  Mal.String o m_pr_str, Mal.Nil));

        Env.set env "read-string" (Mal.Function ("read-string", makeFun_1 m_read_string, Mal.Nil));
        Env.set env "slurp"       (Mal.Function ("slurp",       makeFun_1 m_slurp,       Mal.Nil));

        Env.set env "eval"  (Mal.Function ("eval",  makeFun_1 (m_eval env), Mal.Nil));
        Env.set env "apply" (Mal.Function ("apply", m_apply,                Mal.Nil));
        Env.set env "map"   (Mal.Function ("map",   m_map,                  Mal.Nil));
        Env.set env "throw" (Mal.Function ("throw", makeFun_1 m_throw,      Mal.Nil));

        Env.set env "readline" (Mal.Function ("readline", makeFun_1 m_readline, Mal.Nil));
        Env.set env "time-ms"  (Mal.Function ("time-ms",  m_time_ms,            Mal.Nil));

        Env.set env "*host-language*" (Mal.String "SML");

        env
    end

end

(***** step4 *****)

fun READ str = Reader.read_str str

fun EVAL ast env =
    case ast of
        NONE => NONE
      | SOME exp => SOME (Eval.eval exp env)

fun PRINT ast = case ast of NONE => NONE
                          | SOME mval => SOME (Printer.pr_str mval true)

fun rep str env = PRINT (EVAL (READ str) env)

fun repl env = (print "user> ";
                case TextIO.inputLine TextIO.stdIn of
                    SOME line => (case (rep line env) of
                                      NONE => ()
                                    | SOME str => print (str ^ "\n");
                                  repl env)
                  | NONE => ())
               handle Core.MalException msg => (print ("ERROR: " ^ msg ^ "\n"); repl env)
                    | Mal.Throw arg => (print ("Exception: " ^ (Printer.pr_str arg true) ^ "\n"); repl env)
                    | Subscript => (print "Exception: subscript is out range\n"; repl env)
                    | Overflow => (print "Exception: overflow\n"; repl env)
                    | Div => (print "Exception: division by zero\n"; repl env)
                    | _ => (print "Exception\n"; repl env)


fun main() =
    let
        val env = Eval.makeEnv ()
    in
        rep "(def! not (fn* (a) (if a false true)))" env;
        rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \" nil)\")))))" env;
        rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" env;

        case CommandLine.arguments () of
            (file :: argv) => (Env.set env "*ARGV*" (Mal.List (map Mal.String argv, Mal.Nil));
                               rep ("(load-file \"" ^ file ^ "\")") env)
          | _ => (Env.set env "*ARGV*" (Mal.List ([], Mal.Nil));
                  rep "(println (str \"Mal [\" *host-language* \"]\"))" env;
                  repl env;
                  print "\n";
                  OS.Process.exit (OS.Process.success));
      ()
  end

(* Uncoment this for mlton * )
val _ = main ()
( **)

(* Uncoment this for smlnj * )
structure App = struct

fun Main (appname, args) = (main(); OS.Process.success)

end
( **)