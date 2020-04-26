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
               | Symbol of (int * string)
               | Keyword of (int * string)
               | Int of int
               | Real of real
               | Bool of bool
               | List of Value list
               | Vector of Value vector
               | Hashmap of (Value * Value) SDic.Table
               | Closure of (int * string list * string option * Value list * EnvType ref)
               | Function of (string * (Value list -> Value))

and EnvType = None
            | Env of {data: Value SDic.Table, outer: EnvType}

val Symbols = ref SDic.empty

val Obj_ID = ref 0

fun nextID () = let val id = 1 + !Obj_ID in Obj_ID := id; id end

fun makeKeyword name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val sym = Keyword (nextID (), name)
                    in Symbols := SDic.insert syms (name, sym);
                       sym
                    end
          | SOME sym => sym
    end

fun makeSymbol name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val sym = Symbol (nextID (), name)
                    in Symbols := SDic.insert syms (name, sym);
                       sym
                    end
          | SOME sym => sym
    end

fun symbolID (Symbol (id,_)) = id
  | symbolID _ = ~1

val Quote = makeSymbol "quote"
val Quasiquote = makeSymbol "quasiquote"
val Unquote = makeSymbol "unquote"
val Splice_unquote = makeSymbol "splice-unquote"
val Deref = makeSymbol "deref"
val With_meta = makeSymbol "with-meta"

val Def_E = symbolID (makeSymbol "def!")
val Let_S = symbolID (makeSymbol "let*")
val Do    = symbolID (makeSymbol "do")
val If    = symbolID (makeSymbol "if")
val Fn_S  = symbolID (makeSymbol "fn*")

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
        fun get' Mal.None key = Core.error ("symbol '" ^ key ^ "' not found")
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
                                       then "\"" ^ (String.toString str) ^ "\""
                                       else str
          | prn_str (Mal.Symbol (_, str)) = str
          | prn_str (Mal.Keyword (_, str)) = str
          | prn_str (Mal.Int num) = prn_num (Int.toString num)
          | prn_str (Mal.Real num) = prn_num (Real.toString num)
          | prn_str (Mal.Bool bv) = if bv then "true" else "false"
          | prn_str (Mal.List lst) = "(" ^ (prn_lst lst) ^ ")"
          | prn_str (Mal.Vector vec) = "[" ^ (prn_vec vec 0) ^ "]"
          | prn_str (Mal.Hashmap al) = "{" ^ (prn_map (SDic.alist al)) ^ "}"
          | prn_str (Mal.Closure cls) = "<closure> " ^ (Int.toString (#1 cls)) ^ ">"
          | prn_str (Mal.Function (name, _)) = "<function '" ^ name ^ "'>"

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
      case String.fromString sstr of
          NONE => Core.error "illegal escape sequence"
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
              | SOME ")" => (SOME (Mal.List (rev lst)), rdr2)
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
              | SOME "]" => (SOME (Mal.Vector (Vector.fromList (rev lst))), rdr2)
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
                            else (SOME (Mal.Hashmap avl), rdr2)
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

      fun make_list2 x y = Mal.List [x, y]

      fun make_list3 x y z = Mal.List [x, y, z]

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
  let
      val rdr = make_reader str 0
      val (ast, rdr2) = read_form rdr
  in
      case ast of
          NONE => NONE
        | SOME va => SOME va
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

fun m_equal (Mal.Bool x,     Mal.Bool y)     = Mal.Bool (x = y)
  | m_equal (Mal.Int x,      Mal.Int y)      = Mal.Bool (x = y)
  | m_equal (Mal.Real x,     Mal.Real y)     = Mal.Bool (Real.== (x, y))
  | m_equal (Mal.String x,   Mal.String y)   = Mal.Bool (x = y)
  | m_equal (Mal.Symbol x,   Mal.Symbol y)   = Mal.Bool (x = y)
  | m_equal (Mal.Keyword x,  Mal.Keyword y)  = Mal.Bool (x = y)
  | m_equal (Mal.Nil,        Mal.Nil)        = Mal.Bool true
  | m_equal (Mal.List lst1, Mal.List lst2) =
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
  | m_equal (Mal.Vector v1, Mal.Vector v2) =
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
  | m_equal (Mal.Vector v1, Mal.List lst2) =
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
  | m_equal (Mal.Hashmap hm1, Mal.Hashmap hm2) =
        let
            fun eql [] [] = true
              | eql [] _ = false
              | eql _ [] = false
              | eql (hd1::tl1) (hd2::tl2) = (#1 hd1) = (#1 hd2) andalso
                                            case m_equal (#2(#2 hd1), #2(#2 hd2)) of
                                                Mal.Bool true => eql tl1 tl2
                                              | _ => false
        in
            Mal.Bool (if (SDic.count hm1) <> (SDic.count hm2)
                      then false
                      else eql (SDic.alist hm1) (SDic.alist hm2))
        end
  | m_equal _ = Mal.Bool false

fun m_list_p (Mal.List _) = Mal.Bool true
  | m_list_p _ = Mal.Bool false

fun m_empty_p (Mal.List lst) = Mal.Bool (null lst)
  | m_empty_p (Mal.Vector vec) = Mal.Bool ((Vector.length vec) = 0)
  | m_empty_p (Mal.Hashmap hm) = Mal.Bool (SDic.isEmpty hm)
  | m_empty_p Mal.Nil = Mal.Bool true
  | m_empty_p _ = Core.error "empty? - wrong type of argument"

fun m_count (Mal.List lst) = Mal.Int (length lst)
  | m_count (Mal.Vector vec) = Mal.Int (Vector.length vec)
  | m_count (Mal.Hashmap hm) = Mal.Int (SDic.count hm)
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

fun makeFun_1 ff [arg] = ff arg
  | makeFun_1 _ _ = Core.error "function requires one argument"

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

fun makeEnv () =
    let
        val env = ref Mal.None
    in
        Env.set env "+" (Mal.Function ("+", makeFun_1N m_add (Mal.Int 0)));
        Env.set env "-" (Mal.Function ("-", makeFun_1N m_sub (Mal.Int 0)));
        Env.set env "*" (Mal.Function ("*", makeFun_1N m_mul (Mal.Int 1)));
        Env.set env "/" (Mal.Function ("/", makeFun_1N m_div (Mal.Int 1)));

        Env.set env "<"  (Mal.Function ("<",  makeBFun_2N m_lt));
        Env.set env "<=" (Mal.Function ("<=", makeBFun_2N m_lte));
        Env.set env ">"  (Mal.Function (">",  makeBFun_2N m_gt));
        Env.set env ">=" (Mal.Function (">=", makeBFun_2N m_gte));

        Env.set env "="  (Mal.Function ("=",  makeBFun_2N m_equal));

        Env.set env "list"   (Mal.Function ("list",   Mal.List));
        Env.set env "list?"  (Mal.Function ("list?",  makeFun_1 m_list_p));
        Env.set env "empty?" (Mal.Function ("empty?", makeFun_1 m_empty_p));
        Env.set env "count"  (Mal.Function ("count",  makeFun_1 m_count));

        Env.set env "prn"     (Mal.Function ("prn",     m_prn));
        Env.set env "println" (Mal.Function ("println", m_println));
        Env.set env "str"     (Mal.Function ("prn",     Mal.String o m_str));
        Env.set env "pr-str"  (Mal.Function ("pr-str",  Mal.String o m_pr_str));

        (* SDic.fold (fn (k,v,va) => SDic.insert va (k, v)) e4 syms *)
        env
    end

fun eval ast env =
    let val exp = case ast of
                      Mal.List (Mal.Symbol (id, _) :: args) =>
                          if id = Mal.Def_E then
                              (* def! *)
                              case args of
                                  (Mal.Symbol (_, name) :: arg :: []) =>
                                      let val value = eval arg env
                                      in Env.set env name value;
                                          SOME value
                                      end
                                | (Mal.Symbol (_, name) :: []) =>
                                      (Env.set env name Mal.Nil;
                                       SOME Mal.Nil)
                                | _ => Core.error "def! - wrong syntax"

                          else if id = Mal.Let_S then
                              (* let* *)
                              case args of
                                  (Mal.List defs :: exps) =>
                                      let val env' = Env.make env
                                      in
                                          eval_lst_env defs env';
                                          SOME (foldl (fn (el,va) => eval el env') Mal.Nil exps)
                                      end
                                | (Mal.Vector defs :: exps) =>
                                   let val env' = Env.make env
                                   in
                                       eval_vec_env defs env';
                                       SOME (foldl (fn (el,va) => eval el env') Mal.Nil exps)
                                   end

                                | _ => Core.error "let* - wrong syntax"

                          else if id = Mal.Do then
                              (* do *)
                              SOME (foldl (fn (exp,va) => eval exp env) Mal.Nil args)

                          else if id = Mal.If then
                              (* if *)
                              case args of
                                  (tst :: ex1 :: []) =>
                                      (case eval tst env of
                                           Mal.Nil => SOME Mal.Nil
                                         | Mal.Bool false => SOME Mal.Nil
                                         | _ => SOME (eval ex1 env))
                               | (tst :: ex1 :: ex2 :: []) =>
                                     (case eval tst env of
                                          Mal.Nil => SOME (eval ex2 env)
                                        | Mal.Bool false => SOME (eval ex2 env)
                                        | _ => SOME (eval ex1 env))
                               | _ => Core.error "if - wrong syntax"

                          else if id = Mal.Fn_S then
                              (* fn* *)
                              case args of
                                  (Mal.List args' :: exps) =>
                                      let val (vars, rest) = get_lst_args args'
                                      in SOME (Mal.Closure (Mal.nextID (), vars, rest, exps, env))
                                      end
                                | (Mal.Vector args' :: exps) =>
                                  let val (vars, rest) = get_lst_args (Vector.foldr (op ::) [] args')
                                      in SOME (Mal.Closure (Mal.nextID (), vars, rest, exps, env))
                                      end
                                | _ => Core.error "fn* - wrong syntax"

                          else NONE
                    | _ => NONE
    in
        case exp of
            SOME exp => exp
          | NONE =>
            (case ast of
                 Mal.List (_ :: _) =>
                     (case eval_ast ast env of
                          Mal.List (Mal.Function (_, f) :: args) => f args
                        | Mal.List (Mal.Closure (_, vars, rest, exps, cenv) :: args) =>
                              let val env' = Env.make cenv
                              in eval_args vars rest args env env';
                                 foldl (fn (exp,va) => eval exp env') Mal.Nil exps
                              end
                        | _ => Core.error "function or closure is ecpected")
               | _ => eval_ast ast env)
    end

and eval_lst_env [] env = ()
  | eval_lst_env (Mal.Symbol (_,sym) :: exp :: defs) env =
        (Env.set env sym (eval exp env);
         eval_lst_env defs env)
  | eval_lst_env _ _ = Core.error "let* - wrong symbols definition"

and eval_vec_env vec env =
    if Vector.length vec mod 2 <> 0 then
        Core.error "let* - wrong symbols definition"
    else
        Vector.foldli (fn (i, el, va) => if i mod 2 = 0 then
                                             case Vector.sub (vec, i) of
                                                 (Mal.Symbol (_, symb)) => symb
                                               | _ => Core.error "let* - wrong symbols definition"
                                         else
                                             ((Env.set env va (eval (Vector.sub (vec, i)) env)); ""))
                       "" vec

and get_lst_args args =
    let
        fun get_args [] true  args   = Core.error "fn* - invalid parameters"
          | get_args [] false args   = (rev args, NONE)
          | get_args [arg] true args = (case arg of
                                            Mal.Symbol (_, sym) => (rev args, SOME sym)
                                          | _ => Core.error "fn* - invalid parameters")
          | get_args (arg :: tl) true  args = Core.error "fn* - invalid parameters"
          | get_args (arg :: tl) false args = (case arg of
                                                   Mal.Symbol (_, sym) =>
                                                       if sym = "&"
                                                       then get_args tl true args
                                                       else get_args tl false (sym :: args)
                                                 | _ => Core.error "fn* - invalid parameters")
    in
        get_args args false []
    end

and eval_args [] NONE [] env cenv = ()
  | eval_args [] (SOME sym) [] env cenv =
        (Env.set cenv sym (Mal.List []); ())
  | eval_args [] NONE pars env cenv = Core.error "invalid number of closure arguments"
  | eval_args [] (SOME sym) pars env cenv =
        (Env.set cenv sym (Mal.List pars); ())
  | eval_args (arg :: args) rest (par :: pars) env cenv =
        (Env.set cenv arg par;
         eval_args args rest pars env cenv)
  | eval_args _ _ _ _ _ = Core.error "invalid number of closure arguments"

and eval_ast ast env =
    case ast of
        Mal.Symbol (_, name) => Env.get env name
      | Mal.List lst =>
        Mal.List (map (fn exp => eval exp env) lst)
      | Mal.Vector vec =>
        Mal.Vector (Vector.map (fn exp => eval exp env) vec)
      | Mal.Hashmap avl =>
        Mal.Hashmap (SDic.fold (fn (k, (k1, v1), m) =>
                                   SDic.insert m (k, (k1, (eval v1 env))))
                               SDic.empty avl)
      | v => v

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

fun main() =
  let
      val env = Eval.makeEnv ()
  in
      rep "(def! not (fn* (a) (if a false true)))" env;

      repl env;
      print "\n";
      OS.Process.exit (OS.Process.success);
      ()
  end
