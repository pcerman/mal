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
               | Function_1N of (string * (Value list -> Value))


val Symbols = ref SDic.empty
val SymbolID = ref 0

fun makeKeyword name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val id = !SymbolID + 1
                        val sym = Keyword (id, name)
                    in
                        SymbolID := id;
                        Symbols := SDic.insert syms (name, sym);
                        sym
                    end
          | SOME sym => sym
    end

fun makeSymbol name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val id = !SymbolID + 1
                        val sym = Symbol (id, name)
                    in
                        SymbolID := id;
                        Symbols := SDic.insert syms (name, sym);
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

end

(***** env.sml *****)

structure Env = struct

datatype EnvType = None
                 | Env of {data: Mal.Value SDic.Table, outer: EnvType}

fun make out = let val out' = !out
                   val env = Env { data = SDic.empty, outer = out' }
               in
                   ref env
               end

fun set env key value =
    let
        fun set' None key value =
                 Env {data = SDic.insert SDic.empty (key, value), outer = None}
          | set' (Env {data = dt, outer = ot}) key value =
                 Env {data = SDic.insert dt (key, value), outer = ot}

        val env' = !env
    in
        env := set' env' key value
    end

fun get env key =
    let
        fun get' None key = Core.error ("symbol '" ^ key ^ "' not found")
          | get' (Env {data = dt, outer = ot}) key = (case SDic.lookup dt key of
                                                          NONE => get' ot key
                                                        | SOME v => v)

        val env' = !env
    in
        get' env' key
    end

fun find env key =
    let
        fun find' None _ = Mal.Nil
          | find' (Env {data = dt, outer = ot}) key = (case SDic.lookup dt key of
                                                           NONE => find' ot key
                                                         | SOME v => v)

        val env' = !env
    in
        find' env' key
    end

end

(***** printer.sml *****)

structure Printer = struct

fun prn_str mval =
  let
      fun prn_lst [] = ""
        | prn_lst (hd::[]) = prn_str hd
        | prn_lst (hd::tl) = (prn_str hd) ^ " " ^ (prn_lst tl)

      fun prn_vec vec i =
        let
            val len = Vector.length vec
            fun prn i = if i + 1 = len then prn_str (Vector.sub (vec, i))
                        else prn_str (Vector.sub (vec, i)) ^ " " ^ (prn (i + 1))
        in
            if len = 0 then "" else (prn 0)
        end

      fun prn_map [] = ""
        | prn_map ((s,(k,v))::[]) = s ^ " " ^ (prn_str v)
        | prn_map ((s,(k,v))::tl) = s ^ " " ^ (prn_str v) ^ " " ^ (prn_map tl)

      fun prn_num str =
        if size str > 0 andalso String.sub (str, 0) = #"~"
        then "-" ^ String.substring (str, 1, (size str) - 1)
        else str
  in
      case mval of
          Mal.Nil => "nil"
        | Mal.String str => "\"" ^ (String.toString str) ^ "\""
        | Mal.Symbol (_, str) => str
        | Mal.Keyword (_, str) => str
        | Mal.Int num => prn_num (Int.toString num)
        | Mal.Real num => prn_num (Real.toString num)
        | Mal.Bool bv => if bv then "true" else "false"
        | Mal.List lst => "(" ^ (prn_lst lst) ^ ")"
        | Mal.Vector vec => "[" ^ (prn_vec vec 0) ^ "]"
        | Mal.Hashmap al => "{" ^ (prn_map (SDic.alist al)) ^ "}"
        | Mal.Function_1N (name, _) => "<function " ^ name ^ ">"
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
                                          | SOME k => next rdr2 (SDic.insert avl (Printer.prn_str k, (k, v))) NONE)
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

fun madd arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x + y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x + y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x + (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) + y)
      | _ => Core.error "'+' invalid arguments"

fun msub arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x - y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x - y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x - (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) - y)
      | _ => Core.error "'-' invalid arguments"

fun mmul arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x * y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x * y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x * (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) * y)
      | _ => Core.error "'*' invalid arguments"

fun mdiv arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x div y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x / y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x / (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) / y)
      | _ => Core.error "'/' invalid arguments"

fun makeFun_1N ff arg lst =
    case lst of
        [] => Core.error "function requires at least one argument"
      | [elm] => ff (arg, elm)
      | (hd::tl) => List.foldl (fn (a1,a2) => ff (a2,a1)) hd tl

fun makeEnv () =
    let
        val env = ref Env.None
    in
        Env.set env "+" (Mal.Function_1N ("+", makeFun_1N madd (Mal.Int 0)));
        Env.set env "-" (Mal.Function_1N ("-", makeFun_1N msub (Mal.Int 0)));
        Env.set env "*" (Mal.Function_1N ("*", makeFun_1N mmul (Mal.Int 1)));
        Env.set env "/" (Mal.Function_1N ("/", makeFun_1N mdiv (Mal.Int 1)));

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
                                      in
                                          Env.set env name value;
                                          (false, value)
                                      end
                                | (Mal.Symbol (_, name) :: []) =>
                                      (Env.set env name Mal.Nil;
                                       (false, Mal.Nil))
                                | _ => Core.error "def! - wrong syntax"

                          else if id = Mal.Let_S then
                              (* let* *)
                              case args of
                                  (Mal.List defs :: exps) =>
                                      let val env' = Env.make env
                                      in
                                          eval_lst_env defs env';
                                          (false, List.foldl (fn (el,va) => eval el env') Mal.Nil exps)
                                      end
                                | (Mal.Vector defs :: exps) =>
                                   let val env' = Env.make env
                                   in
                                       eval_vec_env defs env';
                                       (false, List.foldl (fn (el,va) => eval el env') Mal.Nil exps)
                                   end

                               | _ => Core.error "let* - wrong syntax"

                          else (true, eval_ast ast env)
                    | _ => (true, eval_ast ast env)
    in
        case exp of
            (false, any) => any
          | (true, ast') =>
            (case ast' of
                 Mal.List (ff :: args) =>
                 (case ff of
                      Mal.Function_1N (n,f) => f args
                    | _ => Core.error "function is ecpected")
               | exp => exp)
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

and eval_ast ast env =
    case ast of
        Mal.Symbol (_, name) => Env.get env name
      | Mal.List lst =>
        Mal.List (List.map (fn exp => eval exp env) lst)
      | Mal.Vector vec =>
        Mal.Vector (Vector.map (fn exp => eval exp env) vec)
      | Mal.Hashmap avl =>
        Mal.Hashmap (SDic.fold (fn (k, (k1, v1), m) =>
                                   SDic.insert m (k, (k1, (eval v1 env))))
                               SDic.empty avl)
      | v => v

end

(***** step2_eval.sml *****)

fun READ str = Reader.read_str str

fun EVAL ast env =
    case ast of
        NONE => NONE
      | SOME exp => SOME (Eval.eval exp env)

fun PRINT ast = case ast of NONE => NONE
                          | SOME mval => SOME (Printer.prn_str mval)

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
      repl env;
      print "\n";
      OS.Process.exit (OS.Process.success);
      ()
  end
