(***** core.sml *****)

structure Core = struct

fun m_add arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x + y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x + y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x + (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) + y)
      | _ => Mal.error "'+' invalid arguments"

fun m_sub arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x - y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x - y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x - (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) - y)
      | _ => Mal.error "'-' invalid arguments"

fun m_mul arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x * y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x * y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x * (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) * y)
      | _ => Mal.error "'*' invalid arguments"

fun m_div arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Int  (x div y)
      | (Mal.Real x, Mal.Real y) => Mal.Real (x / y)
      | (Mal.Real x, Mal.Int  y) => Mal.Real (x / (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Real ((Real.fromInt x) / y)
      | _ => Mal.error "'/' invalid arguments"

fun m_lt arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x < y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x < y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x < (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) < y)
      | _ => Mal.error "'<' invalid arguments"

fun m_lte arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x <= y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x <= y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x <= (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) <= y)
      | _ => Mal.error "'<' invalid arguments"

fun m_gt arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x > y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x > y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x > (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) > y)
      | _ => Mal.error "'<' invalid arguments"

fun m_gte arg =
    case arg of
        (Mal.Int  x, Mal.Int  y) => Mal.Bool (x >= y)
      | (Mal.Real x, Mal.Real y) => Mal.Bool (x >= y)
      | (Mal.Real x, Mal.Int  y) => Mal.Bool (x >= (Real.fromInt y))
      | (Mal.Int  x, Mal.Real y) => Mal.Bool ((Real.fromInt x) >= y)
      | _ => Mal.error "'<' invalid arguments"

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
                                            case m_equal (al_val hd1, al_val hd2) of
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
                                                    | _ => Mal.error "assoc - only string/symbol/keyword/integer is expeted for key")
              | fold [] hm = hm
              | fold _ _ = Mal.error "assoc - even number of arguments are required"
        in
            Mal.Hashmap ((fold args hm), Mal.Nil)
        end
  | m_assoc _ = Mal.error "assoc - hashmap is expected"

fun m_dissoc ((Mal.Hashmap (hm, _)) :: args) =
        Mal.Hashmap (foldl (fn (arg, hm) => (case arg of
                                                 Mal.String _ => SDic.remove hm (Printer.pr_str arg true)
                                               | Mal.Symbol _ => SDic.remove hm (Printer.pr_str arg true)
                                               | Mal.Keyword _ => SDic.remove hm (Printer.pr_str arg true)
                                               | Mal.Int _ => SDic.remove hm (Printer.pr_str arg true)
                                               | _ => Mal.error "dissoc - only string/symbol/keyword/integer is expeted for key")) hm args,
                     Mal.Nil)
  | m_dissoc _ = Mal.error "dissoc - hashmap is expected"

fun m_hashmap args =
    let fun fold (k :: v :: tl) hm = fold tl (case k of
                                                  Mal.String _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                | Mal.Symbol _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                | Mal.Keyword _ => SDic.insert hm (Printer.pr_str k true,  (k, v))
                                                | Mal.Int _ => SDic.insert hm (Printer.pr_str k true, (k, v))
                                                | _ => Mal.error "hash-map - only string/symbol/keyword/integer is expeted for key")
          | fold [] hm = hm
          | fold _ _ = Mal.error "hash-map - even number of arguments are required"
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
                          | _ => Mal.error "get - only string/symbol/keyword/integer is expeted for key"
        in
            case value of
                SOME (k,v) => v
              | NONE => Mal.Nil
        end
  | m_get Mal.Nil _ = Mal.Nil
  | m_get _ _ = Mal.error "get - hashmap is expected"

fun m_contains_p (Mal.Hashmap (hm, _)) key =
        let
            val value = case key of
                            Mal.String _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Symbol _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Keyword _ => SDic.lookup hm (Printer.pr_str key true)
                          | Mal.Int _ => SDic.lookup hm (Printer.pr_str key true)
                          | _ => Mal.error "contains - only string/symbol/keyword/integer is expeted for key"
        in
            case value of
                SOME _ => Mal.Bool true
              | NONE => Mal.Bool false
        end
  | m_contains_p _ _ = Mal.error "contains - hashmap is expected"

fun m_keys (Mal.Hashmap (hm, _)) = Mal.List (map (#1 o #2) (SDic.alist hm), Mal.Nil)
  | m_keys _ = Mal.error "keys - hashmap is expected"

fun m_vals (Mal.Hashmap (hm, _)) = Mal.List (map (#2 o #2) (SDic.alist hm), Mal.Nil)
  | m_vals _ = Mal.error "vals - hashmap is expected"

fun m_empty_p (Mal.List (lst, _)) = Mal.Bool (null lst)
  | m_empty_p (Mal.Vector (vec, _)) = Mal.Bool ((Vector.length vec) = 0)
  | m_empty_p (Mal.Hashmap (hm, _)) = Mal.Bool (SDic.isEmpty hm)
  | m_empty_p Mal.Nil = Mal.Bool true
  | m_empty_p _ = Mal.error "empty? - wrong type of argument"

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

fun m_int_p (Mal.Int _) = Mal.Bool true
  | m_int_p _ = Mal.Bool false

fun m_real_p (Mal.Real _) = Mal.Bool true
  | m_real_p _ = Mal.Bool false

fun m_int (num as Mal.Int _) = num
  | m_int (Mal.Real num) = Mal.Int (Real.trunc num)
  | m_int _ = Mal.error "int - number is expected"

fun m_real (Mal.Int num) = Mal.Real (Real.fromInt num)
  | m_real (num as Mal.Real _) = num
  | m_real _ = Mal.error "real - number is expected"

fun m_fn_p (Mal.Function _) = Mal.Bool true
  | m_fn_p (Mal.Closure _) = Mal.Bool true
  | m_fn_p _ = Mal.Bool false

fun m_macro_p (Mal.Macro _) = Mal.Bool true
  | m_macro_p _ = Mal.Bool false

fun is_non_atom_char ch = ch <= #" " orelse Char.contains "()[]{}'\"`,;" ch

fun m_symbol (Mal.String "") = Mal.error "symbol - non empty string is required"
  | m_symbol (Mal.String "true") = Mal.error "symbol - invalid name"
  | m_symbol (Mal.String "false") = Mal.error "symbol - invalid name"
  | m_symbol (Mal.String "nil") = Mal.error "symbol - invalid name"
  | m_symbol (Mal.String str) = if String.isPrefix ":" str then
                                    Mal.error "symbol - invalid first character"
                                else if CharVector.exists is_non_atom_char str then
                                    Mal.error "symbol - invalid character"
                                else
                                    (case Reader.is_number str of
                                          Reader.NONUM => Mal.makeSymbol str
                                        | _ => Mal.error "symbol - invalid name"
                                    )
  | m_symbol _ = Mal.error "symbol - string is expected"

fun m_keyword (Mal.String str) = if CharVector.exists is_non_atom_char str then
                                     Mal.error "keyword - invalid character"
                                 else if String.isPrefix ":" str then
                                     Mal.makeKeyword str
                                 else
                                     Mal.makeKeyword (":" ^ str)
  | m_keyword (kw as (Mal.Keyword _)) = kw
  | m_keyword _ = Mal.error "keyword - string is expected"

fun m_count (Mal.List (lst, _)) = Mal.Int (length lst)
  | m_count (Mal.Vector (vec, _)) = Mal.Int (Vector.length vec)
  | m_count (Mal.Hashmap (hm, _)) = Mal.Int (SDic.count hm)
  | m_count Mal.Nil = Mal.Int 0
  | m_count _ = Mal.error "count - invalid argument"

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
           | NONE => Mal.error "read-string - invalid expresion")
  | m_read_string _ = Mal.error "read-string - string is required"

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
            handle _ => Mal.error ("slurp - unable read file: " ^ filename)
    in
        Mal.String (readContent filename)
    end
  | m_slurp _ = (Mal.error "slurp - string is required")

fun m_cons any (Mal.List (lst, _)) = Mal.List (any :: lst, Mal.Nil)
  | m_cons any Mal.Nil = Mal.List ([any], Mal.Nil)
  | m_cons any (Mal.Vector (vec, _)) = Mal.List (any :: (Vector.foldr (op ::) [] vec), Mal.Nil)
  | m_cons _ _ = Mal.error "cons - wrong arguments"

fun m_conj (Mal.List (ls, _) :: args) =
        Mal.List (foldl (op ::) ls args, Mal.Nil)
  | m_conj (Mal.Vector (vec, _) :: args) =
        Mal.Vector (Vector.fromList (Vector.foldr (op ::) args vec), Mal.Nil)
  | m_conj _ = Mal.error "conj - list/vector is expected"

fun m_concat [] = Mal.List ([], Mal.Nil)
  | m_concat lst =
        Mal.List (foldr (fn (el, va) =>
                            (case el of
                                 Mal.Nil => va
                               | Mal.List ([], _) => va
                               | Mal.List (lst, _) => lst @ va
                               | Mal.Vector (vec, _) => Vector.foldr (op ::) va vec
                               | _ => Mal.error "concat - wrong parameters"))
                        []
                        lst,
                  Mal.Nil)

fun m_nth (Mal.List (lst, _)) (Mal.Int i) =
        (List.nth (lst, i)
         handle Subscript => Mal.error "nth - subscript is out of range")
  | m_nth (Mal.Vector (vec, _)) (Mal.Int i) =
        (Vector.sub (vec, i)
         handle Subscript => Mal.error "nth - subscript is out of range")
  | m_nth _ (Mal.Int _) = Mal.error "nth - vector/list is required"
  | m_nth _  _ = Mal.error "nth - integer is required in second argument"

fun m_first Mal.Nil = Mal.Nil
  | m_first (Mal.List ([], _)) = Mal.Nil
  | m_first (Mal.List (lst, _)) = hd lst
  | m_first (Mal.Vector (vec, _)) = if (Vector.length vec) = 0 then Mal.Nil
                                    else Vector.sub (vec, 0)
  | m_first _ = Mal.error "first - list/vector is expected"

fun m_rest Mal.Nil = Mal.List ([], Mal.Nil)
  | m_rest (Mal.List ([], _)) = Mal.List ([], Mal.Nil)
  | m_rest (Mal.List (lst, _)) = Mal.List (tl lst, Mal.Nil)
  | m_rest (Mal.Vector (vec, _)) = if (Vector.length vec) <= 1 then Mal.List ([], Mal.Nil)
                                   else Mal.List (tl (Vector.foldr (op ::) [] vec), Mal.Nil)
  | m_rest _ = Mal.error "rest - list/vector is expected"

fun m_seq (Mal.List ([], _)) = Mal.Nil
  | m_seq (lst as Mal.List _) = lst
  | m_seq (Mal.Vector (vec, _)) = if (Vector.length vec) = 0 then Mal.Nil
                                  else Mal.List (Vector.foldr (op ::) [] vec, Mal.Nil)
  | m_seq (Mal.String "") = Mal.Nil
  | m_seq (Mal.String str) = Mal.List (map (Mal.String o String.str)
                                           (String.explode str),
                                       Mal.Nil)
  | m_seq Mal.Nil = Mal.Nil
  | m_seq _ = Mal.error "seq - wrong argument"

fun m_atom value = Mal.Atom (ref value, Mal.Nil)

fun m_atom_p (Mal.Atom _) = Mal.Bool true
  | m_atom_p _ = Mal.Bool false

fun m_deref (Mal.Atom (at, _)) = !at
  | m_deref _ = Mal.error "deref - atom is expected"

fun m_reset (Mal.Atom (at, _)) value = (at := value; value)
  | m_reset _ _ = Mal.error "reset! - atom is expected"

fun m_with_meta (Mal.List (lst, _)) meta = Mal.List (lst, meta)
  | m_with_meta (Mal.Vector (vec, _)) meta = Mal.Vector (vec, meta)
  | m_with_meta (Mal.Hashmap (hm, _)) meta = Mal.Hashmap (hm, meta)
  | m_with_meta (Mal.Atom (at, _)) meta = let val v = !at in Mal.Atom (ref v, meta) end
  | m_with_meta (Mal.Function (name, ff, _)) meta = Mal.Function (name, ff, meta)
  | m_with_meta (Mal.Closure (vars, rest, exps, env, _)) meta = Mal.Closure (vars, rest, exps, env, meta)
  | m_with_meta (Mal.Macro (vars, rest, exps, env, _)) meta = Mal.Macro (vars, rest, exps, env, meta)
  | m_with_meta _ _ = Mal.error "with-meta - invalid argument"

fun m_meta (Mal.List (_, meta)) = meta
  | m_meta (Mal.Vector (_, meta)) = meta
  | m_meta (Mal.Hashmap (_, meta)) = meta
  | m_meta (Mal.Atom (_, meta)) = meta
  | m_meta (Mal.Function (_, _, meta)) = meta
  | m_meta (Mal.Closure (_, _, _, _, meta)) = meta
  | m_meta (Mal.Macro (_, _, _, _, meta)) = meta
  | m_meta _ = Mal.error "meta - invalid argument"

fun m_eval env ast = Eval.eval ast env

fun apply (Mal.Closure (vars, rest, exps, env, _)) args =
        let val env' = Env.make env
        in
            Eval.eval_args vars rest args env env';
            foldl (fn (ex, va) => Eval.eval ex env') Mal.Nil exps
        end
  | apply (Mal.Function (_, f, _)) args = f args
  | apply _ _ = Mal.error "apply - wrong parameters"

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
  | m_apply _ = Mal.error "apply - wrong arguments"

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
  | m_swap _ = Mal.error "swap! - wrong parameters"

fun m_map [ff, Mal.List (lst, _)] =
        Mal.List (map (fn (el) => apply ff [el]) lst, Mal.Nil)
  | m_map [ff, Mal.Vector (vec, Mal.Nil)] =
        Mal.List (rev (Vector.foldl (fn (el,va) => (apply ff [el]) :: va) [] vec), Mal.Nil)
  | m_map [ff, Mal.Nil] = Mal.List ([], Mal.Nil)
  | m_map _ = Mal.error "map - wrong arguments"

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
  | m_readline _ = Mal.error "readline - string is expected"

fun m_time_ms [] =
    let
        val tm = Time.toMilliseconds (Time.now ())
        val ms = tm mod 864000000
    in
        Mal.Int (Int.fromLarge ms)
    end
  | m_time_ms _ = Mal.error "time_ms = no arguments are required"

end
