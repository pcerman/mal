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

fun from_cstring str =
  let
      fun convert [] false cstr = String.implode (rev cstr)
        | convert [] true _ = Mal.error "unbalanced string"
        | convert (ch :: tl) true cstr = convert tl false
                                                 (case ch of
                                                      #"n"  => #"\n" :: cstr
                                                    | #"r"  => #"\r" :: cstr
                                                    | #"t"  => #"\t" :: cstr
                                                    | #"\"" => #"\"" :: cstr
                                                    | #"\\" => #"\\" :: cstr
                                                    | ch => ch :: #"\\" :: cstr)
        | convert (#"\\" :: tl) _ cstr = convert tl true cstr
        | convert (ch :: tl) false cstr = convert tl false (ch :: cstr)
  in
      convert (String.explode str) false []
  end

fun get_string str =
  let
      val len = size str
      val chr = String.sub (str, len - 1)
      val sstr = if len < 2 orelse chr <> #"\""
                 then Mal.error "unbalanced string"
                 else String.substring (str, 1, len - 2)
  in
      Mal.String (from_cstring sstr)
  end

fun get_atom str =
  case is_number str of
      INT => (case Int.fromString str of
                  NONE => Mal.error "invalid number"
                | SOME num => Mal.Int num)
    | REAL => (case Real.fromString str of
                   NONE => Mal.error "invalid number"
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
                NONE => Mal.error "unbalanced list"
              | SOME ")" => (SOME (Mal.List (rev lst, Mal.Nil)), rdr2)
              | SOME "]" => Mal.error "unbalanced list"
              | SOME "}" => Mal.error "unbalanced list"
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
                NONE => Mal.error "unbalanced vector"
              | SOME "]" => (SOME (Mal.Vector (Vector.fromList (rev lst), Mal.Nil)), rdr2)
              | SOME ")" => Mal.error "unbalanced vector"
              | SOME "}" => Mal.error "unbalanced vector"
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
                NONE => Mal.error "unbalanced hashmap"
              | SOME "}" => if Option.isSome key
                            then Mal.error "hashmap - even number of elements is required"
                            else (SOME (Mal.Hashmap (avl, Mal.Nil)), rdr2)
              | SOME ")" => Mal.error "unbalanced hashmap"
              | SOME "]" => Mal.error "unbalanced hashmap"
              | _ => let
                        val (frm, rdr2) = read_form rdr
                     in
                         case frm of
                             NONE => Mal.error "unbalanced hashmap"
                           | SOME v => (case key of
                                            NONE => (case v of
                                                         Mal.String _ => next rdr2 avl frm
                                                       | Mal.Symbol _ => next rdr2 avl frm
                                                       | Mal.Keyword _ => next rdr2 avl frm
                                                       | Mal.Int _ => next rdr2 avl frm
                                                       | _ => Mal.error "hashmap - only string/symbol/keyword/integer is expeted for key")
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
                                 NONE => Mal.error "form is expected"
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
