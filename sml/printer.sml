(***** printer.sml *****)

structure Printer = struct

fun to_cstring str =
    let
        fun convert [] cstr = String.implode (rev cstr)
          | convert (#"\n" :: tl) cstr = convert tl (#"n"  :: #"\\" :: cstr)
          | convert (#"\r" :: tl) cstr = convert tl (#"r"  :: #"\\" :: cstr)
          | convert (#"\t" :: tl) cstr = convert tl (#"t"  :: #"\\" :: cstr)
          | convert (#"\"" :: tl) cstr = convert tl (#"\"" :: #"\\" :: cstr)
          | convert (#"\\" :: tl) cstr = convert tl (#"\\" :: #"\\" :: cstr)
          | convert (ch :: tl) cstr = convert tl (ch :: cstr)
    in
        convert (String.explode str) []
    end

fun pr_str mval readable =
    let
        fun prn_str Mal.Nil = "nil"
          | prn_str (Mal.String str) = if readable
                                       then "\"" ^ (to_cstring str) ^ "\""
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
