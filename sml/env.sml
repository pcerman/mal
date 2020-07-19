(***** env.sml *****)

structure Env = struct

fun make out = let val env = Mal.Env { data = SDic.empty, outer = out }
               in
                   ref env
               end

fun set env key value =
    let
        fun set' Mal.None key value =
                 Mal.Env {data = SDic.insert SDic.empty (key, value), outer = ref Mal.None}
          | set' (Mal.Env {data = dt, outer = ot}) key value =
                 Mal.Env {data = SDic.insert dt (key, value), outer = ot}

        val env' = !env
    in
        env := set' env' key value
    end

fun get env key =
    let
        fun get' Mal.None key = Mal.error ("'" ^ key ^ "' not found")
          | get' (Mal.Env {data = dt, outer = ot}) key = (case SDic.lookup dt key of
                                                              NONE => get' (!ot) key
                                                            | SOME v => v)

        val env' = !env
    in
        get' env' key
    end

fun find env key =
    let
        fun find' Mal.None _ = Mal.Nil
          | find' (Mal.Env {data = dt, outer = ot}) key = (case SDic.lookup dt key of
                                                               NONE => find' (!ot) key
                                                             | SOME v => v)

        val env' = !env
    in
        find' env' key
    end

(* Helper functions for Env *)

fun makeFun_1 ff [arg] = ff arg
  | makeFun_1 _ _ = Mal.error "function requires one argument"

fun makeFun_2 ff [arg1, arg2] = ff arg1 arg2
  | makeFun_2 _ _ = Mal.error "function requires two arguments"

fun makeFun_1N ff arg lst =
    case lst of
        [] => Mal.error "function requires at least one argument"
      | [elm] => ff (arg, elm)
      | (hd::tl) => foldl (fn (a1,a2) => ff (a2,a1)) hd tl

fun makeBFun_2N ff lst =
    let fun test a1 a2 [] = ff (a1, a2)
          | test a1 a2 (a3::tl) = case ff (a1, a2) of
                                      (Mal.Bool false) => Mal.Bool false
                                    | (Mal.Bool true) => test a2 a3 tl
                                    | _ => Mal.Bool false
    in
        case lst of
            [] => Mal.error "function requires at least two arguments"
          | [elm] => Mal.error "function requires at least two arguments"
          | (a1::a2::tl) => test a1 a2 tl
    end

end
