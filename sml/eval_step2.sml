(**** eval_step2.sml *****)

structure Eval = struct

fun eval ast env =
    case ast of
       ast' as Mal.List (_ :: _, _) =>
           (* function apply *)
           (case eval_ast ast' env of
                Mal.List (Mal.Function (_, f, _) :: args, _) => f args
              | _ => Mal.error "function or closure is expected")
     | ast' =>
           eval_ast ast' env

and eval_args [] NONE [] env cenv = ()
  | eval_args [] (SOME sym) [] env cenv =
        (Env.set cenv sym (Mal.List ([], Mal.Nil)); ())
  | eval_args [] NONE pars env cenv = Mal.error "invalid number of closure arguments"
  | eval_args [] (SOME sym) pars env cenv =
        (Env.set cenv sym (Mal.List (pars, Mal.Nil)); ())
  | eval_args (arg :: args) rest (par :: pars) env cenv =
        (Env.set cenv arg par;
         eval_args args rest pars env cenv)
  | eval_args _ _ _ _ _ = Mal.error "invalid number of closure arguments"

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

end
