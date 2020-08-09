(**** eval_step4.sml *****)

structure Eval = struct

fun eval ast env =
    case ast of
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
               | _ => Mal.error "def! - wrong syntax")

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
              | _ => Mal.error "let* - wrong syntax")

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
              | _ => Mal.error "if - wrong syntax")

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
              | _ => Mal.error "fn* - wrong syntax")

     | ast' as Mal.List (_ :: _, _) =>
           (* function apply *)
           (case eval_ast ast' env of
                Mal.List (Mal.Function (_, f, _) :: args, _) => f args
              | Mal.List (Mal.Closure (vars, rest, exps, cenv, _) :: args, _) =>
                    let val env' = Env.make cenv
                    in eval_args vars rest args env env';
                       foldl (fn (exp,va) => eval exp env') Mal.Nil exps
                    end
              | _ => Mal.error "function or closure is expected")
     | ast' =>
           eval_ast ast' env

and eval_lst_env [] env = ()
  | eval_lst_env (Mal.Symbol sym :: exp :: defs) env =
        (Env.set env sym (eval exp env);
         eval_lst_env defs env)
  | eval_lst_env _ _ = Mal.error "let* - wrong symbols definition"

and eval_vec_env vec env =
    if Vector.length vec mod 2 <> 0 then
        Mal.error "let* - wrong symbols definition"
    else
        Vector.foldli (fn (i, el, va) => if i mod 2 = 0 then
                                             case Vector.sub (vec, i) of
                                                 (Mal.Symbol symb) => symb
                                               | _ => Mal.error "let* - wrong symbols definition"
                                         else
                                             ((Env.set env va (eval (Vector.sub (vec, i)) env)); ""))
                       "" vec

and get_lst_args args =
    let
        fun get_args [] true  args   = Mal.error "fn* - invalid parameters"
          | get_args [] false args   = (rev args, NONE)
          | get_args [arg] true args = (case arg of
                                            Mal.Symbol sym => (rev args, SOME sym)
                                          | _ => Mal.error "fn* - invalid parameters")
          | get_args (arg :: tl) true  args = Mal.error "fn* - invalid parameters"
          | get_args (arg :: tl) false args = (case arg of
                                                   Mal.Symbol sym =>
                                                       if sym = "&"
                                                       then get_args tl true args
                                                       else get_args tl false (sym :: args)
                                                 | _ => Mal.error "fn* - invalid parameters")
    in
        get_args args false []
    end

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
