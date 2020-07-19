(***** step9_try.sml *****)

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
               handle Mal.Exception msg => (print ("ERROR: " ^ msg ^ "\n"); repl env)
                    | Mal.Throw arg => (print ("Exception: " ^ (Printer.pr_str arg true) ^ "\n"); repl env)
                    | Subscript => (print "Exception: subscript is out range\n"; repl env)
                    | Overflow => (print "Exception: overflow\n"; repl env)
                    | Div => (print "Exception: division by zero\n"; repl env)
                    | _ => (print "Exception\n"; repl env)

fun envAdd env (name, mf) = Env.set env name (Mal.Function (name, mf, Mal.Nil))

fun makeTopEnv () =
    let
        val env = ref Mal.None
    in
        envAdd env ("+",           Env.makeFun_1N  Core.m_add (Mal.Int 0));
        envAdd env ("-",           Env.makeFun_1N  Core.m_sub (Mal.Int 0));
        envAdd env ("*",           Env.makeFun_1N  Core.m_mul (Mal.Int 1));
        envAdd env ("/",           Env.makeFun_1N  Core.m_div (Mal.Int 1));
        envAdd env ("<",           Env.makeBFun_2N Core.m_lt             );
        envAdd env ("<=",          Env.makeBFun_2N Core.m_lte            );
        envAdd env (">",           Env.makeBFun_2N Core.m_gt             );
        envAdd env (">=",          Env.makeBFun_2N Core.m_gte            );
        envAdd env ("=",           Env.makeBFun_2N Core.m_equal          );
        envAdd env ("list",        Core.m_list                           );
        envAdd env ("list?",       Env.makeFun_1 Core.m_list_p           );
        envAdd env ("vector",      Core.m_vector                         );
        envAdd env ("vector?",     Env.makeFun_1 Core.m_vector_p         );
        envAdd env ("hash-map",    Core.m_hashmap                        );
        envAdd env ("map?",        Env.makeFun_1 Core.m_map_p            );
        envAdd env ("assoc",       Core.m_assoc                          );
        envAdd env ("dissoc",      Core.m_dissoc                         );
        envAdd env ("get",         Env.makeFun_2 Core.m_get              );
        envAdd env ("contains?",   Env.makeFun_2 Core.m_contains_p       );
        envAdd env ("keys",        Env.makeFun_1 Core.m_keys             );
        envAdd env ("vals",        Env.makeFun_1 Core.m_vals             );
        envAdd env ("empty?",      Env.makeFun_1 Core.m_empty_p          );
        envAdd env ("sequential?", Env.makeFun_1 Core.m_sequential_p     );
        envAdd env ("nil?",        Env.makeFun_1 Core.m_nil_p            );
        envAdd env ("true?",       Env.makeFun_1 Core.m_true_p           );
        envAdd env ("false?",      Env.makeFun_1 Core.m_false_p          );
        envAdd env ("symbol?",     Env.makeFun_1 Core.m_symbol_p         );
        envAdd env ("keyword?",    Env.makeFun_1 Core.m_keyword_p        );
        envAdd env ("symbol",      Env.makeFun_1 Core.m_symbol           );
        envAdd env ("keyword",     Env.makeFun_1 Core.m_keyword          );
        envAdd env ("count",       Env.makeFun_1 Core.m_count            );
        envAdd env ("cons",        Env.makeFun_2 Core.m_cons             );
        envAdd env ("concat",      Core.m_concat                         );
        envAdd env ("nth",         Env.makeFun_2 Core.m_nth              );
        envAdd env ("first",       Env.makeFun_1 Core.m_first            );
        envAdd env ("rest",        Env.makeFun_1 Core.m_rest             );
        envAdd env ("atom",        Env.makeFun_1 Core.m_atom             );
        envAdd env ("atom?",       Env.makeFun_1 Core.m_atom_p           );
        envAdd env ("deref",       Env.makeFun_1 Core.m_deref            );
        envAdd env ("reset!",      Env.makeFun_2 Core.m_reset            );
        envAdd env ("swap!",       Core.m_swap                           );
        envAdd env ("prn",         Core.m_prn                            );
        envAdd env ("println",     Core.m_println                        );
        envAdd env ("str",         Mal.String o Core.m_str               );
        envAdd env ("pr-str",      Mal.String o Core.m_pr_str            );
        envAdd env ("read-string", Env.makeFun_1 Core.m_read_string      );
        envAdd env ("slurp",       Env.makeFun_1 Core.m_slurp            );
        envAdd env ("eval",        Env.makeFun_1 (Core.m_eval env)       );
        envAdd env ("apply",       Core.m_apply                          );
        envAdd env ("map",         Core.m_map                            );
        envAdd env ("throw",       Env.makeFun_1 Core.m_throw            );

        env
    end

fun main() =
    let
        val env = makeTopEnv ()
    in
        rep "(def! not (fn* (a) (if a false true)))" env;
        rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))" env;
        rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" env;

        case CommandLine.arguments () of
            (file :: argv) => (Env.set env "*ARGV*" (Mal.List (map Mal.String argv, Mal.Nil));
                               rep ("(load-file \"" ^ file ^ "\")") env)
          | _ => (Env.set env "*ARGV*" (Mal.List ([], Mal.Nil));
                  repl env;
                  print "\n";
                  OS.Process.exit (OS.Process.success));
        ()
    end

val _ = main ()
