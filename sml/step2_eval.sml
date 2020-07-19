(***** step2_eval.sml *****)

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

        env
    end

fun main() =
    let
        val env = makeTopEnv ()
    in
        repl env;
        print "\n";
        OS.Process.exit (OS.Process.success);
        ()
    end

val _ = main ()
