fun READ str = str
fun EVAL ast env = ast
fun PRINT ast = ast
fun rep str env = PRINT (EVAL (READ str) env)

fun repl env = (print "user> ";
                case TextIO.inputLine TextIO.stdIn of
                     SOME line => (print (rep line env); repl env)
                   | NONE => ()
               )

fun main() =
    let
        val env = []
    in
        repl env;
        print "\n";
        OS.Process.exit (OS.Process.success);
        ()
    end
