(***** step1_read_print.sml *****)

fun READ str = Reader.read_str str

fun EVAL ast env = case ast of
                       NONE => NONE
                     | _ => ast

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
                    | _ => (print "Exception\n"; repl env)

fun main() =
    (
      repl ();
      print "\n";
      OS.Process.exit (OS.Process.success);
      ()
    )

val _ = main ()
