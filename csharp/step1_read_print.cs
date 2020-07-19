using System;

namespace Mal
{
    internal class MAL
    {
        static int Main(string[] args)
        {
            return Repl();
        }

        static Value READ(string arg)
        {
            reader.Set(arg);
            return reader.Read_form();
        }

        static Value EVAL(Value arg) => arg;

        static string PRINT(Value arg)
        {
            return Printer.Pr_str(arg, true);
        }

        static string Rep(string arg)
        {
            return PRINT(EVAL(READ(arg)));
        }

        internal static int Repl()
        {
            for (; ;)
            {
                try
                {
                    Console.Write("user> ");
                    var str = Console.ReadLine();
                    if (str == null)
                        break;
                    var val = Rep(str);
                    if (val != null)
                        Console.WriteLine(val);
                }
                catch (MalException ex)
                {
                    Console.WriteLine("ERROR: {0}", ex.Message);
                }
            }

            return 0;
        }

        private static readonly Reader reader = new Reader("");
    }
}
