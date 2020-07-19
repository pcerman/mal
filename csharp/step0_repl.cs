using System;

namespace Mal
{
    internal class MAL
    {
        static int Main(string[] args)
        {
            return Repl();
        }

        static string READ(string arg) => arg;

        static string EVAL(string arg) => arg;

        static string PRINT(string arg) => arg;

        static string Rep(string arg)
        {
            return PRINT(EVAL(READ (arg)));
        }

        internal static int Repl()
        {
            for (; ;)
            {
                Console.Write("user> ");
                var str = Console.ReadLine();
                if (str == null)
                    break;
                Console.WriteLine(Rep(str));
            }

            return 0;
        }
    }
}
