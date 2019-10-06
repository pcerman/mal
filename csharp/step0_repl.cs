using System;

namespace Mal
{
    internal class Step0
    {
        string READ(string arg) => arg;

        string EVAL(string arg) => arg;

        string PRINT(string arg) => arg;

        string Rep(string arg)
        {
            return PRINT(EVAL(READ (arg)));
        }

        internal void Repl()
        {
            for (; ;)
            {
                Console.Write("user> ");
                var str = Console.ReadLine();
                if (str == null)
                    break;
                Console.WriteLine(Rep(str));
            }
        }
    }
}
