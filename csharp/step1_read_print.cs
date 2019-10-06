using System;

namespace Mal
{
    internal class Step1
    {
        Value READ(string arg)
        {
            reader.Set(arg);
            return reader.Read_form();
        }

        Value EVAL(Value arg) => arg;

        string PRINT(Value arg)
        {
            return Printer.Pr_str(arg, true);
        }

        string Rep(string arg)
        {
            return PRINT(EVAL(READ(arg)));
        }

        internal void Repl()
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
        }

        private readonly Reader reader = new Reader("");
    }
}
