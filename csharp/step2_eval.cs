using System;
using System.Collections.Generic;

namespace Mal
{
    internal class Step2
    {
        Value READ(string arg)
        {
            reader.Set(arg);
            return reader.Read_form();
        }

        Value EVAL(Value arg, Env env)
        {
            Value ast = Eval_ast(arg, env);

            if (ast is List lst)
            {
                if (lst.IsEmpty())
                    return ast;

                Value val = lst.First();

                if (val is Func_Native fn)
                {
                    return fn.Apply(lst.Rest());
                }
                else
                    throw new MalException("function is expected");
            }

            return ast;
        }

        string PRINT(Value arg)
        {
            return Printer.Pr_str(arg, true);
        }

        string Rep(string arg, Env env)
        {
            return PRINT(EVAL(READ(arg), env));
        }

        Value Eval_ast(Value arg, Env env)
        {
            if (env == null)
                return arg;

            if (arg == null)
                return arg;

            if (arg is Symbol sym)
            {
                Value val = env.Get(sym);

                if (val == null)
                    throw new MalException(string.Format("'{0}' not found", sym.Name));

                return val;
            }
            else if (arg is List lst)
            {
                if (lst.Elements == null)
                    return arg;

                List<Value> elms = new List<Value>();
                for (List.Node elm = lst.Elements; elm != null; elm = elm.cdr)
                {
                    elms.Add(EVAL(elm.car, env));
                }

                return new List(elms);
            }
            else if (arg is Vector vec)
            {
                List<Value> elms = new List<Value>();

                foreach (Value v in vec.Elements)
                    elms.Add(EVAL(v, env));

                return new Vector(elms);
            }
            else if (arg is HashMap map)
            {
                HashMap hm = new HashMap();
                Dictionary<Value, Value> elms = hm.Elements;

                foreach (var kv in map.Elements)
                    elms[kv.Key] = EVAL(kv.Value, env);

                return hm;
            }

            return arg;
        }

        Env CreateTopLevelEnv()
        {
            Env env = new Env();

            env.Set(Reader.AddSymbol("+") as Symbol, new Func_Add());
            env.Set(Reader.AddSymbol("-") as Symbol, new Func_Sub());
            env.Set(Reader.AddSymbol("*") as Symbol, new Func_Mul());
            env.Set(Reader.AddSymbol("/") as Symbol, new Func_Div());

            return env;
        }

        internal void Repl()
        {
            Env env = CreateTopLevelEnv();

            for (; ;)
            {
                try
                {
                    Console.Write("user> ");
                    var str = Console.ReadLine();
                    if (str == null)
                        break;
                    var val = Rep(str, env);
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
