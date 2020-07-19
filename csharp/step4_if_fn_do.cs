using System;
using System.Collections.Generic;

namespace Mal
{
    internal class Step4
    {
        static int Main(string[] args)
        {
            Env env = CreateTopLevelEnv();

            return Repl(env);
        }

        static Value READ(string arg)
        {
            reader.Set(arg);
            return reader.Read_form();
        }

        static Value EVAL(Value arg, Env env)
        {
            if (arg is List lst)
            {
                if (lst.IsEmpty())
                    return arg;

                Value val = lst.First();

                if (val == Reader.Def)
                {
                    if (lst.Count() != 3)
                        throw new MalException(Reader.Def.Name + " - syntax error");

                    val = lst.Nth(1);

                    if (val is Symbol sym)
                    {
                        val = EVAL(lst.Nth(2), env);
                        env.Set(sym, val);

                        return val;
                    }
                    else
                        throw new MalException(Reader.Def.Name + " - symbol expected");
                }
                else if (val == Reader.Let)
                {
                    if (lst.Count() != 3)
                        throw new MalException(Reader.Let.Name + " - syntax error");

                    Env newEnv = new Env(env);

                    Value val_1 = lst.Nth(1);
                    if (val_1 is Sequence syms && syms.Count() % 2 == 0)
                    {
                        int cnt = syms.Count();
                        for (int i = 0; i < cnt; i += 2)
                        {
                            if (syms.Nth(i) is Symbol sym)
                            {
                                newEnv.Set(sym, EVAL(syms.Nth(i + 1), newEnv));
                            }
                            else
                                throw new MalException(Reader.Let.Name + " - bindings - symbol is expected");
                        }

                        return EVAL(lst.Nth(2), newEnv);
                    }
                    else
                        throw new MalException(Reader.Let.Name + " - bad bindings");
                }
                else if (val == Reader.If)
                {
                    int cnt = lst.Count();

                    if (cnt != 3 && cnt != 4)
                        throw new MalException(Reader.If.Name + " - syntax error");

                    Value tst = EVAL(lst.Nth(1), env);
                    if (tst == null || tst == Reader.Nil || tst == Reader.False)
                    {
                        return cnt == 3 ? Reader.Nil : EVAL(lst.Nth(3), env);
                    }
                    else
                    {
                        return EVAL(lst.Nth(2), env);
                    }
                }
                else if (val == Reader.Do)
                {
                    if (lst.Count() < 2)
                        throw new MalException(Reader.Do.Name + " - at least one argument is required");

                    Value res = null;

                    int cnt = lst.Count();
                    for (int i = 1; i < cnt; i++)
                        res = EVAL(lst.Nth(i), env);

                    return res;
                }
                else if (val == Reader.Fn)
                {
                    if (lst.Count() != 3)
                        throw new MalException(Reader.Fn.Name + " - syntax error");

                    if (lst.Nth(1) is Sequence seq)
                    {
                        int cnt = seq.Count();
                        for (int i=0; i < cnt; i++)
                        {
                            if (!(seq.Nth(i) is Symbol))
                                throw new MalException(Reader.Fn.Name +" - symbol is expected for argument");
                        }
                    }
                    else
                        throw new MalException(Reader.Fn.Name + " - expected sequence of arguments");

                    if (lst.Nth(2) == null)
                        throw new MalException(Reader.Fn.Name + " - function body is expected");

                    return new Closure(lst.Nth(1) as Sequence, lst.Nth(2), env);
                }
                else
                {
                    Value ast = Eval_ast(arg, env);

                    val = (ast as List).First();

                    if (val is Func_Native fn)
                        return fn.Apply((ast as List).Rest());
                    else if (val is Closure cls)
                        return EVAL(cls.Body, cls.CreateEnv((ast as List).Rest()));
                    else
                        throw new MalException("function is expected");
                }
            }
            else
                return Eval_ast(arg, env);
        }

        static string PRINT(Value arg)
        {
            return Printer.Pr_str(arg, true);
        }

        static string Rep(string arg, Env env)
        {
            return PRINT(EVAL(READ(arg), env));
        }

        static Value Eval_ast(Value arg, Env env)
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

        static Env CreateTopLevelEnv()
        {
            Env env = new Env();

            env.Set(Reader.AddSymbol("+") as Symbol, new Func_Add());
            env.Set(Reader.AddSymbol("-") as Symbol, new Func_Sub());
            env.Set(Reader.AddSymbol("*") as Symbol, new Func_Mul());
            env.Set(Reader.AddSymbol("/") as Symbol, new Func_Div());
            env.Set(Reader.AddSymbol("<") as Symbol, new Func_LT());
            env.Set(Reader.AddSymbol(">") as Symbol, new Func_GT());
            env.Set(Reader.AddSymbol("<=") as Symbol, new Func_LE());
            env.Set(Reader.AddSymbol(">=") as Symbol, new Func_GE());
            env.Set(Reader.AddSymbol("=") as Symbol, new Func_EQ());
            env.Set(Reader.AddSymbol("list") as Symbol, new Func_List());
            env.Set(Reader.AddSymbol("list?") as Symbol, new Func_Predicate("list?", (val) => val is List));
            env.Set(Reader.AddSymbol("vector") as Symbol, new Func_Vector());
            env.Set(Reader.AddSymbol("vector?") as Symbol, new Func_Predicate("vector?", (val) => val is Vector));
            env.Set(Reader.AddSymbol("empty?") as Symbol, new Func_IsEmpty());
            env.Set(Reader.AddSymbol("count") as Symbol, new Func_Count());
            env.Set(Reader.AddSymbol("pr-str") as Symbol, new Func_Pr_str());
            env.Set(Reader.AddSymbol("str") as Symbol, new Func_Str());
            env.Set(Reader.AddSymbol("prn") as Symbol, new Func_Prn());
            env.Set(Reader.AddSymbol("println") as Symbol, new Func_Println());

            Rep("(def! not (fn* [x] (if x false true)))", env);

            return env;
        }

        internal static int Repl(Env env)
        {
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

            return 0;
        }

        private static readonly Reader reader = new Reader("");
    }
}
