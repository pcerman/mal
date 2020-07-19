#define NATIVE_LOAD_FILE

using System;
using System.Collections.Generic;
using System.IO;

namespace Mal
{
    internal class MAL
    {
        static int Main(string[] args)
        {
            Env env = CreateTopLevelEnv();

            List<Value> malArgs = new List<Value>();
            for (int i=1; i < args.Length; i++)
                malArgs.Add(new Str(args[i]));

            env.Set(Reader.AddSymbol("*ARGV*") as Symbol, new List(malArgs));

            if (args.Length == 0)
                return Repl(env);

#if NATIVE_LOAD_FILE
            FileInfo fi = new FileInfo(args[0]);
            if (fi.Exists)
            {
                using (Stream stream = fi.OpenRead())
                    LoadStream(stream, env);
            }
            else
            {
                Console.Error.WriteLine("ERROR: unable open file '{0}'", args[0]);
                return 1;
            }
#else
            List loaderArgs = new List(new List<Value>() { new Str(args[0]) });
            try
            {
                var val = env.Get(Reader.Load_file);
                if (val is Closure cls)
                {
                    EVAL(cls.Body, cls.CreateEnv(loaderArgs));
                }
                else
                if (val is Func_Native fn)
                {
                    fn.Apply(loaderArgs);
                }
                else
                    throw new MalException("unknown function to evaluate file");
            }
            catch (MalException ex)
            {
                Console.Error.WriteLine("ERROR: {0}", ex.Message);
                return 1;
            }
#endif
            return 0;
        }

        static Value READ(string arg)
        {
            reader.Set(arg);
            return reader.Read_form();
        }

        static Value EVAL(Value arg, Env env)
        {
            for (;;)
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

                            arg = lst.Nth(2);
                            env = newEnv;
                            continue;

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
                            if (cnt == 3)
                                return Reader.Nil;

                            arg = lst.Nth(3);
                            continue;
                        }
                        else
                        {
                            arg = lst.Nth(2);
                            continue;
                        }
                    }
                    else if (val == Reader.Do)
                    {
                        if (lst.Count() < 2)
                            throw new MalException(Reader.Do.Name + " - at least one argument is required");

                        int cnt = lst.Count() - 1;

                        for (int i = 1; i < cnt; i++)
                            EVAL(lst.Nth(i), env);

                        arg = lst.Nth(cnt);
                        continue;
                    }
                    else if (val == Reader.Fn)
                    {
                        if (lst.Count() != 3)
                            throw new MalException(Reader.Fn.Name + " - syntax error");

                        if (lst.Nth(1) is Sequence seq)
                        {
                            int cnt = seq.Count();
                            for (int i = 0; i < cnt; i++)
                            {
                                if (!(seq.Nth(i) is Symbol))
                                    throw new MalException(Reader.Fn.Name + " - symbol is expected for argument");
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
                        {
                            arg = cls.Body;
                            env = cls.CreateEnv((ast as List).Rest());
                            continue;
                        }
                        else
                            throw new MalException("function is expected");
                    }
                }
                else
                    return Eval_ast(arg, env);
            }
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

            EnvAddFunction(env,
                new Func_Add(),
                new Func_Sub(),
                new Func_Mul(),
                new Func_Div(),
                new Func_LT(),
                new Func_GT(),
                new Func_LE(),
                new Func_GE(),
                new Func_EQ(),
                new Func_List(),
                new Func_Predicate("list?", (val) => val is List),
                new Func_Vector(),
                new Func_Predicate("vector?", (val) => val is Vector),
                new Func_IsEmpty(),
                new Func_Count(),
                new Func_Pr_str(),
                new Func_Str(),
                new Func_Prn(),
                new Func_Println(),
                new Func_ReadString(),
                new Func_Slurp(),
                new Func_Eval(EVAL, env),
                new Func_Atom(),
                new Func_Predicate("atom?", (val) => val is Atom),
                new Func_Deref(),
                new Func_Reset(),
                new Func_Swap(EVAL),
#if NATIVE_LOAD_FILE
                new Func_Load_file(EVAL, env),
#endif
                new Func_Cons(),
                new Func_Concat()
            );

            Rep("(def! not (fn* [x] (if x false true)))", env);
#if !NATIVE_LOAD_FILE
            Rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", env);
#endif
            return env;
        }

        static void EnvAddFunction(Env env, params Func[] fns)
        {
            foreach (Func fn in fns)
                env.Set(Reader.AddSymbol(fn.FnName) as Symbol, fn);
        }

#if NATIVE_LOAD_FILE
        static void LoadStream(Stream stream, Env env)
        {
            try
            {
                Reader reader = new Reader(stream);
                for (; ;)
                {
                    var form = reader.Read_form();
                    if (form == null)
                        break;
                    EVAL(form, env);
                }
            }
            catch (MalException ex)
            {
                Console.WriteLine("ERROR: {0}", ex.Message);
            }
        }
#endif

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
