using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Mal
{
    internal class MalQuitExc : Exception
    {
        internal MalQuitExc() : base("Quit exception") { }
    }

    internal class Func_Quit : Func_Native
    {
        internal Func_Quit() : base("quit") { }

        internal override Value Apply(Sequence args)
        {
            if (args.Count() != 0)
                throw Error("no argument is expected");

            throw new MalQuitExc();
        }
    }

    internal abstract class Func_OneArg : Func_Native
    {
        protected Func_OneArg(string fn_name) : base(fn_name) { }

        internal sealed override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt != 1)
                throw Error("one argument is expected");

            return Apply_OneArg(args.Nth(0));
        }

        protected abstract Value Apply_OneArg(Value arg);
    }

    internal abstract class Func_TwoArgs : Func_Native
    {
        protected Func_TwoArgs(string fn_name) : base(fn_name) { }

        internal sealed override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt != 2)
                throw Error("two arguments are expected");

            return Apply_TwoArgs(args.Nth(0), args.Nth(1));
        }

        protected abstract Value Apply_TwoArgs(Value arg1, Value arg2);
    }


    internal class Func_Add : Func_Native
    {
        internal Func_Add() : base("+") { }

        internal override Value Apply(Sequence args)
        {
            Number res = Integer.Zero;

            int cnt = args.Count();
            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number num)
                    res = (i == 0) ? num : res + num;
                else
                    throw Invalid(i + 1);
            }

            return res;
        }
    }

    internal class Func_Sub : Func_Native
    {
        internal Func_Sub() : base("-") { }

        internal override Value Apply(Sequence args)
        {
            Number res = null;

            int cnt = args.Count();
            if (cnt < 1)
                throw Error_OneMore();

            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number num)
                {
                    res = (i == 0) ? num : res - num;
                }
                else
                    throw Invalid(i + 1);
            }

            if (cnt == 1)
                res = Integer.Zero - res;

             return res;
        }
    }

    internal class Func_Mul : Func_Native
    {
        internal Func_Mul() : base("*") { }

        internal override Value Apply(Sequence args)
        {
            Number res = Integer.One;

            int cnt = args.Count();
            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number num)
                    res = (i == 0) ? num : res * num;
                else
                    throw Invalid(i + 1);
            }

            return res;
        }
    }

    internal class Func_Div : Func_Native
    {
        internal Func_Div() : base("/") { }

        internal override Value Apply(Sequence args)
        {
            Number res = null;

            int cnt = args.Count();
            if (cnt < 1)
                throw Error_OneMore();

            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number num)
                {
                    res = (i == 0) ? num : res / num;
                }
                else
                    throw Invalid(i + 1);
            }

            if (cnt == 1)
                res = Real.One / res;

            return res;
        }
    }

    internal class Func_LT : Func_Native
    {
        internal Func_LT() : base("<") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            Number n1 = null;
            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number n2)
                {
                    if (n1 != null && (n1 < n2) == Reader.False)
                        return Reader.False;
                    n1 = n2;
                }
                else
                    throw Invalid(i + 1);
            }

            return Reader.True;
        }
    }

    internal class Func_GT : Func_Native
    {
        internal Func_GT() : base(">") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            Number n1 = null;
            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number n2)
                {
                    if (n1 != null && (n1 > n2) == Reader.False)
                        return Reader.False;
                    n1 = n2;
                }
                else
                    throw Invalid(i + 1);
            }

            return Reader.True;
        }
    }

    internal class Func_LE : Func_Native
    {
        internal Func_LE() : base("<=") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            Number n1 = null;
            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number n2)
                {
                    if (n1 != null && (n1 <= n2) == Reader.False)
                        return Reader.False;
                    n1 = n2;
                }
                else
                    throw Invalid(i + 1);
            }

            return Reader.True;
        }
    }

    internal class Func_GE : Func_Native
    {
        internal Func_GE() : base(">=") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            Number n1 = null;
            for (int i = 0; i < cnt; i++)
            {
                if (args.Nth(i) is Number n2)
                {
                    if (n1 != null && (n1 >= n2) == Reader.False)
                        return Reader.False;
                    n1 = n2;
                }
                else
                    throw Invalid(i + 1);
            }

            return Reader.True;
        }
    }

    internal class Func_EQ : Func_Native
    {
        internal Func_EQ() : base("=") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            Value val = args.Nth(0);
            for (int i = 1; i < cnt; i++)
            {
                if (!IsEqual_To(val, args.Nth(i)))
                    return Reader.False;
            }

            return Reader.True;
        }
    }

    internal class Func_List : Func_Native
    {
        internal Func_List() : base("list") { }

        internal override Value Apply(Sequence args)
        {
            return args.Drop(0);
        }
    }

    internal class Func_Vector : Func_Native
    {
        internal Func_Vector() : base("vector") { }

        internal override Value Apply(Sequence args)
        {
            return args is Vector ? args : new Vector(args.Drop(0));
        }
    }

    internal class Func_IsEmpty : Func_OneArg
    {
        internal Func_IsEmpty() : base("empty?") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg == Reader.Nil)
                return Reader.True;

            if (arg is Sequence seq)
                return Bool(seq.IsEmpty());

            if (arg is Str str)
                return Bool(str.IsEmpty());

            if (arg is HashMap map)
                return Bool(map.IsEmpty());

            throw Error("sequence/hash-map/string is expected");
        }
    }

    internal class Func_Count : Func_OneArg
    {
        internal Func_Count() : base("count") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg == Reader.Nil)
                return Integer.Zero;

            if (arg is Sequence seq)
                return new Integer(seq.Count());

            if (arg is Str str)
                return new Integer(str.Length());

            if (arg is HashMap map)
                return new Integer(map.Elements.Count);

            throw Error("sequence/hash-map/string is expected");
        }
    }

    internal class Func_Pr_str : Func_Native
    {
        internal Func_Pr_str() : base("pr-str") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();

            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < cnt; i++)
            {
                if (i > 0)
                    sb.Append(' ');
                args.Nth(i).Build_str(sb, true);
            }

            return new Str(sb.ToString());
        }
    }

    internal class Func_Str : Func_Native
    {
        internal Func_Str() : base("str") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();

            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < cnt; i++)
            {
                args.Nth(i).Build_str(sb, false);
            }

            return new Str(sb.ToString());
        }
    }

    internal class Func_Prn : Func_Native
    {
        internal Func_Prn() : base("prn") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();

            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < cnt; i++)
            {
                if (i > 0)
                    sb.Append(' ');
                args.Nth(i).Build_str(sb, true);
            }

            Console.WriteLine(sb.ToString());

            return Reader.Nil;
        }
    }

    internal class Func_Println : Func_Native
    {
        internal Func_Println() : base("println") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();

            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < cnt; i++)
            {
                if (i > 0)
                    sb.Append(' ');
                args.Nth(i).Build_str(sb, false);
            }

            Console.WriteLine(sb.ToString());

            return Reader.Nil;
        }
    }

    internal class Func_ReadString : Func_OneArg
    {
        internal Func_ReadString() : base("read-string") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Str str)
            {
                Value val = Reader.Read_str(str.Value);
                if (val == null)
                    throw Error("unexpected EOF");

                return val;
            }

            throw Expected("string");
        }
    }

    internal class Func_Slurp : Func_OneArg
    {
        internal Func_Slurp() : base("slurp") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Str str)
            {
                string file_content = null;

                FileInfo fi = new FileInfo(str.Value);
                if (!fi.Exists)
                    throw Error("file '{0}' not exists", str.Value);

                using (StreamReader stream = fi.OpenText())
                {
                    file_content = stream.ReadToEnd();
                }

                return new Str(file_content);
            }

            throw Expected("string");
        }
    }

    internal class Func_Eval : Func_OneArg
    {
        internal Func_Eval(Eval eval, Env env) : base("eval")
        {
            this.eval = eval;
            this.env = env;
        }

        protected override Value Apply_OneArg(Value arg)
        {
            return eval(arg, env);
        }

        private readonly Eval eval;
        private readonly Env env;
    }

    internal class Func_Atom : Func_OneArg
    {
        internal Func_Atom() : base("atom") { }

        protected override Value Apply_OneArg(Value arg)
        {
            return new Atom(arg);
        }
    }

    internal class Func_Deref : Func_OneArg
    {
        internal Func_Deref() : base("deref") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Atom atm)
                return atm.Value;

            throw Expected("atom");
        }
    }

    internal class Func_Reset : Func_TwoArgs
    {
        internal Func_Reset() : base("reset!") { }

        protected override Value Apply_TwoArgs(Value arg1, Value arg2)
        {
            if (arg1 is Atom atm)
            {
                atm.Value = arg2;
                return atm.Value;
            }

            throw Expected("atom");
        }
    }

    internal class Func_Swap : Func_Native
    {
        internal Func_Swap(Eval eval) : base("swap!")
        {
            this.eval = eval;
        }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            if (args.Nth(0) is Atom atm)
            {
                Value arg2 = args.Nth(1);
                if (arg2 is Closure cls)
                {
                    atm.Value = eval(cls.Body, cls.CreateEnv(args.Drop(2).Cons(atm.Value)));
                }
                else if (arg2 is Func_Native fn)
                {
                    atm.Value = fn.Apply(args.Drop(2).Cons(atm.Value));
                }
                else
                    throw Expected("function", 2);

                return atm.Value;
            }

            throw Expected("atom");
        }

        readonly Eval eval;
    }

    internal class Func_Load_file : Func_OneArg
    {
        internal Func_Load_file(Eval eval, Env env)
            : base("load-file")
        {
            this.eval = eval;
            this.env = env;
        }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Str str)
            {
                FileInfo fi = new FileInfo(str.Value);

                if (!fi.Exists)
                    throw Error("file '{0}' not exists", str.Value);

                using (Stream stream = fi.OpenRead())
                {
                    Reader reader = new Reader(stream);
                    for (; ; )
                    {
                        var form = reader.Read_form();
                        if (form == null)
                            break;
                        eval(form, env);
                    }
                }

                return Reader.Nil;
            }

            throw Expected("string");
        }

        private readonly Eval eval;
        private readonly Env env;
    }

    internal class Func_Cons : Func_TwoArgs
    {
        internal Func_Cons() : base("cons") { }

        protected override Value Apply_TwoArgs(Value arg1, Value arg2)
        {
            if (arg2 is Sequence seq)
            {
                return seq.Cons(arg1);
            }

            throw Expected("sequence", 2);
        }
    }

    internal class Func_Concat : Func_Native
    {
        internal Func_Concat() : base("concat") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();

            List app = new List();

            for (int i = cnt - 1; i >= 0; i--)
            {
                Value val = args.Nth(i);

                if (val is List lst)
                    app = List.Append(lst, app);
                else if (val is Vector vec)
                    app = List.Append_D(vec.Drop(0), app);
                else if (val != Reader.Nil)
                    throw Expected("sequence", i + 1);
            }

            return app;
        }
    }

    internal class Func_First : Func_OneArg
    {
        internal Func_First() : base("first") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Sequence seq)
                return seq.First();

            if (arg == Reader.Nil)
                return arg;

            throw Expected("sequence");
        }
    }

    internal class Func_Rest : Func_OneArg
    {
        internal Func_Rest() : base("rest") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Sequence seq)
                return seq.Rest();

            if (arg == Reader.Nil)
                return new List();

            throw Expected("sequence");
        }
    }

    internal class Func_Nth : Func_TwoArgs
    {
        internal Func_Nth() : base("nth") { }

        protected override Value Apply_TwoArgs(Value arg1, Value arg2)
        {
            if (arg1 is Sequence seq)
            {
                if (arg2 is Integer ii)
                    return seq.Nth((int)ii.Num);

                throw Expected("sequence");
            }

            throw Expected("sequence");
        }
    }

    internal class Func_Predicate : Func_OneArg
    {
        internal delegate bool Predicate(Value val);

        internal Func_Predicate(string name, Predicate predicate)
            : base(name)
        {
            this.predicate = predicate;
        }

        protected override Value Apply_OneArg(Value arg)
        {
            return Bool(predicate(arg));
        }

        private readonly Predicate predicate;
    }

    internal class Func_HashMap : Func_Native
    {
        internal Func_HashMap() : base("hash-map") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt % 2 == 1)
                throw Error("even number os arguments is required");

            HashMap hm = new HashMap();
            Dictionary<Value, Value> dic = hm.Elements;

            for (int i = 0; i < cnt; i += 2)
            {
                Value key = args.Nth(i);
                if (key is Symbol || key is Keyword || key is Str)
                {
                    dic.Add(key, args.Nth(i + 1));
                }
                else
                    throw Expected("symbol/keyword/string", i + 1);
            }

            return hm;
        }
    }

    internal class Func_Keyword : Func_OneArg
    {
        internal Func_Keyword() : base("keyword") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Str str)
            {
                string cstr = str.Value;
                if (string.IsNullOrWhiteSpace(cstr))
                    cstr = ":";
                else
                {
                    for (int i = 0; i < cstr.Length; i++)
                    {
                        if (Reader.IsNonAtomChar(cstr[i]))
                            throw Error("invalid character");
                    }

                    if (cstr.Length == 0 || cstr[0] != ':')
                        cstr = ":" + cstr;
                }

                return Reader.AddSymbol(cstr);
            }

            if (arg is Keyword)
                return arg;

            throw Expected("string/keyword");
        }
    }

    internal class Func_Symbol : Func_OneArg
    {
        internal Func_Symbol() : base("symbol") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Str str)
            {
                string cstr = str.Value;
                if (!string.IsNullOrWhiteSpace(cstr))
                {
                    for (int i = 0; i < cstr.Length; i++)
                    {
                        if (Reader.IsNonAtomChar(cstr[i]))
                            throw Error("invalid character");
                    }
                }

                if (cstr.Length == 0)
                    throw Error("non empty string is required");

                if (cstr[0] == ':')
                    throw Error("invalid character");

                return Reader.AddSymbol(cstr);
            }

            throw Expected("string");
        }
    }

    internal class Func_Assoc : Func_Native
    {
        internal Func_Assoc() : base("assoc") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt > 0 && args.First() is HashMap hm)
            {
                if (cnt % 2 == 0)
                    throw Error("even number of associative arguments is required");
                hm = new HashMap(hm);
                Dictionary<Value, Value> dic = hm.Elements;

                for (int i = 1; i < cnt; i += 2)
                {
                    Value key = args.Nth(i);
                    if (key is Symbol || key is Keyword || key is Str)
                    {
                        dic[key] = args.Nth(i + 1);
                    }
                    else
                        throw Expected("symbol/keyword/string", i + 1);
                }

                return hm;
            }
            else
                throw Expected("hash-map");
        }
    }

    internal class Func_Dissoc : Func_Native
    {
        internal Func_Dissoc() : base("dissoc") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt > 0 && args.First() is HashMap hm)
            {
                hm = new HashMap(hm);
                Dictionary<Value, Value> dic = hm.Elements;

                for (int i = 1; i < cnt; i++)
                {
                    Value key = args.Nth(i);
                    if (key is Symbol || key is Keyword || key is Str)
                    {
                        dic.Remove(key);
                    }
                    else
                        throw Expected("symbol/keyword/string", i + 1);
                }

                return hm;
            }
            else
                throw Expected("hash-map");
        }
    }

    internal class Func_Get : Func_TwoArgs
    {
        internal Func_Get() : base("get") { }

        protected override Value Apply_TwoArgs(Value arg1, Value arg2)
        {
            if (arg1 is HashMap hm)
            {
                if (hm.Elements.TryGetValue(arg2, out Value val))
                    return val;

                return Reader.Nil;
            }
            else if (arg1 == Reader.Nil)
                return Reader.Nil;
            else
                throw Expected("hash-map");
        }
    }

    internal class Func_Contains : Func_TwoArgs
    {
        internal Func_Contains() : base("contains?") { }

        protected override Value Apply_TwoArgs(Value arg1, Value arg2)
        {
            if (arg1 is HashMap hm)
                return Bool(hm.Elements.TryGetValue(arg2, out _));

            if (arg1 is Sequence seq)
                return Bool(seq.FindEQ(arg2) >= 0);

            throw Expected("hash-map/sequence");
        }
    }

    internal class Func_Keys : Func_OneArg
    {
        internal Func_Keys() : base("keys") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is HashMap hm)
            {
                return new List(hm.Elements.Keys);
            }

            throw Expected("hash-map");
        }
    }

    internal class Func_Vals : Func_OneArg
    {
        internal Func_Vals() : base("vals") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is HashMap hm)
            {
                return new List(hm.Elements.Values);
            }

            throw Expected("hash-map");
        }
    }

    internal class Func_Apply : Func_Native
    {
        internal Func_Apply(Eval eval)
            : base("apply")
        {
            this.eval = eval;
        }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            List lst = new List();

            if (args.Nth(cnt - 1) is Sequence seq)
                lst = seq.Drop(0);
            else
                lst = lst.Cons(args.Nth(cnt - 1));

            for (int i = cnt - 2; i > 0; i--)
                lst = lst.Cons(args.Nth(i));

            if (args.Nth(0) is Func_Native fn)
            {
                return fn.Apply(lst);
            }
            else if (args.Nth(0) is Closure cls)
            {
                return eval(cls.Body, cls.CreateEnv(lst));
            }

            throw Expected("function/closure");
        }

        private readonly Eval eval;
    }

    internal class Func_Map : Func_TwoArgs
    {
        internal Func_Map(Eval eval)
            : base("map")
        {
            this.eval = eval;
        }

        protected override Value Apply_TwoArgs(Value arg1, Value arg2)
        {
            if (arg2 is Sequence seq)
            {
                int cnt = seq.Count();
                List<Value> vals = new List<Value>();

                if (arg1 is Func_Native fn)
                {
                    for (int i = 0; i < cnt; i++)
                        vals.Add(fn.Apply(new List(seq.Nth(i))));

                    return new List(vals);
                }
                else if (arg1 is Closure cls)
                {
                    for (int i = 0; i < cnt; i++)
                        vals.Add(eval(cls.Body, cls.CreateEnv(new List(seq.Nth(i)))));

                    return new List(vals);
                }

                throw Expected("function/closure");
            }

            throw Expected("sequence", 2);
        }

        private readonly Eval eval;
    }

    internal class Func_Conj : Func_Native
    {
        internal Func_Conj() : base("conj") { }

        internal override Value Apply(Sequence args)
        {
            int cnt = args.Count();
            if (cnt < 2)
                throw Error_TwoMore();

            if (args.Nth(0) is Vector vec)
            {
                List<Value> vals = new List<Value>(vec.Elements);

                for (int i = 1; i < cnt; i++)
                    vals.Add(args.Nth(i));

                return new Vector(vals);
            }
            else if (args.Nth(0) is List lst)
            {
                for (int i = 1; i < cnt; i++)
                    lst = lst.Cons(args.Nth(i));

                return lst;
            }

            throw Expected("sequence");
        }
    }

    internal class Func_Throw : Func_OneArg
    {
        internal Func_Throw() : base("throw") { }

        protected override Value Apply_OneArg(Value arg)
        {
            throw new MalException(arg);
        }
    }

    internal class Func_Time_ms : Func_Native
    {
        internal Func_Time_ms() : base("time-ms") { }

        internal override Value Apply(Sequence args)
        {
            if (args.Count() != 0)
                throw Error("no argument is expected");

            var span = DateTime.Now.Subtract(new DateTime(1970, 1, 1));

            return new Integer((long)span.TotalMilliseconds);
        }
    }

    internal class Func_Seq : Func_OneArg
    {
        internal Func_Seq() : base("seq") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg == Reader.Nil)
                return Reader.Nil;

            if (arg is Sequence seq && seq.IsEmpty())
                return Reader.Nil;

            if (arg is List)
            {
                return arg;
            }

            if (arg is Vector vec)
                return vec.Drop(0);

            if (arg is Str str)
            {
                if (string.IsNullOrEmpty(str.Value))
                    return Reader.Nil;

                int cnt = str.Value.Length;

                Value[] vals = new Value[cnt];
                for (int i = 0; i < cnt; i++)
                    vals[i] = new Str(str.Value.Substring(i, 1));

                return new List(vals);
            }

            throw Expected("list/vector/string");
        }
    }

    internal class Func_Meta : Func_OneArg
    {
        internal Func_Meta() : base("meta") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is ValueWithMeta meta)
            {
                return meta.Meta ?? Reader.Nil;
            }

            throw Expected("object with meta");
        }
    }

    internal class Func_WithMeta : Func_TwoArgs
    {
        internal Func_WithMeta() : base("with-meta") { }

        protected override Value Apply_TwoArgs(Value arg1, Value arg2)
        {
            if (arg1 is ValueWithMeta meta)
            {
                return meta.Clone(arg2);
            }

            throw Expected("object with meta");
        }
    }

    internal class Func_Readline : Func_OneArg
    {
        internal Func_Readline() : base("readline") { }

        protected override Value Apply_OneArg(Value arg)
        {
            if (arg is Str str)
            {
                string prompt = str.Value;
                if (prompt == null)
                    prompt = "";

                string line = MAL.Readline(prompt);

                return line == null ? (Value)Reader.Nil : new Str(line);
            }

            throw Expected("string");
        }
    }
}
