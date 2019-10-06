using System;
using System.Collections.Generic;
using System.Text;

namespace Mal
{
    internal abstract class Value
    {
        internal abstract void Build_str(StringBuilder sb, bool readably);

        internal abstract int HashCode();

        internal virtual bool IsEqual_To(Value v)
        {
            return this == v;
        }

        internal virtual bool IsEq_To(Value v)
        {
            return this == v;
        }

        internal new string ToString()
        {
            StringBuilder sb = new StringBuilder();
            Build_str(sb, true);
            return sb.ToString();
        }

        internal static bool IsEqual_To(Value v1, Value v2)
        {
            if (v1 == null && v2 == null)
                return true;

            if (v1 == null || v2 == null)
                return false;

            return v1.IsEqual_To(v2);
        }

        internal static bool IsEq_To(Value v1, Value v2)
        {
            if (v1 == null && v2 == null)
                return true;

            if (v1 == null || v2 == null)
                return false;

            return v1.IsEq_To(v2);
        }
    }

    internal abstract class ValueWithMeta : Value
    {
        internal abstract ValueWithMeta Clone(Value meta);

        protected Value meta;

        internal Value Meta { get => meta; }
    }

    internal class Symbol : Value
    {
        internal Symbol(string str)
        {
            Name = str;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            sb.Append(Name);
        }

        internal override int HashCode()
        {
            return Name.GetHashCode();
        }

        internal string Name { get; }
    }

    internal class Keyword : Value
    {
        internal Keyword(string str)
        {
            Name = str;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            sb.Append(Name);
        }

        internal override int HashCode()
        {
            return Name.GetHashCode();
        }

        internal string Name { get; }
    }

    internal class Constant : Value
    {
        internal Constant(string str)
        {
            Name = str;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            sb.Append(Name);
        }

        internal override int HashCode()
        {
            return Name.GetHashCode();
        }

        internal string Name { get; }
    }

    internal class Atom : ValueWithMeta
    {
        internal Atom(Value val)
        {
            Value = val;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            sb.Append("(atom ");
            Value.Build_str(sb, readably);
            sb.Append(")");
        }

        internal override int HashCode()
        {
            throw new NotImplementedException();
        }

        internal override bool IsEqual_To(Value v)
        {
            if (this == v)
                return true;

            if (v is Atom atom)
                return IsEqual_To(Value, atom.Value);

            return false;
        }

        internal override ValueWithMeta Clone(Value meta)
        {
            Atom atm = new Atom(Value)
            {
                meta = meta
            };
            return atm;
        }

        internal Value Value;
    }

    internal class HashMap : ValueWithMeta
    {
        internal HashMap()
        {
            Elements = new Dictionary<Value, Value>(new ValueComparer());
        }
        internal HashMap(HashMap hm)
        {
            Elements = new Dictionary<Value, Value>(hm.Elements, new ValueComparer());
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            bool first = true;

            sb.Append("{");
            foreach (var elm in Elements)
            {
                if (first)
                    first = false;
                else
                    sb.Append(" ");

                elm.Key.Build_str(sb, readably);
                sb.Append(" ");
                elm.Value.Build_str(sb, readably);
            }
            sb.Append("}");
        }

        internal override int HashCode()
        {
            throw new NotImplementedException();
        }

        internal override bool IsEqual_To(Value v)
        {
            if (this == v)
                return true;

            if (v is HashMap map)
            {
                if (Elements == null && map.Elements == null)
                    return true;
                if (Elements == null || map.Elements == null)
                    return false;

                if (Elements.Count != map.Elements.Count)
                    return false;

                foreach (var kv in map.Elements)
                {
                    if (!Elements.TryGetValue(kv.Key, out Value val))
                        return false;

                    if (!IsEqual_To(kv.Value, val))
                        return false;
                }

                return true;
            }

            return false;
        }

        internal bool IsEmpty()
        {
            return Elements == null || Elements.Count == 0;
        }

        internal int Count()
        {
            return Elements == null ? 0 : Elements.Count;
        }

        internal override ValueWithMeta Clone(Value meta)
        {
            HashMap hm = new HashMap(this)
            {
                meta = meta
            };
            return hm;
        }

        internal Dictionary<Value, Value> Elements { get; }
    }

    internal class Str : Value
    {
        internal Str(string str)
        {
            Value = str;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            if (readably)
            {
                sb.Append('"');
                string str = Value.Replace("\\", "\\\\");
                str = str.Replace("\"", "\\\"");
                str = str.Replace("\n", "\\n");
                str = str.Replace("\r", "\\r");
                str = str.Replace("\t", "\\t");
                sb.Append(str);
                sb.Append('"');
            }
            else
                sb.Append(Value);
        }

        internal override int HashCode()
        {
            return Value.GetHashCode();
        }

        internal override bool IsEqual_To(Value v)
        {
            if (this == v)
                return true;

            if (v is Str str)
            {
                return Value == str.Value;
            }

            return false;
        }

        internal override bool IsEq_To(Value v)
        {
            if (this == v)
                return true;

            if (v is Str str)
            {
                return Value == str.Value;
            }

            return false;
        }

        internal bool IsEmpty()
        {
            return Value == null ? true : Value.Length == 0;
        }

        internal int Length()
        {
            return Value == null ? 0 : Value.Length;
        }

        internal string Value { get; }

        internal static Str Empty { get; } = new Str("");
        internal static Str Space { get; } = new Str(" ");
    }

    internal abstract class Func : ValueWithMeta
    {
        protected Func(string name) { FnName = name; }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            sb.Append(string.Format("#<function '{0}'>", FnName));
        }

        internal override int HashCode()
        {
            throw new NotImplementedException();
        }

        protected MalException Error(string msg, params string[] pars)
        {
            return new MalException(FnName + " - " + string.Format(msg, pars));
        }

        internal string FnName { get; }
    }

    internal abstract class Func_Native : Func
    {
        internal delegate Value Eval(Value v, Env e);

        protected Func_Native(string fn_name) : base(fn_name) { }

        protected MalException Error_OneMore()
        {
            return Error("at least one argument is expected");
        }

        protected MalException Error_TwoMore()
        {
            return Error("at least two arguments are expected");
        }

        protected MalException Invalid(int idx = 0)
        {
            if (idx <= 0)
                return Error("invalid argument");

            switch (idx)
            {
                case 1:  return Error("invalid first argument");
                case 2:  return Error("invalid second argument");
                case 3:  return Error("invalid third argument");
            }

            return Error("invalid {0}-th argument", idx.ToString());
        }

        protected MalException Expected(string type, int idx = -1)
        {
            if (idx > 0)
                return Error(type + " is expected at position {0}", idx.ToString());
            else
                return Error(type + " is expected");
        }

        internal override ValueWithMeta Clone(Value meta)
        {
            var obj = MemberwiseClone();
            if (obj is Func_Native fn)
            {
                fn.meta = meta;
                return fn;
            }

            throw new MalException("unable to clone native function");
        }

        internal abstract Value Apply(Sequence args);

        protected static Value Bool(bool val)
        {
            return val ? Reader.True : Reader.False;
        }
    }

    internal class Closure : Func
    {
        internal Closure(Sequence vars, Value body, Env env)
            : base("fn*")
        {
            int amp = vars.FindEQ(Reader.Amp);

            if (amp >= 0)
            {
                if (vars.Count() != amp + 2 || IsEq_To(vars.Nth(amp + 1), Reader.Amp))
                    throw Error("syntax error");

                this.vars = vars.Take(amp);
                var_rest = vars.Nth(amp + 1) as Symbol;
            }
            else
            {
                this.vars = vars;
            }

            this.Body = body;
            this.env = env;

            fun_id = ++ID;
        }

        Closure(Closure cls)
            : base("fn*")
        {
            IsMacro = cls.IsMacro;
            Body = cls.Body;
            vars = cls.vars;
            var_rest = cls.var_rest;
            env = cls.env;

            fun_id = ++ID;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
#if DEBUG
            if (IsMacro)
                sb.Append(string.Format("#<macro {0} (fn* ", fun_id));
            else
                sb.Append(string.Format("#<closure {0} (fn* ", fun_id));

            if (var_rest != null)
            {
                char fc = vars is List ? '(' : '[';
                char lc = vars is List ? ')' : ']';
                int cnt = vars.Count();
                sb.Append(fc);
                for (int i = 0; i < cnt; i++)
                {
                    if (i > 0)
                        sb.Append(' ');
                    vars.Nth(i).Build_str(sb, readably);
                }

                sb.Append(cnt > 0 ? " & " : "& ");
                var_rest.Build_str(sb, readably);
                sb.Append(lc);
            }
            else
                vars.Build_str(sb, readably);
            sb.Append(" ");
            Body.Build_str(sb, readably);
            sb.Append(")>");
#else
            if (IsMacro)
                sb.Append(string.Format("#<macro {0}>", fun_id));
            else
                sb.Append(string.Format("#<closure {0}>", fun_id));
#endif
        }

        internal override int HashCode()
        {
            throw new NotImplementedException();
        }

        internal Env CreateEnv(Sequence args)
        {
            int vcnt = vars.Count();
            int acnt = args.Count();

            if (vcnt > acnt || var_rest == null && vcnt != acnt)
                throw new MalException("apply - wrong number of arguments");

            Env newEnv = new Env(env);

            for (int i = 0; i < vcnt; i++)
                newEnv.Set(vars.Nth(i) as Symbol, args.Nth(i));

            if (var_rest != null)
                newEnv.Set(var_rest, args.Drop(vcnt));

            return newEnv;
        }

        internal override ValueWithMeta Clone(Value meta)
        {
            Closure cls = new Closure(this)
            {
                meta = meta
            };
            return cls;
        }

        internal bool IsMacro = false;

        internal Value Body { get; }

        private readonly Sequence vars;
        private readonly Symbol var_rest;
        private readonly Env env;

        private readonly int fun_id = 0;

        private static int ID = 0;
    }

    class ValueComparer : IEqualityComparer<Value>
    {
        public bool Equals(Value x, Value y)
        {
            return Value.IsEqual_To(x, y);
        }

        public int GetHashCode(Value obj)
        {
            return obj.HashCode();
        }
    }
}
