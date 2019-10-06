using System.Collections.Generic;

namespace Mal
{
    internal class Env
    {
        internal Env()
        {
            table = new Dictionary<string, Value>();
        }

        internal Env(Env outer)
        {
            table = new Dictionary<string, Value>();
            Outer = outer;
        }

        internal Value Get(Symbol sym)
        {
            if (sym == null)
                return null;

            if (table.TryGetValue(sym.Name, out Value val))
                return val;

            return Outer?.Get(sym);
        }

        internal void Set(Symbol sym, Value val)
        {
            if (sym == null || val == null)
                throw new MalException("env - symbol is expeted");

            table[sym.Name] = val;
        }

        internal Env Outer { get; }

        private readonly Dictionary<string, Value> table;
    }
}
