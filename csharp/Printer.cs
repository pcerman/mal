using System;
using System.Collections.Generic;
using System.Text;

namespace Mal
{
    internal class Printer
    {
        internal static string Pr_str(Value val, bool readably)
        {
            if (val == null)
                return null;

            StringBuilder sb = new StringBuilder();
            val.Build_str(sb, readably);
            return sb.ToString();
        }
    }
}
