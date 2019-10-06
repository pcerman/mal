using System;
using System.Collections.Generic;
using System.Text;

namespace Mal
{
    internal abstract class Number : Value
    {
        internal override bool IsEqual_To(Value v)
        {
            if (this == v)
                return true;

            if (this is Integer int1)
            {
                if (v is Integer int2)
                    return int1.Num == int2.Num;

                if (v is Real r2)
                    return int1.Num == r2.Num;
            }
            else if (this is Real r1)
            {
                if (v is Integer int2)
                    return r1.Num == int2.Num;

                if (v is Real r2)
                    return r1.Num == r2.Num;
            }

            return false;
        }

        internal override bool IsEq_To(Value v)
        {
            if (this == v)
                return true;

            if (this is Integer int1 && v is Integer int2)
                return int1.Num == int2.Num;

            if (this is Real r1 && v is Real r2)
                return r1.Num == r2.Num;

            return false;
        }

        internal abstract double Dbl { get; }
        internal abstract long Int { get; }

        public static Number operator +(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return new Real(n1.Dbl + n2.Dbl);
            }
            else
            {
                return new Integer(n1.Int + n2.Int);
            }
        }

        public static Number operator -(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return new Real(n1.Dbl - n2.Dbl);
            }
            else
            {
                return new Integer(n1.Int - n2.Int);
            }
        }

        public static Number operator *(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return new Real(n1.Dbl * n2.Dbl);
            }
            else
            {
                return new Integer(n1.Int * n2.Int);
            }
        }

        public static Number operator /(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return new Real(n1.Dbl / n2.Dbl);
            }
            else
            {
                return new Integer(n1.Int / n2.Int);
            }
        }

        public static Value operator <(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return n1.Dbl < n2.Dbl ? Reader.True : Reader.False;
            }
            else
            {
                return n1.Int < n2.Int ? Reader.True : Reader.False;
            }
        }

        public static Value operator >(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return n1.Dbl > n2.Dbl ? Reader.True : Reader.False;
            }
            else
            {
                return n1.Int > n2.Int ? Reader.True : Reader.False;
            }
        }

        public static Value operator <=(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return n1.Dbl <= n2.Dbl ? Reader.True : Reader.False;
            }
            else
            {
                return n1.Int <= n2.Int ? Reader.True : Reader.False;
            }
        }

        public static Value operator >=(Number n1, Number n2)
        {
            if (n1 is Real || n2 is Real)
            {
                return n1.Dbl >= n2.Dbl ? Reader.True : Reader.False;
            }
            else
            {
                return n1.Int >= n2.Int ? Reader.True : Reader.False;
            }
        }
    }

    internal class Integer : Number
    {
        internal Integer(long val)
        {
            Num = val;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            sb.Append(Num);
        }

        internal override int HashCode()
        {
            return Num.GetHashCode();
        }

        internal override double Dbl => Num;

        internal override long Int => Num;

        internal long Num { get; }

        internal static Integer Zero { get; } = new Integer(0);
        internal static Integer One { get; } = new Integer(1);
    }

    internal class Real : Number
    {
        internal Real(double val)
        {
            Num = val;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            string str = Num.ToString();

            sb.Append(str);
            if (str.IndexOfAny(new[] { '.', 'E', 'e' }) < 0)
                sb.Append('.');
        }

        internal override int HashCode()
        {
            return Num.GetHashCode();
        }

        internal override double Dbl => Num;

        internal override long Int => (long)Num;

        internal double Num { get; }

        internal static Real Zero { get; } = new Real(0);
        internal static Real One { get; } = new Real(1);
    }
}
