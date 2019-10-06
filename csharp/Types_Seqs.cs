using System;
using System.Collections.Generic;
using System.Text;

namespace Mal
{
    internal abstract class Sequence : ValueWithMeta
    {
        internal abstract bool IsEmpty();
        internal abstract int Count();
        internal abstract Value First();
        internal abstract List Rest();
        internal abstract Value Nth(int n);
        internal abstract List Take(int n);
        internal abstract List Drop(int n);
        internal abstract List Cons(Value v);
        internal abstract int FindEQ(Value v);
    }

    internal class List : Sequence
    {
        internal class Node
        {
            internal Value car;
            internal Node cdr;
        }

        internal List(ICollection<Value> elms)
        {
            if (elms == null || elms.Count == 0)
                Elements = null;
            else
            {
                Node last = null;

                foreach (Value elm in elms)
                {
                    if (last == null)
                    {
                        Elements = new Node() { car = elm, cdr = null };
                        last = Elements;
                    }
                    else
                    {
                        last.cdr = new Node() { car = elm, cdr = null };
                        last = last.cdr;
                    }
                }
            }
        }

        internal List(params Value[] elms)
        {
            if (elms == null || elms.Length == 0)
                Elements = null;
            else
            {
                Node last = null;

                foreach (Value elm in elms)
                {
                    if (last == null)
                    {
                        Elements = new Node() { car = elm, cdr = null };
                        last = Elements;
                    }
                    else
                    {
                        last.cdr = new Node() { car = elm, cdr = null };
                        last = last.cdr;
                    }
                }
            }
        }

        internal List(Node elms = null)
        {
            Elements = elms;
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            bool first = true;

            sb.Append("(");
            for (Node elm = Elements; elm != null; elm = elm.cdr)
            {
                if (first)
                    first = false;
                else
                    sb.Append(" ");

                elm.car.Build_str(sb, readably);
            }
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

            if (v is List lst)
            {
                if (Elements == lst.Elements)
                    return true;

                Node e1 = Elements;
                Node e2 = lst.Elements;
                while (e1 != null && e2 != null)
                {
                    if (!IsEqual_To(e1.car, e2.car))
                        return false;
                    e1 = e1.cdr;
                    e2 = e2.cdr;
                }

                return e1 == null && e2 == null;
            }
            else if (v is Vector vec)
            {
                if (Elements == null && (vec.Elements == null || vec.Elements.Count == 0))
                    return true;
                if (Elements == null || (vec.Elements == null || vec.Elements.Count == 0))
                    return false;

                Node e1 = Elements;
                IEnumerator<Value> it2 = vec.Elements.GetEnumerator();

                while (it2.MoveNext())
                {
                    if (e1 == null)
                        return false;

                    if (!IsEqual_To(e1.car, it2.Current))
                        return false;
                    e1 = e1.cdr;
                }

                return e1 == null;
            }
            else
                return false;
        }

        internal override bool IsEq_To(Value v)
        {
            if (this == v)
                return true;

            if (v is List lst)
            {
                return Elements == lst.Elements;
            }

            return false;
        }

        internal bool IsPair()
        {
            return Elements != null;
        }

        internal override bool IsEmpty()
        {
            return Elements == null;
        }

        internal override int Count()
        {
            int cnt = 0;
            for (Node elm = Elements; elm != null; elm = elm.cdr)
                cnt++;
            return cnt;
        }

        internal override Value First()
        {
            if (Elements == null)
                return Reader.Nil;

            return Elements.car;
        }

        internal override List Rest()
        {
            if (Elements == null)
                return this;

            return new List(Elements.cdr);
        }

        internal override Value Nth(int n)
        {
            for (Node elm = Elements; n >= 0 && elm != null; elm = elm.cdr, n--)
            {
                if (n == 0)
                    return elm.car;
            }

            throw new MalException("nth - index out of range");
        }

        internal override List Take(int n)
        {
            if (n <= 0 || Elements == null)
                return new List();

            Node first = new Node() { car = Elements.car, cdr = null };
            Node last = first;

            for (Node elm = Elements.cdr; elm != null; elm = elm.cdr)
            {
                if (--n == 0)
                    break;

                last.cdr = new Node() { car = elm.car, cdr = null };
                last = last.cdr;
            }

            return new List(first);
        }

        internal override List Drop(int n)
        {
            if (n <= 0 || Elements == null)
                return this;

            Node elm = Elements;

            for (; elm != null; elm = elm.cdr)
            {
                if (n-- == 0)
                    break;
            }

            return new List(elm);
        }

        internal override List Cons(Value val)
        {
            return new List(new Node() { car = val, cdr = Elements });
        }

        internal override int FindEQ(Value v)
        {
            if (Elements == null)
                return -1;

            int idx = 0;
            for (Node elm = Elements; elm != null; elm = elm.cdr)
            {
                if (IsEq_To(elm.car, v))
                    return idx;
                idx++;
            }

            return -1;
        }

        internal List Reverse()
        {
            Node first = null;

            for (Node elm = Elements; elm != null; elm = elm.cdr)
                first = new Node() { car = elm.car, cdr = first };

            return new List(first);
        }

        internal static List Append_D(List lst1, List lst2)
        {
            if (lst1 == null || lst1.Elements == null)
                return lst2;

            if (lst2 == null || lst2.Elements == null)
                return lst1;

            for (Node elm = lst1.Elements; elm != null; elm = elm.cdr)
            {
                if (elm.cdr == null)
                {
                    elm.cdr = lst2.Elements;
                    break;
                }
            }

            return lst1;
        }

        internal static List Append(List lst1, List lst2)
        {
            if (lst1 == null || lst1.Elements == null)
                return lst2;

            Node first = new Node() { car = lst1.Elements.car, cdr = null };
            Node last = first;

            for (Node elm = lst1.Elements.cdr; elm != null; elm = elm.cdr)
            {
                last.cdr = new Node() { car = elm.car, cdr = null };
                last = last.cdr;
            }

            last.cdr = lst2.Elements;

            return new List(first);
        }

        internal override ValueWithMeta Clone(Value meta)
        {
            List lst = new List(Elements)
            {
                meta = meta
            };
            return lst;
        }

        internal Node Elements { get; }
    }

    internal class Vector : Sequence
    {
        internal Vector(List<Value> elms)
        {
            Elements = elms;
        }

        internal Vector(List elms)
        {
            if (elms == null || elms.IsEmpty())
                Elements = new List<Value>();
            else
            {
                int cnt = elms.Count();
                Elements = new List<Value>(cnt + 1);

                for (List.Node elm = elms.Elements; elm != null; elm = elm.cdr)
                {
                    Elements.Add(elm.car);
                }
            }
        }

        internal override void Build_str(StringBuilder sb, bool readably)
        {
            bool first = true;

            sb.Append("[");
            foreach (Value elm in Elements)
            {
                if (first)
                    first = false;
                else
                    sb.Append(" ");

                elm.Build_str(sb, readably);
            }
            sb.Append("]");
        }

        internal override int HashCode()
        {
            throw new NotImplementedException();
        }

        internal override bool IsEqual_To(Value v)
        {
            if (this == v)
                return true;

            if (v is List lst)
            {
                return lst.IsEqual_To(this);
            }
            else if (v is Vector vec)
            {
                if (Elements == null && vec.Elements == null)
                    return true;
                if (Elements == null || vec.Elements == null)
                    return false;

                if (Elements.Count != vec.Elements.Count)
                    return false;

                IEnumerator<Value> it1 = Elements.GetEnumerator();
                IEnumerator<Value> it2 = vec.Elements.GetEnumerator();

                while (it1.MoveNext() && it2.MoveNext())
                {
                    if (!IsEqual_To(it1.Current, it2.Current))
                        return false;
                }

                return true;
            }

            return false;
        }

        internal override bool IsEmpty()
        {
            return Elements == null || Elements.Count == 0;
        }

        internal override int Count()
        {
            return Elements != null ? Elements.Count : 0;
        }

        internal override Value First()
        {
            if (Elements == null || Elements.Count == 0)
                return Reader.Nil;
            return Elements[0];
        }

        internal override List Rest()
        {
            if (Elements == null || Elements.Count < 2)
                return new List(null as List.Node);

            List.Node first = new List.Node() {
                car = Elements[1],
                cdr = null
            };

            List.Node last = first;

            for (int i=2; i < Elements.Count; i++)
            {
                last.cdr = new List.Node() {
                    car = Elements[i],
                    cdr = null
                };

                last = last.cdr;
            }

            return new List(first);
        }

        internal override Value Nth(int n)
        {
            if (Elements == null || n < 0 || n >= Elements.Count)
                throw new MalException("nth - index out of range");

            return Elements[n];
        }

        internal override List Take(int n)
        {
            if (n <= 0 || Elements == null || Elements.Count == 0)
                return new List();

            List.Node first = new List.Node() { car = Elements[0], cdr = null };
            List.Node last = first;

            int cnt = Math.Min(n, Elements.Count);
            for (int i=1; i < cnt; i++)
            {
                last.cdr = new List.Node() { car = Elements[i], cdr = null };
                last = last.cdr;
            }

            return new List(first);
        }

        internal override List Drop(int n)
        {
            if (Elements == null || Elements.Count <= n)
                return new List();

            if (n < 0)
                n = 0;

            List.Node first = new List.Node() { car = Elements[n], cdr = null };
            List.Node last = first;

            int cnt = Elements.Count;
            for (int i = n + 1; i < cnt; i++)
            {
                last.cdr = new List.Node() { car = Elements[i], cdr = null };
                last = last.cdr;
            }

            return new List(first);
        }

        internal override List Cons(Value val)
        {
            return Drop(0).Cons(val);
        }

        internal override int FindEQ(Value v)
        {
            if (Elements == null || Elements.Count == 0)
                return -1;

            for (int i=0; i < Elements.Count; i++)
            {
                if (IsEq_To(Elements[i], v))
                    return i;
            }

            return -1;
        }

        internal override ValueWithMeta Clone(Value meta)
        {
            Vector vec = new Vector(Elements)
            {
                meta = meta
            };
            return vec;
        }

        internal List<Value> Elements { get; }
    }
}
