using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Mal
{
    internal struct Position
    {
        public int Row;
        public int Col;
    }

    internal class MalException : Exception
    {
        internal MalException(string msg, Position pos)
            : base(msg)
        {
            Position = pos;
        }

        internal MalException(string msg)
            : base(msg)
        {
            Position = new Position() {
                Row = -1,
                Col = -1
            };
        }

        internal MalException(Value val)
            :base("try/catch/throw")
        {
            Val = val;
        }

        internal new string Message
        {
            get
            {
                if (Val != null)
                {
                    StringBuilder sb = new StringBuilder();
                    Val.Build_str(sb, true);
                    return sb.ToString();
                }

                if (Position.Col >= 0 && Position.Row >= 0)
                    return string.Format("{0} at position [{1}, {2}]", base.Message, Position.Row, Position.Col);

                return base.Message;
            }
        }

        internal Position Position { get; }
        internal Value Val { get; }
    }

    internal class Reader
    {
        static Reader()
        {
            Symbols = new Dictionary<string, Value>
            {
                { Nil.Name, Nil },
                { True.Name, True },
                { False.Name, False },
                { Quote.Name, Quote },
                { Quasiquote.Name, Quasiquote },
                { Unquote.Name, Unquote },
                { Splice_unquote.Name, Splice_unquote },
                { Deref.Name, Deref },
                { With_meta.Name, With_meta },
                { Def.Name, Def },
                { Let.Name, Let },
                { Fn.Name, Fn },
                { Amp.Name, Amp },
                { If.Name, If },
                { Do.Name, Do },
                { Load_file.Name, Load_file },
                { Defmacro.Name, Defmacro },
                { Macroexpand.Name, Macroexpand },
                { Try.Name, Try },
                { Catch.Name, Catch }
            };
        }

        internal Reader(Stream stream)
        {
            reader = new StreamReader(stream);
            pos = new Position();
        }

        internal Reader(string str)
        {
            reader = new StringReader(str);
            pos = new Position();
        }

        internal static Value Read_str(string str)
        {
            Reader rd = new Reader(str);

            Value val = rd.Read_form();

            Value nonexpected;

            try
            {
                nonexpected = rd.Read_form();
            }
            catch (MalException)
            {
                nonexpected = False;
            }

            if (nonexpected != null)
                throw new MalException("reader - unexpected characters after expression");

            return val;
        }

        internal void Set(string str)
        {
            reader = new StringReader(str);
            pos.Row++;
            pos.Col = 0;
        }

        #region reading AST

        internal Value Read_form()
        {
            string token = Next_token();
            if (string.IsNullOrEmpty(token))
                return null;

            switch (token[0])
            {
                case '(': return Read_List();
                case '[': return Read_Vector();
                case '{': return Read_HashMap();

                case '~':
                    {
                        Value fn = token == "~@" ? Splice_unquote : Unquote;

                        Value val = Read_form();
                        if (val == null)
                            throw new MalException(token + " - missing value", pos);

                        List<Value> lst = new List<Value> {
                            fn,
                            val
                        };

                        return new List(lst);
                    }

                case '@':
                    {
                        Value val = Read_form();
                        if (val == null)
                            throw new MalException("@ - missing value", pos);

                        List<Value> lst = new List<Value> {
                            Deref,
                            val
                        };

                        return new List(lst);
                    }

                case '^':
                    {
                        Value val1 = Read_form();
                        if (val1 == null)
                            throw new MalException("^ - missing value", pos);

                        Value val2 = Read_form();
                        if (val2 == null)
                            throw new MalException("^ - missing value", pos);

                        List<Value> lst = new List<Value> {
                            With_meta,
                            val2,
                            val1
                        };

                        return new List(lst);
                    }

                case '\'':
                    {
                        Value val = Read_form();
                        if (val == null)
                            throw new MalException("quote - missing value", pos);

                        List<Value> lst = new List<Value> {
                            Quote,
                            val
                        };

                        return new List(lst);
                    }

                case '`':
                    {
                        Value val = Read_form();
                        if (val == null)
                            throw new MalException("quasiquote - missing value", pos);

                        List<Value> lst = new List<Value> {
                            Quasiquote,
                            val
                        };

                        return new List(lst);
                    }

                case '"':
                    token = token.Substring(1, token.Length - 2);
                    token = token.Replace("\\\\", "\u0001");
                    token = token.Replace("\\\"", "\"");
                    token = token.Replace("\\n", "\n");
                    token = token.Replace("\\r", "\r");
                    token = token.Replace("\\t", "\t");
                    token = token.Replace("\u0001", "\\");
                    return new Str(token);

                default:
                    if (IsSpecialChar(token[0]))
                        throw new MalException(string.Format("unexpected character '{0}'", token[0]));

                    if (IsInteger(token))
                        return new Integer(long.Parse(token));
                    else if (IsReal(token))
                        return new Real(double.Parse(token));
                    else
                    {
                        Value val = AddSymbol(token);

                        if (val == null)
                            throw new MalException(string.Format("invalid token '{0}'", token), pos_token);

                        return val;
                    }
            }
        }

        Value Read_List()
        {
            List<Value> elms = new List<Value>();

            for (string token = Next_token(); ; token = Next_token())
            {
                if (token == null)
                    throw new MalException("unbalanced list", pos);

                if (token == ")")
                    break;

                if (token == "]" || token == "}")
                    throw new MalException("unbalanced list", pos);

                Unget_token(token);

                Value val = Read_form();
                elms.Add(val);
            }

            return new List(elms);
        }

        Value Read_Vector()
        {
            List<Value> elems = new List<Value>();

            for (string token = Next_token(); ; token = Next_token())
            {
                if (token == null)
                    throw new MalException("unbalanced vector", pos);

                if (token == "]")
                    break;

                if (token == ")" || token == "}")
                    throw new MalException("unbalanced vector", pos);

                Unget_token(token);

                Value val = Read_form();
                elems.Add(val);
            }

            return new Vector(elems);
        }

        Value Read_HashMap()
        {
            HashMap hm = new HashMap();
            Dictionary<Value, Value> elms = hm.Elements;

            Value key = null;

            for (string token = Next_token(); ; token = Next_token())
            {
                if (token == null || token == ")" || token == "]" || token == "}" && key != null)
                    throw new MalException("unbalanced hash-map", pos);

                if (token == "}")
                    break;

                Unget_token(token);

                if (key == null)
                {
                    key = Read_form();
                    if (!(key is Symbol || key is Keyword || key is Str))
                        throw new MalException("hash-map - invalid type for key", pos);
                }
                else
                {
                    elms[key] = Read_form();
                    key = null;
                }
            }

            return hm;
        }

        static internal Value AddSymbol(string token)
        {
            if (Symbols.TryGetValue(token, out Value val))
                return val;

            if (token[0] == ':')
            {
                val = new Keyword(token);
            }
            else
                val = new Symbol(token);

            if (val != null)
                Symbols.Add(token, val);

            return val;
        }

        static internal Dictionary<string, Value> Symbols { get; private set; }

        #endregion

        #region tokens predicates

        static bool IsInteger(string token)
        {
            bool has_digit = false;

            int i = IsSignChar(token[0]) ? 1 : 0;
            for (; i < token.Length; i++)
            {
                if (!IsDigitChar(token[i]))
                    return false;
                has_digit = true;
            }

            return has_digit;
        }

        enum State { beg, dig1, dot, dot1, dig2, exp, sign, dig3 };

        static bool IsReal(string token)
        {
            int i = IsSignChar(token[0]) ? 1 : 0;

            State state = State.beg;

            for (; i < token.Length; i++)
            {
                char ch = token[i];
                switch (ch)
                {
                    case '.':
                        switch (state)
                        {
                            case State.beg:  state = State.dot;  break;
                            case State.dig1: state = State.dot1; break;
                            default: return false;
                        }
                        break;

                    case 'e':
                    case 'E':
                        switch (state)
                        {
                            case State.dig1:
                            case State.dig2:
                            case State.dot1: state = State.exp; break;
                            default: return false;
                        }
                        break;

                    case '+':
                    case '-':
                        switch (state)
                        {
                            case State.exp: state = State.sign; break;
                            default: return false;
                        }
                        break;

                    default:
                        if (IsDigitChar(ch))
                        {
                            switch (state)
                            {
                                case State.dig1:
                                case State.dig2:
                                case State.dig3: break;

                                case State.beg:  state = State.dig1; break;

                                case State.dot:
                                case State.dot1: state = State.dig2; break;

                                case State.exp:
                                case State.sign: state = State.dig3; break;

                                default: return false;
                            }
                        }
                        else
                            return false;
                        break;
                }
            }

            switch (state)
            {
                case State.dig1:
                case State.dig2:
                case State.dig3:
                case State.dot1:
                    return true;
            }
            return false;
        }

        #endregion

        #region stream of tokens

        void Unget_token(string token)
        {
            if (!string.IsNullOrEmpty(token))
            {
                if (cur_token == null)
                    cur_token = new Stack<string>();

                cur_token.Push(token);
            }
        }

        string Next_token()
        {
            string token = cur_token != null && cur_token.Count > 0 ? cur_token.Pop() : null;
            if (token != null)
                return token;

            bool in_comment = false;

            for (; ; Next())
            {
                int data = Peek();
                if (data < 0)
                    return null;

                char ch = (char)data;

                if (ch == '\n')
                {
                    in_comment = false;
                    continue;
                }

                if (in_comment)
                    continue;

                if (ch == ';')
                {
                    in_comment = true;
                    continue;
                }

                if (IsWhiteSpaceChar(ch))
                    continue;

                pos_token = pos;

                if (ch == '~')
                {
                    Next();
                    if (Peek() == '@')
                    {
                        Next();
                        return "~@";
                    }

                    return "~";
                }

                if (ch == '"')
                    return Get_str();

                if (IsSpecialChar(ch))
                {
                    Next();
                    return new string(ch, 1);
                }

                if (IsNonAtomChar(ch))
                {
                    throw new MalException(string.Format("unknown character: '{0}'", ch), pos_token);
                }

                return Get_atom();
            }
        }

        string Get_str()
        {
            StringBuilder sb = new StringBuilder();

            sb.Append('"');

            bool escape = false;
            for (Next(); ; Next())
            {
                int ch = Peek();
                if (ch <= 0)
                {
                    throw new MalException("unbalanced string - '\"' is expected", pos);
                }

                if (escape)
                {
                    escape = false;
                    sb.Append((char)ch);
                    continue;
                }

                if (ch == '"')
                {
                    sb.Append((char)Next());
                    return sb.ToString();
                }

                if (ch == '\\')
                {
                    escape = true;
                    sb.Append((char)ch);
                    continue;
                }

                sb.Append((char)ch);
            }
        }

        string Get_atom()
        {
            StringBuilder sb = new StringBuilder();

            for (; ; Next())
            {
                int ch = Peek();
                if (ch < 0 || IsNonAtomChar((char)ch))
                    return sb.ToString();

                sb.Append((char)ch);
            }
        }

        static bool IsWhiteSpaceChar(char ch)
        {
            return ch <= ' ' || ch == ',';
        }

        static bool IsSpecialChar(char ch)
        {
            return "()[]{}'`~^@".Contains(ch);
        }

        internal static bool IsNonAtomChar(char ch)
        {
            return IsWhiteSpaceChar(ch)
                || "()[]{}'`;\"".Contains(ch);
        }

        static bool IsDigitChar(char ch)
        {
            return char.IsDigit(ch);
        }

        static bool IsSignChar(char ch)
        {
            return ch == '+' || ch == '-';
        }

        private Stack<string> cur_token;
        private Position pos_token;

        #endregion

        #region stream of characters

        int Peek()
        {
            return reader.Peek();
        }

        int Next()
        {
            int ch = reader.Read();

            if (ch == '\r' || ch == '\n')
            {
                if (ch == '\r')
                {
                    if (reader.Peek() == '\n')
                        reader.Read();
                    ch = '\n';
                }

                pos.Row++;
                pos.Col = 0;
            }
            else
                pos.Col++;

            return ch;
        }

        private TextReader reader;
        private Position pos;

        #endregion

        internal static Constant Nil { get; } = new Constant("nil");
        internal static Constant True { get; } = new Constant("true");
        internal static Constant False { get; } = new Constant("false");
        internal static Constant Quote { get; } = new Constant("quote");
        internal static Constant Quasiquote { get; } = new Constant("quasiquote");
        internal static Constant Unquote { get; } = new Constant("unquote");
        internal static Constant Splice_unquote { get; } = new Constant("splice-unquote");
        internal static Symbol Deref { get; } = new Symbol("deref");
        internal static Symbol With_meta { get; } = new Symbol("with-meta");
        internal static Constant Def { get; } = new Constant("def!");
        internal static Constant Let { get; } = new Constant("let*");
        internal static Constant Fn { get; } = new Constant("fn*");
        internal static Symbol Amp { get; } = new Symbol("&");
        internal static Constant If { get; } = new Constant("if");
        internal static Constant Do { get; } = new Constant("do");
        internal static Symbol Load_file { get; } = new Symbol("load-file");
        internal static Constant Defmacro { get; } = new Constant("defmacro!");
        internal static Constant Macroexpand { get; } = new Constant("macroexpand");
        internal static Constant Try { get; } = new Constant("try*");
        internal static Constant Catch { get; } = new Constant("catch*");
    }
}
