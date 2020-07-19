#include <ostream>
#include <sstream>
#include <exception>
#include <cstdarg>
#include <algorithm>

#include "types.h"
#include "reader.h"
#include "gc.h"

namespace MAL
{

//----------------------------------------------------------------------

char buffer[1040];
const int buffer_size = sizeof(buffer);

//----------------------------------------------------------------------

const std::string & MalException::get_msg()
{
   if (_msg.empty() && _val != nullptr)
   {
      std::ostringstream os;
      _val->pr_str(os, true);
      _msg = os.str();
   }

   return _msg;
}

Value * MalException::get_value()
{
   if (_val == nullptr)
      _val = String::create(_msg);

   return _val;
}

//----------------------------------------------------------------------

void error(int idx, const char * fun, const char * msg ...)
{
   if (fun != nullptr && *fun != 0)
   {
      if (idx > 0)
      {
         const char * p = "th";

         if (idx < 10 || idx > 13)
         {
            switch (idx % 10)
            {
               case 1: p = "st"; break;
               case 2: p = "nd"; break;
               case 3: p = "rd"; break;
            }
         }

         snprintf(buffer, buffer_size, "%s - %d%s argument - ", fun, idx, p);
      }
      else
         snprintf(buffer, buffer_size, "%s - ", fun);
   }
   else
      buffer[0] = 0;

   int buf_len = (int)strlen(buffer);

   char * buf = buffer + buf_len;
   buf_len = buffer_size - 1 - buf_len;

   va_list args;
   va_start (args, msg);

   vsnprintf(buf, buf_len, msg, args);

   va_end(args);

   throw MalException(buffer);
}

void error(int idx, Value * sym, const char * msg ...)
{
   const char * fun = nullptr;

   if (Value::is_type(sym, Type::Symbol))
      fun = R<Symbol>(sym).get_name().c_str();
   else if (Value::is_type(sym, Type::Keyword))
      fun = R<Keyword>(sym).get_name().c_str();

   if (fun != nullptr && *fun != 0)
   {
      if (idx > 0)
      {
         const char * p = "th";

         if (idx < 10 || idx > 13)
         {
            switch (idx % 10)
            {
               case 1: p = "st"; break;
               case 2: p = "nd"; break;
               case 3: p = "rd"; break;
            }
         }

         snprintf(buffer, buffer_size, "%s - %d%s argument - ", fun, idx, p);
      }
      else
         snprintf(buffer, buffer_size, "%s - ", fun);
   }
   else
      buffer[0] = 0;

   int buf_len = (int)strlen(buffer);

   char * buf = buffer + buf_len;
   buf_len = buffer_size - 1 - buf_len;

   va_list args;
   va_start (args, msg);

   vsnprintf(buf, buf_len, msg, args);

   va_end(args);

   throw MalException(buffer);
}


void error(reader & rdr, const char * msg ...)
{
   if (rdr.get_row() == 0)
      snprintf(buffer, buffer_size, "read at %d - ", rdr.get_col());
   else
      snprintf(buffer, buffer_size, "read at %d:%d - ", rdr.get_row(), rdr.get_col());

   int cnt = (int)strlen(buffer);

   char * buf = buffer + cnt;
   int buf_len = buffer_size - 1 - cnt;

   va_list args;
   va_start (args, msg);

   vsnprintf(buf, buf_len, msg, args);

   va_end(args);

   throw MalException(buffer);
}

void error_expected(int idx, const char * type, const char * fun)
{
   error(idx, fun, "%s is expected", type);
}

void error_expected(int idx, const char * type, Value * sym)
{
   const char * fun = nullptr;

   if (Value::is_type(sym, Type::Symbol))
      fun = R<Symbol>(sym).get_name().c_str();
   else if (Value::is_type(sym, Type::Keyword))
      fun = R<Keyword>(sym).get_name().c_str();

   error_expected(idx, type, fun);
}

//----------------------------------------------------------------------

bool Value::equals(Value * val_a, Value * val_b)
{
   if (val_a == val_b)
      return true;

   if (val_a == nullptr || val_b == nullptr)
      return false;

   if (!is_type(val_a, val_b->get_type()))
   {
      if (!is_sequence(val_a) || !is_sequence(val_b))
         return false;
   }

   return val_a->equals_to(val_b);
}

bool Value::is_number(Value * arg)
{
   if (arg == nullptr)
      return false;

   Type type = arg->get_type();

   return type == Type::Int || type == Type::Real;
}

bool Value::is_sequence(Value * arg)
{
   if (arg == nullptr)
      return false;

   Type type = arg->get_type();

   return type == Type::List || type == Type::Vector;
}

bool Value::is_type(Value * arg, Type type)
{
   if (arg == nullptr)
      return false;

   return arg->get_type() == type;
}

// -1   =>    val_a < val_b
//  0   =>    val_a = val_b
//  1   =>    val_a > val_b

int Value::compare(Value * val_a, Value * val_b)
{
   if (val_a == nullptr)
   {
      return val_b == nullptr ? 0 : -1;
   }

   if (val_b == nullptr)
      return 1;

   Type typ_a = val_a->get_type();
   Type typ_b = val_b->get_type();

   if (typ_a < typ_b)
      return -1;

   if (typ_a > typ_b)
      return 1;

   switch (typ_a)
   {
      case Type::Symbol:
        return ((Symbol *)val_a)->compare(*(Symbol *)val_b);

      case Type::Keyword:
       return ((Keyword *)val_a)->compare(*(Keyword *)val_b);

      case Type::Int:
           return ((Int *)val_a)->compare(*(Int *)val_b);

      case Type::Real:
          return ((Real *)val_a)->compare(*(Real *)val_b);

      case Type::String:
        return ((String *)val_a)->compare(*(String *)val_b);

      default:
         error(0, "", "values are not comparable");
         break;
   }

   return 0;
}

void Value::move_object_(Value ** p_val)
{
   if (p_val != nullptr)
   {
      Value * val = *p_val;

      if (GC::is_in_pool_B(val))
      {
         if (Value::is_type(val, Type::Reference))
         {
            *p_val = ((Reference *)val)->get();
         }
         else
         {
            Value * val2 = val->gc_clone();
            *p_val = val2;

            Reference ref(val2);

            memmove(val, &ref, sizeof(ref));
         }
      }
   }
}

//----------------------------------------------------------------------

void * GCValue::operator new (std::size_t size)
{
   return GC::malloc((int)size);
}

void GCValue::operator delete(void *)
{
   error(0, "", "GCValue free - access violation");
}

//----------------------------------------------------------------------

void Reference::pr_str(std::ostream & os, bool readably)
{
   os << "<ref " << (void *)_ptr << '>';
}

bool Reference::equals_to(Value * val) const
{
   if (!is_type(val, Type::Reference))
      return false;
   return _ptr == ((Reference *)val)->_ptr;
}

//----------------------------------------------------------------------

void Symbol::pr_str(std::ostream & os, bool readably)
{
   os << _name;
}

bool Symbol::equals_to(Value * val) const
{
   if (!is_type(val, Type::Symbol))
      return false;
   return _name == ((Symbol *)val)->_name;
}

int Symbol::compare(const Symbol & val) const
{
   return _name.compare(val._name);
}

Symbol * Symbol::create(const char * name)
{
   if (strlen(name) > 1024)
      error(0, "", "too much long name for symbol");

   auto pos = _symbols.find(name);

   if (pos != _symbols.end())
      return pos->second.get();

   Symbol * sym = new Symbol(name);

   _symbols.emplace(sym->_name, sym);

   return sym;
}

bool Symbol::is_symbol(const char * name, Symbol ** sym)
{
   auto it = _symbols.find(name);
   if (it == _symbols.end())
      return false;

   if (sym != nullptr)
      *sym = it->second.get();

   return true;
}

Symbol::Table Symbol::_symbols;

Symbol * const Symbol::Defmacro    = Symbol::create("defmacro!");
Symbol * const Symbol::Macroexpand = Symbol::create("macroexpand");

Symbol * const Symbol::Def = Symbol::create("def!");
Symbol * const Symbol::Let = Symbol::create("let*");
Symbol * const Symbol::If  = Symbol::create("if");
Symbol * const Symbol::Fn  = Symbol::create("fn*");
Symbol * const Symbol::Do  = Symbol::create("do");
Symbol * const Symbol::Amp = Symbol::create("&");

Symbol * const Symbol::quote          = Symbol::create("quote");
Symbol * const Symbol::quasiquote     = Symbol::create("quasiquote");
Symbol * const Symbol::unquote        = Symbol::create("unquote");
Symbol * const Symbol::splice_unquote = Symbol::create("splice-unquote");
Symbol * const Symbol::deref          = Symbol::create("deref");
Symbol * const Symbol::with_meta      = Symbol::create("with-meta");

Symbol * const Symbol::Try    = Symbol::create("try*");
Symbol * const Symbol::Catch  = Symbol::create("catch*");

Symbol * const Symbol::cons   = Symbol::create("cons");
Symbol * const Symbol::concat = Symbol::create("concat");

//----------------------------------------------------------------------

void Keyword::pr_str(std::ostream & os, bool readably)
{
   os << _name;
}

bool Keyword::equals_to(Value * val) const
{
   if (!is_type(val, Type::Keyword))
      return false;
   return _name == ((Keyword *)val)->_name;
}

int Keyword::compare(const Keyword & val) const
{
   return _name.compare(val._name);
}

Keyword * Keyword::create(const char * name)
{
   if (strlen(name) > 1024)
      error(0, "", "too much long name for keyword");

   auto pos = _keywords.find(name);

   if (pos != _keywords.end())
   {
      return pos->second.get();
   }

   Keyword * kwd = new Keyword(name);

   _keywords.emplace(kwd->_name.c_str(), kwd);

   return kwd;
}

bool Keyword::is_keyword(const char * name, Keyword ** kwd)
{
   auto it = _keywords.find(name);
   if (it == _keywords.end())
      return false;

   if (kwd != nullptr)
      *kwd = it->second.get();

   return true;
}

Keyword::Table Keyword::_keywords;

//----------------------------------------------------------------------


void Constant::pr_str(std::ostream & os, bool readably)
{
   os << _name;
}

bool Constant::equals_to(Value * val) const
{
   return this == val;
}

Constant * const Constant::Nil  = new Constant("nil");
Constant * const Constant::True  = new Constant("true");
Constant * const Constant::False = new Constant("false");

//----------------------------------------------------------------------

void Int::pr_str(std::ostream & os, bool readably)
{
   os << _num;
}

bool Int::equals_to(Value * val) const
{
   if (!is_type(val, Type::Int))
      return false;
   return _num == ((Int *)val)->_num;
}

int Int::compare(const Int & val) const
{
   if (_num < val._num)
      return -1;
   if (_num > val._num)
      return 1;
   return 0;
}

Number * Int::add(Number * num) const
{
   if (num->get_type() == Type::Int)
      return Int::create(_num + num->get_int());

   return Real::create(_num + num->get_real());
}

Number * Int::sub(Number * num) const
{
   if (num->get_type() == Type::Int)
      return Int::create(_num - num->get_int());

   return Real::create(_num - num->get_real());
}

Number * Int::mul(Number * num) const
{
   if (num->get_type() == Type::Int)
      return Int::create(_num * num->get_int());

   return Real::create(_num * num->get_real());
}

Number * Int::div(Number * num) const
{
   if (num->get_type() == Type::Int)
      return Int::create(_num / num->get_int());

   return Real::create(_num / num->get_real());
}

Value * Int::lt(Number * num) const
{
   bool val = false;

   if (is_type(num, Type::Int))
      val = _num < ((Int *)num)->_num;
   else
      val = _num < num->get_real();

   return val ? Constant::True : Constant::False;
}

Value * Int::le(Number * num) const
{
   bool val = false;

   if (is_type(num, Type::Int))
      val = _num <= ((Int *)num)->_num;
   else
      val = _num <= num->get_real();

   return val ? Constant::True : Constant::False;
}

Value * Int::gt(Number * num) const
{
   bool val = false;

   if (is_type(num, Type::Int))
      val = _num > ((Int *)num)->_num;
   else
      val = _num > num->get_real();

   return val ? Constant::True : Constant::False;
}

Value * Int::ge(Number * num) const
{
   bool val = false;

   if (is_type(num, Type::Int))
      val = _num >= ((Int *)num)->_num;
   else
      val = _num >= num->get_real();

   return val ? Constant::True : Constant::False;
}

Int * Int::create(int64_t num)
{
   return new Int(num);
}

//----------------------------------------------------------------------

void Real::pr_str(std::ostream & os, bool readably)
{
   snprintf(buffer, buffer_size, "%.16G", _num);

   // add '.' if there is no one
   int pos = (int)strcspn(buffer, ".Ee");
   if (buffer[pos] == 0)
   {
       buffer[pos] = '.';
       buffer[pos + 1] = 0;
   }

   // find end of the number without exponent part
   char * ptr_e = strrchr(buffer, 'E');
   if (ptr_e == nullptr)
      ptr_e = buffer + strlen(buffer);

   char * ptr = ptr_e;

   // remove trailing '0'
   for (ptr--; ptr > buffer; ptr--)
   {
      if (*ptr != '0')
         break;
   }

   // move exponent part to the end of the number
   if (ptr + 1 != ptr_e)
   {
      if (*ptr_e != 0)
         memmove(ptr+1, ptr_e, strlen(ptr_e) + 1);
      else
         ptr[1] = 0;
   }

   os << buffer;
}

bool Real::equals_to(Value * val) const
{
   if (!is_type(val, Type::Real))
      return false;
   return _num == ((Real *)val)->_num;
}

int Real::compare(const Real & val) const
{
   if (_num < val._num)
      return -1;
   if (_num > val._num)
      return 1;
   return 0;
}

Number * Real::add(Number * num) const
{
   return Real::create(_num + num->get_real());
}

Number * Real::sub(Number * num) const
{
   return Real::create(_num - num->get_real());
}

Number * Real::mul(Number * num) const
{
   return Real::create(_num * num->get_real());
}

Number * Real::div(Number * num) const
{
   return Real::create(_num / num->get_real());
}

Value * Real::lt(Number * num) const
{
   bool val = _num < num->get_real();

   return val ? Constant::True : Constant::False;
}

Value * Real::le(Number * num) const
{
   bool val = _num <= num->get_real();

   return val ? Constant::True : Constant::False;
}

Value * Real::gt(Number * num) const
{
   bool val = _num > num->get_real();

   return val ? Constant::True : Constant::False;
}

Value * Real::ge(Number * num) const
{
   bool val = _num >= num->get_real();

   return val ? Constant::True : Constant::False;
}

Real * Real::create(double num)
{
   return new Real(num);
}

//----------------------------------------------------------------------

String::String(const char * str, int add_extra_size)
   : _add_extra_size { add_extra_size }
{
   strcpy(_str, str);
}

String::String(const std::string & str, int add_extra_size)
   : _add_extra_size { add_extra_size }
{
   strcpy(_str, str.c_str());
}

Value * String::gc_clone()
{
   GC::add_extra_size(_add_extra_size);
   return new String(_str, _add_extra_size);
}

void String::pr_str(std::ostream & os, bool readably)
{
   if (readably)
   {
      os << '"';
      for (const char * ptr=_str; *ptr; ptr++)
      {
         unsigned char ch = (unsigned char)*ptr;
         if (ch < ' ')
         {
            os << '\\';
            switch (ch)
            {
               case '\a':  os << 'a'; break;
               case '\b':  os << 'b'; break;
               case '\f':  os << 'f'; break;
               case '\n':  os << 'n'; break;
               case '\r':  os << 'r'; break;
               case '\t':  os << 't'; break;
               case '\v':  os << 'v'; break;

               default:
                  snprintf(buffer, buffer_size, "%02x", (int)ch);
                  os << "x" << buffer;
                  break;
            }
         }
         else if (ch == '"')
            os << "\\\"";
         else if (ch == '\\')
            os << "\\\\";
         else
            os << ch;
      }
      os << '"';
   }
   else
   {
      os << _str;
   }
}

bool String::equals_to(Value * val) const
{
   if (!is_type(val, Type::String))
      return false;

   return strcmp(_str, ((String *)val)->_str) == 0;
}

int String::compare(const String & val) const
{
   return strcmp(_str, val._str);
}

String * String::create(const char * str)
{
   std::ostringstream oss;

   int len = (int)strlen(str);

   if (len < 2 || str[0] != '"' || str[len - 1] != '"')
      error(0, "", "unbalanced string");

   str++;
   len -= 2;

   int state = 0;
   int chval = 0;

   for (int i=0; i < len ;i++)
   {
      unsigned char ch = (unsigned char)str[i];

      switch (state)
      {
         case 0:
            if (ch == '\\')
               state = 1;
            else
               oss << ch;
            break;

         case 1:
            switch (ch)
            {
               case 'x':
                  state = 2;
                  chval = 0;
                  break;

               case '0':
               case '1':
               case '2':
               case '3':
                  state = 5;
                  chval = ch - '0';
                  break;

               case '4':
               case '5':
               case '6':
               case '7':
                  state = 6;
                  chval = ch - '0';
                  break;

               case 'a': state = 0; oss << '\a'; break;
               case 'b': state = 0; oss << '\b'; break;
               case 'f': state = 0; oss << '\f'; break;
               case 'n': state = 0; oss << '\n'; break;
               case 'r': state = 0; oss << '\r'; break;
               case 't': state = 0; oss << '\t'; break;
               case 'v': state = 0; oss << '\v'; break;

               default:
                  state = 0;
                  oss << ch;
                  break;
            }
            break;

         case 2:
         case 3:
            state++;
            if ('0' <= ch && ch <= '9')
               chval = (chval << 4) + (ch - '0');
            else if ('A' <= ch && ch <= 'F')
               chval = (chval << 4) + 10 + (ch - 'A');
            else if ('a' <= ch && ch <= 'f')
               chval = (chval << 4) + 10 + (ch - 'a');
            else if (state == 3)
            {
               oss << 'x';
               state = 0;
            }
            else if (state == 4)
            {
               if (chval > 0)
                  oss << (char)chval;
               i--;
               state = 0;
            }

            if (state == 4)
            {
               if (chval > 0)
                  oss << (char)chval;
               state = 0;
            }
            break;

         case 5:
         case 6:
            state++;
            if ('0' <= ch && ch <= '7')
               chval = (chval << 3) + (ch - '0');
            else
            {
               if (chval > 0)
                  oss << (char)chval;
               i--;
               state = 0;
            }

            if (state == 7)
            {
               if (chval > 0)
                  oss << (char)chval;
               state = 0;
            }
            break;
      }
   }

   if (state > 2)
   {
      if (chval > 0)
         oss << (char)chval;
   }

   std::string cstr = oss.str();

   return create(cstr);
}

String * String::create(const std::string & str)
{
   int add_extra = 0;
   int len = (int)str.length();

   if (len > 7)
   {
      add_extra = 8 * (len / 8);
      GC::add_extra_size(add_extra);
   }

   return new String(str, add_extra);
}

//----------------------------------------------------------------------

bool Sequence::equals_to(Value * val) const
{
   if (!is_sequence(val))
      return false;

   Type type1 = get_type();
   Type type2 = val->get_type();

   if (type1 == Type::List && type2 == Type::List)
   {
      return equals(((List *)this)->elements(), ((List *)val)->elements());
   }

   Sequence * seq = (Sequence *)val;

   int count = length();

   if (count != seq->length())
      return false;

   for (int i=0; i < count; i++)
   {
      if (!equals(nth(i), seq->nth(i)))
         return false;
   }

   return true;
}

//----------------------------------------------------------------------

void ListNode::gc_move()
{
   move_object(_car);
   move_object(_cdr);
}

void ListNode::pr_str(std::ostream & os, bool readably)
{
   ListNode * ptr = this;

   for (;;)
   {
      if (ptr->_car == nullptr)
         os << "null";
      else
         ptr->_car->pr_str(os, readably);

      ptr = ptr->_cdr;

      if (ptr == nullptr)
         break;

      os << ' ';
   }
}

bool ListNode::equals_to(Value * val) const
{
   if (!is_type(val, Type::ListNode))
      return false;

   const ListNode * ptr1 = this;
   const ListNode * ptr2 = (ListNode *)val;

   while (ptr1 != nullptr && ptr2 != nullptr)
   {
      if (ptr1 == ptr2)
         return true;

      if (!equals(ptr1->_car, ptr2->_car))
         return false;

      ptr1 = ptr1->_cdr;
      ptr2 = ptr2->_cdr;
   }

   return ptr1 == ptr2;
}

Value * ListNode::nth(int idx) const
{
   if (idx >= 0)
   {
      for (const ListNode * ptr = this; ptr != nullptr; ptr = ptr->_cdr)
      {
         if (idx-- == 0)
            return ptr->_car;
      }
   }

   error(0, "nth", "index is out of range");

   return nullptr;
}

ListNode * ListNode::last_node()
{
   ListNode * ptr = this;

   while (ptr->_cdr != nullptr)
   {
      ptr = ptr->_cdr;
   }

   return ptr;
}

int ListNode::count() const
{
   int cnt = 0;

   for (const ListNode * ptr = this; ptr != nullptr; ptr = ptr->_cdr)
      cnt++;

   return cnt;
}

bool ListNode::min_count_p(int min_cnt) const
{
   if (min_cnt <= 0)
      true;

   for (const ListNode * ptr = this; ptr != nullptr; ptr = ptr->_cdr)
   {
      if (--min_cnt <= 0)
         return true;
   }

   return false;
}

ListNode * ListNode::create(Value * car, ListNode * cdr)
{
   GCStack gcs;

   gcs.push(car);
   gcs.push(cdr);

   ListNode * nd = new ListNode();

   nd->_car = car;
   nd->_cdr = cdr;

   return nd;
}

//----------------------------------------------------------------------

void List::gc_move()
{
   move_object(_elements);
}

void List::pr_str(std::ostream & os, bool readably)
{
   os << '(';
   if (_elements != nullptr)
      _elements->pr_str(os, readably);
   os << ')';
}

void List::cons(Value * val)
{
   GCStack gcs;
   ListP self = this;

   self->_elements = ListNode::create(val, _elements);
}

List * List::map(std::function<Value * (Value *)> fn)
{
   GCStack gcs;

   ValueP val;
   ListNodeP beg = nullptr;
   ListNodeP end = nullptr;
   ListNodeP ptr = _elements;

   for (; ptr != nullptr; ptr = ptr->_cdr)
   {
      val = fn(ptr->_car);
      ListNode * nod = ListNode::create(val);

      if (beg == nullptr)
      {
         beg = nod;
         end = nod;
      }
      else
      {
         end->_cdr = nod;
         end = nod;
      }
   }

   return List::create(beg);
}

void List::for_each(std::function<void (Value *)> fn)
{
   GCStack gcs;

   ListNodeP ptr = _elements;

   for (; ptr != nullptr; ptr = ptr->_cdr)
   {
      fn(ptr->_car);
   }
}

void List::reverse_d()
{
   ListNode * ptr = _elements;
   ListNode * cdr = nullptr;

   while (ptr != nullptr)
   {
      std::swap(cdr, ptr->_cdr);
      std::swap(ptr, cdr);
   }

   _elements = cdr;
}

void List::rev_prepend_d(ListNode * node)
{
   GCStack gcs;
   gcs.push(node);

   ListP self = this;
   ListNodeP elm;
   ListNodeP beg = _elements;

   for (; node != nullptr; node = node->_cdr)
   {
      elm = ListNode::create(node->_car, beg);
      beg = elm;
   }

   self->_elements = beg;
}

Value * List::nth(int idx) const
{
   if (_elements == nullptr)
      error(0, "nth", "index is out of range");

   return _elements->nth(idx);
}

List * List::create(ListNode * elms)
{
   GCStack gcs;
   gcs.push(elms);

   List * lst = new List();

   lst->_elements = elms;

   return lst;
}

//----------------------------------------------------------------------

Vector::Vector(int count, bool clear)
{
   _count = std::max(count, 0);
   _elements[0] = nullptr;
   if (clear && _count > 0)
      memset(_elements, 0, _count * sizeof(Value *));
}

Value * Vector::gc_clone()
{
   if (_count > 1)
      GC::add_extra_size((_count - 1) * sizeof(Value *));

   Vector * v = new Vector(_count, false);

   memmove(v->_elements, _elements, _count * sizeof(Value *));

   return v;
}

void Vector::gc_move()
{
   for (int i=0; i < _count; i++)
      move_object(_elements[i]);
}

void Vector::pr_str(std::ostream & os, bool readably)
{
   os << '[';
   if (_count > 0)
   {
      for (int i=0; i < _count; i++)
      {
         if (i > 0)
            os << ' ';
         _elements[i]->pr_str(os, readably);
      }
   }
   os << ']';
}

Vector * Vector::map(std::function<Value * (Value *)> fn)
{
   GCStack gcs;
   VectorP self = this;
   ValueP val;

   int count =_count;

   VectorP vec = create(count, false);

   for (int i=0; i < count; i++)
   {
      val = fn(self->_elements[i]);
      vec->_elements[i] = val;
   }

   return vec;
}

List * Vector::map_to_list(std::function<Value * (Value *)> fn)
{
   GCStack gcs;
   VectorP self = this;

   ListNodeP beg = nullptr;
   ListNodeP end = nullptr;

   int count =_count;

   for (int i=0; i < count; i++)
   {
      ListNode * nod = ListNode::create(fn(self->_elements[i]));
      if (beg == nullptr)
      {
         beg = nod;
         end = nod;
      }
      else
      {
         end->set_cdr(nod);
         end = nod;
      }
   }

   return List::create(beg);
}

ListNode * Vector::to_node_list()
{
   GCStack gcs;

   VectorP self = this;

   ListNodeP beg = nullptr;
   ListNodeP end;
   ListNodeP nod;

   int count = _count;

   for (int i=0; i < count; i++)
   {
      nod = ListNode::create(self->_elements[i]);
      if (beg == nullptr)
      {
         beg = nod;
         end = nod;
      }
      else
      {
         end->set_cdr(nod);
         end = nod;
      }
   }

   return beg;
}


List * Vector::drop(int n)
{
   GCStack gcs;

   VectorP self = this;

   ListNodeP beg = nullptr;
   ListNodeP end;
   ListNodeP nod;

   int count = _count;

   if (n < 0)
      n = 0;

   for (int i=n; i < count; i++)
   {
      nod = ListNode::create(self->_elements[i]);
      if (beg == nullptr)
      {
         beg = nod;
         end = nod;
      }
      else
      {
         end->set_cdr(nod);
         end = nod;
      }
   }

   return List::create(beg);
}

Value * Vector::nth(int idx) const
{
   if (idx < 0 || idx >= _count)
      error(0, "nth", "index is out of range");

   return _elements[idx];
}

Vector * Vector::create(int count, bool clear)
{
   if (count < 0)
      count = 0;

   if (count > 1)
      GC::add_extra_size((count - 1) * sizeof(Value *));

   return new Vector(count, clear);
}

Vector * Vector::create(std::vector<Value *> & elms)
{
   int count = (int)elms.size();

   GCStack gcs;
   for (int i=0; i < count; i++)
      gcs.push(elms[i]);

   if (count > 1)
      GC::add_extra_size((count - 1) * sizeof(Value *));

   Vector * v = new Vector(count, false);

   memmove(v->_elements, elms.data(), count * sizeof(Value *));

   return v;
}

Vector * Vector::create(Vector * vec)
{
   GCStack gcs;
   gcs.push(vec);

   int count = vec->_count;

   Vector * v = create(count, false);

   memmove(v->_elements, vec->_elements, count * sizeof(Value *));

   return v;
}

//----------------------------------------------------------------------

void Atom::gc_move()
{
   move_object(_value);
}

void Atom::pr_str(std::ostream & os, bool readably)
{
   os << "(atom ";
   _value->pr_str(os, readably);
   os << ")";
}

bool Atom::equals_to(Value * val) const
{
   if (!is_type(val, Type::Atom))
      return false;

   return equals(_value, ((Atom *)val)->_value);
}

Atom * Atom::create(Value * val)
{
   GCStack gcs;
   gcs.push(val);

   Atom * atm = new Atom();

   atm->set(val);

   return atm;
}

//----------------------------------------------------------------------

TreeNode::TreeNode(Value * key, Value * data,
                   TreeNode * ltn, TreeNode * rtn)
   : _key { key }, _data { data }, _ltn { ltn }, _rtn {rtn }
{
   int ld = _ltn == nullptr ? 0 : _ltn->_height;
   int rd = _rtn == nullptr ? 0 : _rtn->_height;

   _height = 1 + std::max(ld, rd);
}

void TreeNode::set(Value * key, Value * data, TreeNode * ltn, TreeNode * rtn)
{
   _key = key;
   _data = data;
   _ltn = ltn;
   _rtn = rtn;

   int ld = _ltn == nullptr ? 0 : _ltn->_height;
   int rd = _rtn == nullptr ? 0 : _rtn->_height;

   _height = 1 + std::max(ld, rd);
}

int TreeNode::balance(TreeNode * node)
{
   if (node == nullptr)
      return 0;

   int ld = node->_ltn == nullptr ? 0 : node->_ltn->_height;
   int rd = node->_rtn == nullptr ? 0 : node->_rtn->_height;

   return ld - rd;
}

TreeNode * TreeNode::rotateL(TreeNode * node)
{
   if (node == nullptr)
      return nullptr;

   if (node->_rtn == nullptr)
      return node;

   GCStack gcs;
   gcs.push(node);

   TreeNodeP ltn;

   ltn = create(node->_key, node->_data,
                node->_ltn, node->_rtn->_ltn);

   return create(node->_rtn->_key, node->_rtn->_data,
                 ltn, node->_rtn->_rtn);
}

TreeNode * TreeNode::rotateR(TreeNode * node)
{
   if (node == nullptr)
      return nullptr;

   if (node->_ltn == nullptr)
      return node;

   GCStack gcs;
   gcs.push(node);

   TreeNodeP rtn;

   rtn = create(node->_key, node->_data,
                node->_ltn->_rtn, node->_rtn);

   return create(node->_ltn->_key, node->_ltn->_data,
                 node->_ltn->_ltn, rtn);
}

TreeNode * TreeNode::rebalance(TreeNode * node)
{
   GCStack gcs;
   gcs.push(node);

   switch (balance(node))
   {
      case 2:
         if (balance(node->_ltn) == -1)
         {
            TreeNode * ltn = rotateL(node->_ltn);

            return rotateR(create(node->_key, node->_data,
                                  ltn, node->_rtn));
         }

         return rotateR(node);

      case -2:
         if (balance(node->_rtn) == 1)
         {
            TreeNode * rtn = rotateR(node->_rtn);

            return rotateL(create(node->_key, node->_data,
                                  node->_ltn, rtn));
         }
         return rotateL(node);
   }

   return node;
}

TreeNode * TreeNode::insert(TreeNode * node, Value * key, Value * data)
{
   GCStack gcs;
   gcs.push(node);
   gcs.push(key);
   gcs.push(data);

   if (node == nullptr)
      return create(key, data, nullptr, nullptr);

   int cmp = compare(key, node->_key);

   if (cmp == 0)
      return create(key, data, node->_ltn, node->_rtn);

   if (cmp < 0)
   {
      TreeNode * ltn = insert(node->_ltn, key, data);

      return rebalance(create(node->_key, node->_data,
                              ltn, node->_rtn));
   }

   TreeNode * rtn = insert(node->_rtn, key, data);

   return rebalance(create(node->_key, node->_data,
                           node->_ltn, rtn));
}

std::pair<TreeNode *, TreeNode *> TreeNode::pop_min(TreeNode * node)
{
   GCStack gcs;
   gcs.push(node);

   if (node == nullptr)
      return std::make_pair(nullptr, nullptr);

   if (node->get_left() == nullptr)
      return std::make_pair(node, node->get_right());

   auto rec = pop_min(node->get_left());

   TreeNodeP mnode = rec.first;
   TreeNodeP mlt = rec.second;
   TreeNodeP res = rebalance(create(node->get_key(),
                                    node->get_data(),
                                    mlt,
                                    node->get_right()));
   return std::make_pair(mnode, res);
}

std::pair<TreeNode *, TreeNode *> TreeNode::pop_max(TreeNode * node)
{
   GCStack gcs;
   gcs.push(node);

   if (node == nullptr)
      return std::make_pair(nullptr, nullptr);

   if (node->get_right() == nullptr)
      return std::make_pair(node, node->get_left());

   auto rec = pop_max(node->get_right());

   TreeNodeP mnode = rec.first;
   TreeNodeP mrt = rec.second;
   TreeNodeP res = rebalance(create(node->get_key(),
                                    node->get_data(),
                                    node->get_left(),
                                    mrt));
   return std::make_pair(mnode, res);
}

TreeNode * TreeNode::remove(TreeNode * node, Value * key)
{
   if (node == nullptr)
      return nullptr;

   GCStack gcs;
   gcs.push(node);
   gcs.push(key);

   TreeNodeP mnode;
   TreeNodeP mrt;

   int cmp = compare(key, node->_key);

   if (cmp < 0)
   {
      mnode = remove(node->_ltn, key);

      return rebalance(create(node->_key,
                              node->_data,
                              mnode,
                              node->_rtn));
   }

   if (cmp > 0)
   {
      mnode = remove(node->_rtn, key);

      return rebalance(create(node->_key,
                              node->_data,
                              node->_ltn,
                              mnode));
   }

   if (node->_ltn == nullptr)
      return node->_rtn;
   if (node->_rtn == nullptr)
      return node->_ltn;

   auto p = pop_min(node->_rtn);

   mnode = p.first;
   mrt = p.second;

   if (mnode == nullptr)
      error(0, "remove", "AVLTree exception");

   return rebalance(create(mnode->_key,
                           mnode->_data,
                           node->_ltn,
                           mrt));
}

TreeNode * TreeNode::lookup(TreeNode * node, Value * key)
{
   if (node == nullptr)
      return nullptr;

   int cmp = compare(key, node->_key);
   if (cmp == 0)
      return node;

   return lookup(cmp < 0 ? node->_ltn : node->_rtn, key);
}

void TreeNode::gc_move()
{
   move_object(_key);
   move_object(_data);
   move_object(_ltn);
   move_object(_rtn);
}

void TreeNode::pr_str(std::ostream & os, bool readably)
{
   if (_ltn != nullptr)
   {
      _ltn->pr_str(os, readably);
      os << ' ';
   }

   _key->pr_str(os, readably);
   os << ' ';
   _data->pr_str(os, readably);

   if (_rtn != nullptr)
   {
      os << ' ';
      _rtn->pr_str(os, readably);
   }
}

bool TreeNode::equals_to(Value * val) const
{
   return this == val;
}

TreeNode * TreeNode::map(std::function<Value * (Value *)> fn)
{
   GCStack gcs;

   TreeNodeP self = this;

   TreeNodeP ltn = self->_ltn == nullptr
                 ? self->_ltn : self->_ltn->map(fn);

   ValueP data = fn(_data);

   TreeNodeP rtn = self->_rtn == nullptr
                 ? self->_rtn : self->_rtn->map(fn);

   return TreeNode::create(self->_key, data, ltn, rtn);
}

void TreeNode::for_each(std::function<void (Value *, Value *)> fn)
{
   GCStack gcs;
   TreeNodeP self = this;

   if (_ltn != nullptr)
      _ltn->for_each(fn);

   fn(_key, _data);

   if (self->_rtn != nullptr)
      self->_rtn->for_each(fn);
}

int TreeNode::count() const
{
   int count = 1;

   if (_ltn != nullptr)
      count += _ltn->count();
   if (_rtn != nullptr)
      count += _rtn->count();

   return count;
}

Value * TreeNode::gc_clone()
{
   return new TreeNode(_key, _data, _ltn, _rtn);
}

TreeNode * TreeNode::create(Value * key, Value * data,
                            TreeNode * ltn, TreeNode * rtn)
{
   GCStack gcs;

   gcs.push(key);
   gcs.push(data);
   gcs.push(ltn);
   gcs.push(rtn);

   TreeNode * nd = new TreeNode();

   nd->set(key, data, ltn, rtn);

   return nd;
}

//----------------------------------------------------------------------

void AVLTree::gc_move()
{
   move_object(_root);
}

void AVLTree::pr_str(std::ostream & os, bool readably)
{
   os << '{';
   if (_root != nullptr)
      _root->pr_str(os, readably);
   os << '}';
}

bool AVLTree::equals_to(Value * val) const
{
   if (!is_type(val, Type::AVLTree))
      return false;

   TreeNode * root = ((AVLTree *)val)->_root;

   if (equals(_root, root))
      return true;

   if (_root == nullptr || root == nullptr)
      return false;

   std::vector<Value *> v1;
   std::vector<Value *> v2;

   _root->for_each([&](Value * key, Value * data)
                   { v1.push_back(key); v1.push_back(data); });
    root->for_each([&](Value * key, Value * data)
                   { v2.push_back(key); v2.push_back(data); });

   if (v1.size() != v2.size())
      return false;

   for (int i=0; i < v1.size(); i++)
   {
      if (!equals(v1[i], v2[i]))
         return false;
   }

   return true;
}

AVLTree * AVLTree::map(std::function<Value * (Value *)> fn)
{
   return AVLTree::create(_root == nullptr ? _root : _root->map(fn));
}

//----------------------------------------------------------------------

void Function::pr_str(std::ostream & os, bool readably)
{
   if (_is_macro)
   {
      snprintf(buffer, buffer_size, "<macro %p>", (void *)this);
      os << buffer;
   }
   else
   {
      os << "<function '"
         << _name
         << "'>";
   }
}

bool Function::equals_to(Value * val) const
{
   return false;
}

void check_args_N(ListNode * args, int cnt, const char * name)
{
   if (cnt < 1)
   {
      if (args != nullptr)
         error(0, name, "expects no argument");
   }
   else
   {
      const char * plural = cnt == 1 ? "" : "s";

      if (args == nullptr || args->count() != cnt)
         error(0, name, "expects %d argument%s", cnt, plural);
   }
}

void check_args_V(ListNode * args, int cnt, const char * name)
{
   if (cnt >= 1)
   {
      const char * plural = cnt <= 1 ? "" : "s";

      if (args == nullptr || !args->min_count_p(cnt))
         error(0, name, "expects at least %d argument%s", cnt, plural);
   }
}

//----------------------------------------------------------------------

Value * Closure::gc_clone()
{
   return new Closure(_env, _pars, _body, _is_macro);
}

void Closure::gc_move()
{
   EnvStack::move_objects(_env);

   move_object(_pars);
   move_object(_body);
}

void Closure::pr_str(std::ostream & os, bool readably)
{
#ifdef _DEBUG
   if (_is_macro)
      os << "<macro ";
   else
      os << "<closure ";

   _pars->pr_str(os, true);
   os << " ";
   _body->pr_str(os, true);
   os << ">";
#else
   if (_is_macro)
      snprintf(buffer, buffer_size, "<macro %p>", (void *)this);
   else
      snprintf(buffer, buffer_size, "<closure %p>", (void *)this);

   os << buffer;
#endif
}

bool Closure::equals_to(Value * val) const
{
   return false;
}

EnvPtr Closure::create_env(ListNode * args)
{
   GCStack gcs;
   gcs.push(args);

   SequenceP pars = _pars;

   bool is_rest = false;

   int cnt_p = pars->length();
   if (cnt_p >= 2 && pars->nth(cnt_p - 2) == Symbol::Amp)
   {
      is_rest = true;
      cnt_p -= 2;
   }

   int cnt_a = args->count();
   if (!(cnt_p == cnt_a && !is_rest ||
         cnt_p <= cnt_a && is_rest))
   {
      error(0, "apply", "wrong number of arguments");
   }

   EnvPtr env(new Env(_env));
   EnvStack ens(env);

   for (int i=0; i < cnt_p; i++)
   {
      env->set((Symbol *)pars->nth(i), args->nth(i));
   }

   ListP rest;

   if (is_rest)
   {
      rest = List::create();

      for (int i=cnt_p; i < cnt_a; i++)
         rest->cons(args->nth(i));

      rest->reverse_d();

      env->set((Symbol *)pars->nth(cnt_p + 1), rest);
   }

   return env;
}

Value * Closure::eval(ListNode * args)
{
   GCStack gcs;
   gcs.push(args);

   ClosureP self = this;

   EnvPtr env = create_env(args);
   EnvStack ens(env);

   return EVAL(self->_body, env);
}

Closure * Closure::create(EnvPtr & env, Sequence * pars, Value * body)
{
   GCStack gcs;

   gcs.push(pars);
   gcs.push(body);

   Closure * cls = new Closure(env);

   cls->set(pars, body);

   return cls;
}

Closure * Closure::create(Closure * cls)
{
   GCStack gcs;
   gcs.push(cls);

   EnvPtr env = cls->_env;

   Closure * cls_ = new Closure(env);

   cls_->set(cls->_pars, cls->_body);
   cls_->set_macro(cls->_is_macro);

   return cls_;
}

//----------------------------------------------------------------------

void GCStack::move_objects()
{
   std::for_each(_values, _values + _size, Value::move_object_);
}

Value *** GCStack::_values = nullptr;
int GCStack::_capacity = 0;
int GCStack::_size = 0;

//----------------------------------------------------------------------

std::map<Value *, Value *> gMetaData;

void move_meta_data()
{
   std::vector<Value *> md;
   bool repeat = true;

   while (repeat)
   {
      repeat = false;

      std::for_each(gMetaData.begin(), gMetaData.end(),
                     [&](auto & kvp)
                     {
                        if (GC::is_in_pool_B(kvp.first) &&
                            Value::is_type(kvp.first, Type::Reference) &&
                            GC::is_in_pool_B(kvp.second))
                        {
                           Value::move_object(kvp.second);
                           repeat = true;
                        }
                     });
   }

   std::for_each(gMetaData.begin(), gMetaData.end(),
                  [&](auto & kvp)
                  {
                     if (GC::is_in_pool_B(kvp.first) &&
                         Value::is_type(kvp.first, Type::Reference))
                     {
                        md.push_back(R<Reference>(kvp.first).get());
                        md.push_back(kvp.second);
                     }
                  });

   gMetaData.clear();

   size_t cnt = md.size();
   for (size_t i=1; i < cnt; i+=2)
      gMetaData[ md[i-1] ] = md[i];
}

}
