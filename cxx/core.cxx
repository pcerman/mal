#include <sstream>
#include <iostream>
#include <fstream>
#include <chrono>
#include <limits>

#include "core.h"
#include "types.h"
#include "reader.h"
#include "env.h"
#include "gc.h"

namespace MAL
{

Value * eval_stream(std::istream & stream, EnvPtr & env)
{
   GCStack gcs;

   ValueP form = nullptr;
   ValueP value = Constant::Nil;

   reader rdr(stream);

   for (;;)
   {
      form = rdr.read_form();
      if (form == nullptr)
         break;
      value = EVAL(form, env);
   }

   return value;
}

Value * c_add(ListNode * args, const char * name)
{
   check_args_V(args, 1, name);

   GCStack gcs;
   gcs.push(args);

   NumberP res = nullptr;

   for (int i=1; args != nullptr; args = args->get_cdr(), i++)
   {
      if (!Value::is_number(args->get_car()))
         error_expected(i, "number", name);

      Number * num = (Number *)(args->get_car());
      if (res == nullptr)
         res = num;
      else
         res = res->add(num);
   }

   return res;
}

Value * c_sub(ListNode * args, const char * name)
{
   check_args_V(args, 1, name);

   GCStack gcs;
   gcs.push(args);

   NumberP res = nullptr;

   int cnt = 0;

   for (int i=1; args != nullptr; args = args->get_cdr(), i++)
   {
      cnt++;

      if (!Value::is_number(args->get_car()))
         error_expected(i, "number", name);

      Number * num = (Number *)(args->get_car());
      if (res == nullptr)
         res = num;
      else
         res = res->sub(num);
   }

   if (cnt == 1)
      res = Int::zero()->sub(res);

   return res;
}

Value * c_mul(ListNode * args, const char * name)
{
   check_args_V(args, 1, name);

   GCStack gcs;
   gcs.push(args);

   NumberP res = nullptr;

   for (int i=1; args != nullptr; args = args->get_cdr(), i++)
   {
      if (!Value::is_number(args->get_car()))
         error_expected(i, "number", name);

      Number * num = (Number *)(args->get_car());
      if (res == nullptr)
         res = num;
      else
         res = res->mul(num);
   }

   return res;
}

Value * c_div(ListNode * args, const char * name)
{
   check_args_V(args, 1, name);

   GCStack gcs;
   gcs.push(args);

   NumberP res = nullptr;

   int cnt = 0;

   for (int i=1; args != nullptr; args = args->get_cdr(), i++)
   {
      cnt++;

      if (!Value::is_number(args->get_car()))
         error_expected(i, "number", name);

      Number * num = (Number *)(args->get_car());
      if (res == nullptr)
         res = num;
      else
         res = res->div(num);
   }

   if (cnt == 1)
      res = Real::one()->div(res);

   return res;
}

Value * c_lt(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (!Value::is_number(arg1))
      error_expected(1, "number", name);

   if (!Value::is_number(arg2))
      error_expected(2, "number", name);

   return R<Number>(arg1).lt((Number *)arg2);
}

Value * c_le(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (!Value::is_number(arg1))
      error_expected(1, "number", name);

   if (!Value::is_number(arg2))
      error_expected(2, "number", name);

   return R<Number>(arg1).le((Number *)arg2);
}

Value * c_gt(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (!Value::is_number(arg1))
      error_expected(1, "number", name);

   if (!Value::is_number(arg2))
      error_expected(2, "number", name);

   return R<Number>(arg1).gt((Number *)arg2);
}

Value * c_ge(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (!Value::is_number(arg1))
      error_expected(1, "number", name);

   if (!Value::is_number(arg2))
      error_expected(2, "number", name);

   return R<Number>(arg1).ge((Number *)arg2);
}

Value * c_equal(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   return Value::equals(arg1, arg2) ? Constant::True : Constant::False;
}

Value * c_list(ListNode * args, const char * name)
{
   return List::create(args);
}

Value * c_list_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (Value::is_type(arg, Type::List))
      return Constant::True;

   return Constant::False;
}

Value * c_empty_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (arg == Constant::Nil)
      return Constant::True;

   bool empty = false;

   switch (arg->get_type())
   {
      case Type::List:
         empty = R<List>(arg).is_null();
         break;

      case Type::Vector:
         empty = R<Vector>(arg).length() == 0;
         break;

      case Type::AVLTree:
         empty = R<AVLTree>(arg).empty_p();
         break;

      default:
         error_expected(0, "sequence/hashmap", name);
         break;
   }

   return empty ? Constant::True : Constant::False;
}

Value * c_count(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (arg == Constant::Nil)
      return Int::zero();

   int count = 0;

   switch (arg->get_type())
   {
      case Type::List:
         count = R<List>(arg).length();
         break;

      case Type::Vector:
         count = R<Vector>(arg).length();
         break;

      case Type::AVLTree:
         count = R<AVLTree>(arg).count();
         break;

      case Type::String:
         count = (int)strlen(R<String>(arg).str());
         break;

      default:
         error(0, "sequence/hashmap/string", name);
         break;
   }

   return Int::create(count);
}

Value * c_str(ListNode * args, const char * name)
{
   std::ostringstream os;

   for (;args != nullptr; args = args->get_cdr())
   {
      if (args->get_car() == nullptr)
         continue;

      args->get_car()->pr_str(os, false);
   }

   return String::create(os.str());
}

Value * c_pr_str(ListNode * args, const char * name)
{
   std::ostringstream os;

   bool space = false;

   for (;args != nullptr; args = args->get_cdr())
   {
      if (args->get_car() == nullptr)
         continue;

      if (space)
         os << ' ';
      else
         space = true;

      args->get_car()->pr_str(os, true);
   }

   return String::create(os.str());
}

Value * c_prn(ListNode * args, const char * name)
{
   std::ostringstream os;

   bool space = false;

   for (;args != nullptr; args = args->get_cdr())
   {
      if (args->get_car() == nullptr)
         continue;

      if (space)
         os << ' ';
      else
         space = true;

      args->get_car()->pr_str(os, true);
   }

   std::cout << os.str() << std::endl;

   return Constant::Nil;
}

Value * c_println(ListNode * args, const char * name)
{
   std::ostringstream os;

   bool space = false;

   for (;args != nullptr; args = args->get_cdr())
   {
      if (args->get_car() == nullptr)
         continue;

      if (space)
         os << ' ';
      else
         space = true;

      args->get_car()->pr_str(os, false);
   }

   std::cout << os.str() << std::endl;

   return Constant::Nil;
}

Value * c_read_string(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (!Value::is_type(arg, Type::String))
      error_expected(0, "string", name);

   String * str = (String *)arg;
   reader rdr(str->str());

   return rdr.read_form();
}

Value * c_slurp(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (!Value::is_type(arg, Type::String))
      error_expected(0, "string", name);

   String * str = (String *)arg;

   std::ifstream inp(str->str());
   if (!inp.is_open())
      error(0, name, "file '%s' does not exist", str->str());

   // get length of file:
   inp.seekg (0, inp.end);
   int length = (int)inp.tellg();
   inp.seekg (0, inp.beg);

   std::string content(length, ' ');

   inp.read((char *)content.c_str(), length);

   return String::create(content);
}

Value * c_load_file(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (!Value::is_type(arg, Type::String))
      error_expected(0, "string", name);

   String * str = (String *)arg;

   std::string filename = str->str();

   std::ifstream inp(filename.c_str());
   if (!inp.is_open())
      error(0, name, "file '%s' does not exist", str->str());

   eval_stream(inp, gTopEnv);

   return Constant::Nil;
}

Value * c_eval(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return EVAL(arg, gTopEnv);
}

Value * c_atom(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Atom::create(arg);
}

Value * c_atom_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::Atom)
        ? Constant::True : Constant::False;
}

Value * c_deref(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (!Value::is_type(arg, Type::Atom))
      error_expected(0, "atom", name);

   return R<Atom>(arg).get();
}

Value * c_reset(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (!Value::is_type(arg1, Type::Atom))
      error_expected(1, "atom", name);

   R<Atom>(arg1).set(arg2);

   return arg2;
}

Value * c_swap(ListNode * args, const char * name)
{
   if (args == nullptr)
      error_expected(1, "atom", name);

   GCStack gcs;
   gcs.push(args);

   ValueP v1 = args->get_car();
   args = args->get_cdr();

   if (!Value::is_type(v1, Type::Atom))
      error_expected(1, "atom", name);

   if (args == nullptr)
      error_expected(2, "function", name);

   ValueP v2 = args->get_car();
   args = args->get_cdr();

   ValueP res = nullptr;

   ListNodeP nod = ListNode::create(v1.R<Atom>().get(), args);

   if (Value::is_type(v2, Type::Function))
   {
      res = v2.R<Function>().eval(nod);
   }
   else if (Value::is_type(v2, Type::Closure))
   {
      res = v2.R<Closure>().eval(nod);
   }
   else
      error_expected(2, "function", name);

   v1.R<Atom>().set(res);

   return res;
}

Value * c_cons(ListNode * args, const char * name)
{
   check_args_2(args, name);

   GCStack gcs;

   ValueP arg1 = args->nth(0);
   ValueP arg2 = args->nth(1);

   ListP lst;

   if (arg2 == Constant::Nil)
   {
      lst = List::create();
      lst->cons(arg1);
   }
   else if (Value::is_type(arg2, Type::List))
   {
      lst = List::create();
      lst->set_elements(R<List>(arg2).elements());
      lst->cons(arg1);
   }
   else if (Value::is_type(arg2, Type::Vector))
   {
      lst = List::create();
      VectorP vec = arg2.P<Vector>();

      int count = vec->length();
      for (int i=count-1; i >= 0; i--)
      {
         lst->cons(vec->nth(i));
      }

      lst->cons(arg1);
   }
   else
      error_expected(2, "sequence", name);

   return lst;
}

Value * c_concat(ListNode * args, const char * name)
{
   GCStack gcs;

   gcs.push(args);

   ListNodeP beg = nullptr;
   ListNodeP end;
   ListNodeP nod;
   ListNodeP ptr;
   ValueP val;

   for (int idx = 1; args != nullptr; args = args->get_cdr(), idx++)
   {
      bool is_last = args->get_cdr() == nullptr;

      val = args->get_car();

      if (val == Constant::Nil)
      { }
      else if (Value::is_type(val, Type::List))
      {
         ptr = val.R<List>().elements();

         if (is_last)
         {
            if (beg == nullptr)
               beg = ptr;
            else
               end->set_cdr(ptr);
         }
         else
         {
            for (; ptr != nullptr; ptr = ptr->get_cdr())
            {
               nod = ListNode::create(ptr->get_car());
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
         }
      }
      else if (Value::is_type(val, Type::Vector))
      {
         int cnt = val.R<Vector>().length();

         for (int i=0; i < cnt; i++)
         {
            nod = ListNode::create(val.R<Vector>().nth(i));
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
      }
      else
         error_expected(idx, "sequence", name);
   }

   return List::create(beg);
}

Value * c_nth(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (!Value::is_sequence(arg1))
      error_expected(0, "sequence", name);

   if (!Value::is_type(arg2, Type::Int))
      error_expected(2, "non negative integer", name);

   auto idx = R<Int>(arg2).get_int();
   if (idx < 0 || idx > std::numeric_limits<int>::max())
       error(0, name, "index is out of range");

   return R<Sequence>(arg1).nth((int)idx);
}

Value * c_first(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (arg == Constant::Nil)
      return Constant::Nil;

   Value * elm = nullptr;

   if (Value::is_type(arg, Type::List))
      elm = R<List>(arg).first();
   else if (Value::is_type(arg, Type::Vector))
   {
      if (R<Vector>(arg).length() > 0)
         elm = R<Vector>(arg).nth(0);
   }
   else
      error_expected(0, "sequence", name);

   return elm == nullptr ? Constant::Nil : elm;
}

Value * c_rest(ListNode * args, const char * name)
{
   check_args_1(args, name);

   GCStack gcs;

   ValueP arg = args->nth(0);

   if (arg == Constant::Nil)
      return List::create();

   ListP res = nullptr;

   if (Value::is_type(arg, Type::List))
      res = R<List>(arg).rest();
   else if (Value::is_type(arg, Type::Vector))
      res = R<Vector>(arg).drop(1);
   else
      error_expected(0, "sequence", name);

   return res;
}

Value * c_apply(ListNode * args, const char * name)
{
   GCStack gcs;
   gcs.push(args);

   FunctionP fun = nullptr;
   ClosureP cls = nullptr;

   ValueP val = args->get_car();

   if (Value::is_type(val, Type::Function))
   {
      if (!R<Function>(val).is_macro())
         fun = P<Function>(val);
   }
   else if (Value::is_type(val, Type::Closure))
   {
      if (!R<Closure>(val).is_macro())
         cls = P<Closure>(val);
   }

   if (fun == nullptr && cls == nullptr)
      error_expected(1, "function/closure", name);

   args = args->get_cdr();

   ValueP last = args->last();

   ListNodeP beg = nullptr;
   ListNodeP end;
   ListNodeP nod;

   if (Value::is_sequence(last))
   {
      while (args != nullptr && args->get_cdr() != nullptr)
      {
         nod = ListNode::create(args->get_car());
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
         args = args->get_cdr();
      }

      if (Value::is_type(last, Type::List))
         nod = last.R<List>().elements();
      else if (Value::is_type(last, Type::Vector))
         nod = last.R<Vector>().drop(0)->elements();

      if (beg == nullptr)
         beg = nod;
      else
         end->set_cdr(nod);
   }
   else
   {
      beg = args;
   }

   if (fun != nullptr)
      val = fun->eval(beg);
   else if (cls != nullptr)
      val = cls->eval(beg);
   else
      val = Constant::Nil;

   return val;
}

Value * c_map(ListNode * args, const char * name)
{
   check_args_2(args, name);

   GCStack gcs;

   ValueP arg1 = args->nth(0);
   ValueP arg2 = args->nth(1);

   FunctionP fun = nullptr;
   ClosureP cls = nullptr;

   if (Value::is_type(arg1, Type::Function))
   {
      if (!R<Function>(arg1).is_macro())
         fun = P<Function>(arg1);
   }
   else if (Value::is_type(arg1, Type::Closure))
   {
      if (!R<Closure>(arg1).is_macro())
         cls = P<Closure>(arg1);
   }

   if (fun == nullptr && cls == nullptr)
      error_expected(1, "function/closure", name);

   ListNodeP par = ListNode::create(nullptr);

   List * lst = nullptr;

   std::function<Value * (Value *)> fn;

   if (fun != nullptr)
   {
      fn = [&](Value * elm)
           {
              par->set_car(elm);
              Value * val = fun->eval(par);
              par->set_car(nullptr);
              return val;
            };
   }

   if (cls != nullptr)
   {
      fn = [&](Value * elm)
           {
              par->set_car(elm);
              Value * val = cls->eval(par);
              par->set_car(nullptr);
              return val;
            };
   }

   if (Value::is_type(arg2, Type::List))
   {
      lst = R<List>(arg2).map(fn);
   }
   else if (Value::is_type(arg2, Type::Vector))
   {
      lst = R<Vector>(arg2).map_to_list(fn);
   }
   else
      error_expected(2, "sequence", name);

   return lst;
}

Value * c_nil_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return arg == Constant::Nil ? Constant::True : Constant::False;
}

Value * c_true_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return arg == Constant::True ? Constant::True : Constant::False;
}

Value * c_false_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return arg == Constant::False ? Constant::True : Constant::False;
}

Value * c_symbol_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::Symbol)
        ? Constant::True : Constant::False;
}

Value * c_keyword_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::Keyword)
        ? Constant::True : Constant::False;
}

Value * c_symbol(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (!Value::is_type(arg, Type::String))
      error_expected(0, "string", name);

   const char * str = R<String>(arg).str();
   int len = (int)strlen(str);

   if (len == 0)
      error(0, name, "non empty string is required");

   if (len >= buffer_size - 8)
      error(0, name, "name is too much long");

   if (str[0] == ':')
      error(0, name, "invalid first character: ':'");

   for (const char * ptr = str; *ptr; ptr++)
   {
      if (is_non_atom(*ptr))
         error(0, name, "invalid character: '%c'", *ptr);
   }

   if (std::regex_match(str, re_int_number) ||
       std::regex_match(str, re_real_number) ||
       strcmp(str, "true") == 0 ||
       strcmp(str, "false") == 0 ||
       strcmp(str, "nil") == 0 ||
       Keyword::is_keyword(str))
   {
      error(0, name, "invalid name: '%s'", str);
   }

   return Symbol::create(str);
}

Value * c_keyword(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (Value::is_type(arg, Type::Keyword))
      return arg;

   if (!Value::is_type(arg, Type::String))
      error_expected(0, "string", name);

   const char * str = R<String>(arg).str();
   int len = (int)strlen(str);

   if (len == 0)
      error(0, name, "non empty string is required");

   else if (len >= buffer_size - 8)
      error(0, name, "name is too much long");

   if (str[0] != ':')
   {
      buffer[0] = ':';
      strcpy(buffer + 1, str);
      str = buffer;
   }

   for (const char * ptr = str; *ptr; ptr++)
   {
      if (is_non_atom(*ptr))
         error(0, name, "invalid character: '%c'", *ptr);
   }

   if (Symbol::is_symbol(str))
      error(0, name, "invalid name: '%s'", str);

   return Keyword::create(str);
}

Value * c_vector_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::Vector)
        ? Constant::True : Constant::False;
}

Value * c_vector(ListNode * args, const char * name)
{
   GCStack gcs;
   gcs.push(args);

   int cnt = args == nullptr ? 0 : args->count();

   VectorP vec = Vector::create(cnt);

   for (int i=0; args != nullptr; i++)
   {
      (*vec)[i] = args->get_car();
      args = args->get_cdr();
   }

   return vec;
}

Value * c_sequential_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_sequence(arg) ? Constant::True : Constant::False;
}

Value * assoc(AVLTree * map, ListNode * args, int sidx, const char * name)
{
   GCStack gcs;
   gcs.push(map);
   gcs.push(args);

   ValueP key = nullptr;

   for (int i=0; args != nullptr; i++)
   {
      if (i % 2 == 0)
      {
         key = args->get_car();
         switch (key->get_type())
         {
            case Type::Int:
            case Type::Keyword:
            case Type::String:
            case Type::Symbol:
               break;

            default:
               error_expected(sidx + i, "string/symbol/keyword/integer", name);
               break;
         }
      }
      else
      {
         map->insert(key, args->get_car());
         key = nullptr;
      }
      args = args->get_cdr();
   }

   if (key != nullptr)
      error(0, name, "missing value for last key");

   return map;
}

Value * c_assoc(ListNode * args, const char * name)
{
   check_args_V(args, 1, name);

   GCStack gcs;
   gcs.push(args);

   ValueP hm = args->get_car();
   args = args->get_cdr();

   if (!Value::is_type(hm, Type::AVLTree))
      error_expected(1, "map", name);

   AVLTreeP map = AVLTree::create(hm.R<AVLTree>().get_root());
   ValueP key = nullptr;

   return assoc(map, args, 2, name);
}

Value * c_dissoc(ListNode * args, const char * name)
{
   check_args_V(args, 1, name);

   GCStack gcs;
   gcs.push(args);

   ValueP hm = args->get_car();
   args = args->get_cdr();

   if (!Value::is_type(hm, Type::AVLTree))
      error_expected(1, "map", name);

   AVLTreeP map = AVLTree::create(hm.R<AVLTree>().get_root());
   ValueP key = nullptr;

   for (int i=2; args != nullptr; i++)
   {
      key = args->get_car();
      switch (key->get_type())
      {
         case Type::Int:
         case Type::Keyword:
         case Type::String:
         case Type::Symbol:
            break;

         default:
            error_expected(i, "string/symbol/keyword/integer", name);
            break;
      }

      map->remove(key);

      args = args->get_cdr();
   }

   return map;
}

Value * c_map_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::AVLTree)
        ? Constant::True : Constant::False;
}

Value * c_hash_map(ListNode * args, const char * name)
{
   GCStack gcs;
   gcs.push(args);

   AVLTreeP hm = AVLTree::create(nullptr);

   return assoc(hm, args, 1, name);
}

Value * c_get(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (arg1 == Constant::Nil)
      return Constant::Nil;

   if (!Value::is_type(arg1, Type::AVLTree))
      error_expected(0, "hash-map", name);

   if (arg2 == nullptr)
      return Constant::Nil;

   switch (arg2->get_type())
   {
      case Type::Int:
      case Type::Keyword:
      case Type::String:
      case Type::Symbol:
         break;

      default:
         return Constant::Nil;
   }

   TreeNode * res = R<AVLTree>(arg1).lookup(arg2);

   return res == nullptr ? Constant::Nil : res->get_data();
}

Value * c_contains_p(ListNode * args, const char * name)
{
   check_args_2(args, name);

   Value * arg1 = args->nth(0);
   Value * arg2 = args->nth(1);

   if (!Value::is_type(arg1, Type::AVLTree))
      error_expected(0, "hash-map", name);

   if (arg2 == nullptr)
      return Constant::False;

   switch (arg2->get_type())
   {
      case Type::Int:
      case Type::Keyword:
      case Type::String:
      case Type::Symbol:
         break;

      default:
         return Constant::False;
   }

   TreeNode * res = R<AVLTree>(arg1).lookup(arg2);

   return res != nullptr ? Constant::True : Constant::False;
}

Value * c_keys(ListNode * args, const char * name)
{
   check_args_1(args, name);

   GCStack gcs;

   ValueP arg = args->nth(0);

   if (!Value::is_type(arg, Type::AVLTree))
      error_expected(0, "hash-map", name);

   std::vector<Value *> keys;

   R<AVLTree>(arg).for_each([&](Value * key, Value * val)
                                { keys.push_back(key); });

   for (int i = 0; i < keys.size(); i++)
      gcs.push(keys[i]);

   ListP lst = List::create();

   for (int i = (int)keys.size() - 1; i >= 0; i--)
      lst->cons(keys[i]);

   return lst;
}

Value * c_vals(ListNode * args, const char * name)
{
   check_args_1(args, name);

   GCStack gcs;

   ValueP arg = args->nth(0);

   if (!Value::is_type(arg, Type::AVLTree))
      error_expected(0, "hash-map", name);

   std::vector<Value *> vals;

   R<AVLTree>(arg).for_each([&](Value * key, Value * val)
                                { vals.push_back(val); });

   for (int i = 0; i < vals.size(); i++)
      gcs.push(vals[i]);

   ListP lst = List::create();

   for (int i = (int)vals.size() - 1; i >= 0; i--)
      lst->cons(vals[i]);

   return lst;
}

Value * c_throw(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   throw MalException(arg);
}

Value * c_readline(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (!Value::is_type(arg, Type::String))
      error_expected(0, "string", name);

   std::string str;
   if (!readline(R<String>(arg).str(), str))
      return Constant::Nil;

   return String::create(str);
}

Value * c_time_ms(ListNode * args, const char * name)
{
   check_args_0(args, name);

   using namespace std::chrono;
   milliseconds ms = duration_cast<milliseconds>(
      high_resolution_clock::now().time_since_epoch()
   );

   return Int::create(ms.count());
}

Value * c_conj(ListNode * args, const char * name)
{
   GCStack gcs;
   gcs.push(args);

   ValueP seq = args->get_car();
   args = args->get_cdr();

   if (args == nullptr)
      return seq;

   ListP   lst;
   VectorP vec;

   if (Value::is_type(seq, Type::List))
   {
      lst = List::create(seq.R<List>().elements());

      while (args != nullptr)
      {
         lst->cons(args->get_car());

         args = args->get_cdr();
      }

      seq = lst;
   }
   else if (Value::is_type(seq, Type::Vector))
   {
      int cnt1 = seq.R<Vector>().length();
      int cnt2 = args->count();

      vec = Vector::create(cnt1 + cnt2);

      Value ** ptr1 = &(*vec)[0];
      Value ** ptr2 = &(seq.R<Vector>()[0]);
      memmove(ptr1, ptr2, cnt1 * sizeof(Value *));

      for (int i=0; i < cnt2 && args != nullptr; i++)
      {
         ptr1[cnt1 + i] = args->get_car();
         args = args->get_cdr();
      }

      seq = vec;
   }
   else
      error(0, "sequence", name);

   return seq;
}

Value * c_string_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::String)
        ? Constant::True : Constant::False;
}

Value * c_number_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_number(arg)
        ? Constant::True : Constant::False;
}

Value * c_int_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::Int)
        ? Constant::True : Constant::False;
}

Value * c_real_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   return Value::is_type(arg, Type::Real)
        ? Constant::True : Constant::False;
}

Value * c_int(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (Value::is_type(arg, Type::Real))
     return Int::create(R<Real>(arg).get_int());

   if (!Value::is_type(arg, Type::Int))
     error_expected(0, "number", name);

   return arg;
}

Value * c_real(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (Value::is_type(arg, Type::Int))
     return Real::create(R<Int>(arg).get_real());

   if (!Value::is_type(arg, Type::Real))
     error_expected(0, "number", name);

   return arg;
}

Value * c_fn_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (Value::is_type(arg, Type::Function) && !R<Function>(arg).is_macro())
      return Constant::True;

   if (Value::is_type(arg, Type::Closure) && !R<Closure>(arg).is_macro())
      return Constant::True;

   return Constant::False;
}

Value * c_macro_p(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (Value::is_type(arg, Type::Function) && R<Function>(arg).is_macro())
      return Constant::True;

   if (Value::is_type(arg, Type::Closure) && R<Closure>(arg).is_macro())
      return Constant::True;

   return Constant::False;
}

Value * c_seq(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   if (arg == Constant::Nil)
      return Constant::Nil;

   if (Value::is_type(arg, Type::List))
   {
      if (R<List>(arg).is_null())
         return Constant::Nil;

      return arg;
   }

   if (Value::is_type(arg, Type::Vector))
   {
      if (R<Vector>(arg).length() == 0)
         return Constant::Nil;

      return R<Vector>(arg).drop(0);
   }

   if (Value::is_type(arg, Type::String))
   {
      GCStack gcs;
      gcs.push(arg);

      int len = (int)strlen(R<String>(arg).str());
      if (len == 0)
         return Constant::Nil;

      std::string buf = " ";

      StringP str;
      ListP lst = List::create();

      for (int i=len - 1; i >= 0; i--)
      {
         ((char *)buf.c_str())[0] = R<String>(arg).str()[i];

         str = String::create(buf);
         lst->cons(str);
      }

      return lst;
   }

   error_expected(0, "nil/list/vector/string", name);

   return Constant::Nil;
}

Value * c_with_meta(ListNode * args, const char * name)
{
   check_args_2(args, name);

   GCStack gcs;

   ValueP arg1 = args->nth(0);
   ValueP arg2 = args->nth(1);

   if (!GC::is_in_pool_A(arg1))
      error(0, name, "unexpected type");

   ValueP val = nullptr;

   switch (arg1->get_type())
   {
      case Type::Int:
         val = Int::create(P<Int>(arg1));
         break;

      case Type::Real:
         val = Real::create(P<Real>(arg1));
         break;

      case Type::String:
         val = String::create(P<String>(arg1));
         break;

      case Type::List:
         val = List::create(P<List>(arg1));
         break;

      case Type::Vector:
         val = Vector::create(P<Vector>(arg1));
         break;

      case Type::Atom:
         val = Atom::create(P<Atom>(arg1)->get());
         break;

      case Type::AVLTree:
         val = AVLTree::create(P<AVLTree>(arg1)->get_root());
         break;

      case Type::Function:
         val = Function::create(P<Function>(arg1));
         break;

      case Type::Closure:
         val = Closure::create(P<Closure>(arg1));
         break;

      default:
         error(0, name, "unexpected type");
   }

   gMetaData[val] = arg2;

   return val;
}

Value * c_meta(ListNode * args, const char * name)
{
   check_args_1(args, name);

   Value * arg = args->nth(0);

   auto it = gMetaData.find(arg);

   if (it == gMetaData.end())
      return Constant::Nil;

   return it->second;
}

Value * c_gc(ListNode * args, const char * name)
{
   check_args_0(args, name);

   GC::gc_run();

   return Int::create(GC::gc_count());
}

Value * c_gc_info(ListNode * args, const char * name)
{
   check_args_0(args, name);

   int     gc_count = GC::gc_count();
   int64_t gc_time = GC::gc_time();

   GCStack gcs;

   AVLTreeP hm = AVLTree::create(nullptr);

   KeywordP key = Keyword::create(":gc-count");
   IntP num = Int::create(gc_count);
   hm->insert(key, num);

   key = Keyword::create(":gc-time");
   num = Int::create(gc_time);
   hm->insert(key, num);

   return hm;
}

}
