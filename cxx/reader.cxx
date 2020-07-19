#include <iostream>
#include <algorithm>
#include <istream>
#include <sstream>

#ifndef _MSC_VER
#include <readline/readline.h>
#include <readline/history.h>
#include <readline/tilde.h>
#endif

#include "reader.h"
#include "gc.h"

namespace MAL
{

std::regex re_real_number("^[+-]?(?:\\d+\\.?\\d*|\\.\\d+)([eE][-+]?\\d+)?$");
std::regex re_int_number("^[+-]?\\d+$");

//----------------------------------------------------------------------

reader::reader(const std::string & str)
{
   _input = new std::istringstream(str);
   _stream_owner = true;

   _row = 0;
   _col = 0;

   _is_unget = false;
}

reader::reader(std::istream & stream)
{
   _input = &stream;
   _stream_owner = false;

   _row = 1;
   _col = 0;

   _is_unget = false;
}

reader::~reader()
{
   if (_stream_owner && _input != nullptr)
   {
      delete _input;
   }
}

Value * reader::read_form()
{
   const char * token = next_token();

   if (token == nullptr || *token == 0)
      return nullptr;

   switch (token[0])
   {
      case '(':
         return read_list();

      case '[':
         return read_vector();

      case '{':
         return read_hashmap();

      case '~':
         {
            GCStack gcs;

            ListP lst = List::create();
            ValueP val = nullptr;

            char ch2 = token[1];

            val = get_form();
            lst->cons(val);

            if (ch2 == '@')
               lst->cons(Symbol::splice_unquote);
            else
               lst->cons(Symbol::unquote);

            return lst;
         }

      case '@':
         {
            GCStack gcs;

            ListP lst = List::create();
            ValueP val = nullptr;

            val = get_form();

            lst->cons(val);
            lst->cons(Symbol::deref);

            return lst;
         }

      case '^':
         {
            GCStack gcs;

            ListP lst = List::create();
            ValueP val1 = nullptr;
            ValueP val2 = nullptr;

            val1 = get_form();
            val2 = get_form();

            lst->cons(val1);
            lst->cons(val2);
            lst->cons(Symbol::with_meta);

            return lst;
         }

      case '\'':
         {
            GCStack gcs;

            ListP lst = List::create();
            ValueP val = nullptr;

            val = get_form();

            lst->cons(val);
            lst->cons(Symbol::quote);

            return lst;
         }

      case '`':
         {
            GCStack gcs;

            ListP lst = List::create();
            ValueP val = nullptr;

            val = get_form();

            lst->cons(val);
            lst->cons(Symbol::quasiquote);

            return lst;
         }

      case '"':
         return String::create(token);

      default:
      {
         if (is_non_atom(token[0]))
            error(*this, "unexpected character '%c'", token[0]);

         Keyword * kwd = nullptr;

         if (token[0] == ':')
            return Keyword::create(token);
         else if (std::regex_match(token, re_int_number))
            return Int::create(atoll(token));
         else if (std::regex_match(token, re_real_number))
            return Real::create(atof(token));
         else if (strcmp(token, "nil") == 0)
            return Constant::Nil;
         else if (strcmp(token, "true") == 0)
            return Constant::True;
         else if (strcmp(token, "false") == 0)
            return Constant::False;
         else if (Keyword::is_keyword(token, &kwd))
            return kwd;
         else
            return Symbol::create(token);
      }
   }

   return nullptr;
}

List * reader::read_list()
{
   GCStack gcs;

   ListNodeP beg = nullptr;
   ListNodeP end = nullptr;
   ListNodeP nod = nullptr;

   for (;;)
   {
      const char * token = next_token();

      if (token == nullptr)
         error(*this, "unbalanced list");

      if (token[0] == ')')
         break;

      if (token[0] == ']' || token[0] == '}')
         error(*this, "unbalanced list");

      unget_token();

      nod = ListNode::create(read_form());

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
      nod = nullptr;
   }

   return List::create(beg);
}

Vector * reader::read_vector()
{
   GCStack gcs;

   const int max_size = 2048;

   std::vector<Value *> elms;
   elms.reserve(max_size + 10);

   for (;;)
   {
      if (elms.size() >= max_size)
         error(*this, "the maximal size of vector literal is reached");

      const char * token = next_token();

      if (token == nullptr)
         error(*this, "unbalanced vector");

      if (token[0] == ']')
         break;

      if (token[0] == ')' || token[0] == '}')
         error(*this, "unbalanced vector");

      unget_token();

      Value * val = read_form();

      elms.push_back(val);

      gcs.push(elms.back());
   }

   return Vector::create(elms);
}

AVLTree * reader::read_hashmap()
{
   GCStack gcs;

   AVLTreeP hm = AVLTree::create(nullptr);

   ValueP key = nullptr;
   ValueP data = nullptr;

   for (;;)
   {
      const char * token = next_token();

      if (token == nullptr)
         error(*this, "unbalanced hashmap");

      if (token[0] == '}')
      {
         if (key != nullptr)
            error(*this, "hashmap - even number of elements is required");
         break;
      }

      if (token[0] == ')' || token[0] == ']')
         error(*this, "unbalanced hashmap");

      unget_token();

      if (key == nullptr)
      {
         key = read_form();

         switch (key->get_type())
         {
            case Type::Int:
            case Type::Keyword:
            case Type::String:
            case Type::Symbol:
               break;

            default:
               error(*this, "hashmap - only string/symbol/keyword/integer is expected for key");
         }
      }
      else
      {
         data = read_form();

         hm->insert(key, data);

         key = nullptr;
         data = nullptr;
      }
   }

   return hm;
}

Value * reader::get_form()
{
   Value * form = read_form();
   if (form == nullptr)
      error(*this, "form is expected");

   return form;
}

char reader::next_char()
{
   char ch = 0;

   if (_input->eof() || !_input->get(ch))
      return 0;

   switch (ch)
   {
      case '\r':
         if (_input->peek() == '\n')
            break;

      case '\n':
         _row++;
         _col = 0;
         break;

      default:
         _col++;
   }

   return ch;
}

const char * reader::next_token()
{
   if (_is_unget)
   {
      _is_unget = false;
      return _token.data();
   }

   _token.clear();

   char ch = 0;

   for (;;)
   {
      skip_whitespace();
      if (_input->eof())
         return nullptr;

      ch = (char)_input->peek();

      if (ch == ';')
         skip_comment();
      else
         break;
   }

   if (ch == '~')
   {
      _token.push_back(ch);

      if (next_char() && _input->peek() == '@')
      {
         _token.push_back('@');
         next_char();
      }

      _token.push_back(0);

      return _token.data();
   }

   if (is_special(ch))
   {
      next_char();
      _token.push_back(ch);
      _token.push_back(0);
      return _token.data();
   }

   if (ch == '"')
      return get_string();

   return get_atom();
}

const char * reader::get_string()
{
   int data = _input->peek();
   if (data == EOF)
      return nullptr;

   char ch = (char)data;
   _token.push_back(ch);

   next_char();

   bool add_next = false;
   bool is_ok = false;

   while (!_input->eof())
   {
      data = _input->peek();
      if (data == EOF)
         break;

      ch = (char)data;
      //if (ch == '\r' || ch == '\n')
      //   break;

      _token.push_back(ch);

      next_char();

      if (add_next)
      {
         add_next = false;
      }
      else if (ch == '\\')
      {
         add_next = true;
      }
      else if (ch == '"')
      {
         is_ok = true;
         break;
      }
   }

   _token.push_back(0);

   if (!is_ok)
      error(*this, "unbalanced string");

   return _token.data();
}

const char * reader::get_atom()
{
   int data = _input->peek();
   if (data == EOF)
      return nullptr;

   char ch = (char)data;
   _token.push_back(ch);

   next_char();

   while (!_input->eof())
   {
      data = _input->peek();
      if (data == EOF)
         break;

      char ch = (char)data;

      if (is_non_atom(ch))
         break;

      _token.push_back(ch);

      next_char();
   }

   _token.push_back(0);

   return _token.data();
}

void reader::skip_whitespace()
{
   while (!_input->eof())
   {
      char ch = next_char();
      if ((unsigned char)ch > ' ' && ch != ',')
      {
         _input->unget();
         break;
      }
   }
}

void reader::skip_comment()
{
   while (!_input->eof())
   {
      char ch = next_char();
      switch (ch)
      {
         case '\r':
            if (_input->peek() != '\n')
               return;
            break;

         case '\n':
            return;
      }
   }
}

bool readline(const char * prompt, std::string & output)
{
#ifdef _MSC_VER
    static char buffer[2048];

    if (std::cin.eof())
        return false;

    std::cout << prompt;

    std::cin.getline(buffer, std::size(buffer) - 1);

    output = buffer;
#else
   static std::string history_file = "~/.mal-history";
   static bool history_loaded = false;

   if (!history_loaded)
   {
      char * filename = tilde_expand(history_file.c_str());
      history_file = filename;
      free(filename);

      read_history(history_file.c_str());
      history_loaded = true;
   }

   char * line = ::readline(prompt);
   if (line == nullptr)
      return false;

   output = line;

   auto pos = std::find_if(output.cbegin(), output.cend(),
                           [](const char ch) { return (ch > ' '); });

   if (pos != output.cend())
   {
      add_history(line);
      append_history(1, history_file.c_str());
   }

   free(line);
#endif

   return true;
}

}
