#pragma once

#include <iostream>
#include <memory>
#include <vector>
#include <tuple>
#include <regex>

#include "types.h"

namespace MAL
{

extern std::regex re_real_number;
extern std::regex re_int_number;

inline bool is_whitespace(char ch)
{
   return (unsigned char)ch <= ' ' || ch == ',';
}

inline bool is_special(char ch)
{
   return strchr("()[]{}'`~^@", ch) != nullptr;
}

inline bool is_non_atom(char ch)
{
   return is_whitespace(ch)
       || strchr("()[]{}'\"`,;", ch) != nullptr;
}

class reader
{
public:
   reader(const std::string & str);
   reader(std::istream & stream);
   ~reader();

   Value * read_form();

   int get_row() const { return _row; }
   int get_col() const { return _col; }

private:
   List * read_list();
   Vector * read_vector();
   AVLTree * read_hashmap();

   Value * get_form();

   const char * next_token();
   void unget_token()  { _is_unget = true; }

   const char * get_string();
   const char * get_atom();

   char next_char();

   void skip_whitespace();
   void skip_comment();

private:
   std::istream * _input;
   bool _stream_owner;

   int _row;
   int _col;

   bool _is_unget;
   std::vector<char> _token;
};

bool readline(const char * prompt, std::string & output);

}
