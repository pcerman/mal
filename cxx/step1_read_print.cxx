#include <iostream>

#include "types.h"
#include "reader.h"
#include "printer.h"
#include "gc.h"

namespace MAL
{

Value * READ(const std::string & arg)
{
   reader rdr(arg);

   return rdr.read_form();
}

Value * EVAL(Value * arg, EnvPtr env)
{
   return arg;
}

std::string PRINT(Value * arg)
{
   return pr_str(arg, true);
}

std::string rep(const std::string & arg)
{
   EnvPtr env;
   return PRINT(EVAL(READ(arg), env));
}

void repl(const char * prompt)
{
   std::string input;

   while (readline(prompt, input))
   {
      try
      {
         std::cout << rep(input) << std::endl;
      }
      catch (MalException & ex)
      {
         std::cout << "Error: " << ex.get_msg() << std::endl;
      }
   }
}

}

int main(int argc, char * argv[])
{
   MAL::GC::initialize(1024*1024);

   MAL::repl("user> ");
   return 0;
}
