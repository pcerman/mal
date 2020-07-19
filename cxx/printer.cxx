#include <iostream>
#include <sstream>

#include "printer.h"

namespace MAL
{

std::string pr_str(Value * val, bool readably)
{
   if (val == nullptr)
      return std::string();

   std::ostringstream oss;
   val->pr_str(oss, readably);

   return oss.str();
}

#ifdef _DEBUG

void print(Value * val)
{
   std::cout << ";;; ";

   if (val == nullptr)
      std::cout << "<nullptr>";
   else
      val->pr_str(std::cout, true);

   std::cout << std::endl;
}

#endif

}
