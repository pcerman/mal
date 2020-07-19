#include <iostream>
#include <string>
#include <algorithm>

#include <readline/readline.h>
#include <readline/history.h>

using String = std::string;

String READ(const String arg)
{
   return arg;
}

String EVAL(const String arg)
{
   return arg;
}

String PRINT(const String arg)
{
   return arg;
}

String rep(const String arg)
{
   return PRINT(EVAL(READ(arg)));
}

bool readline(const char * prompt, String & output)
{
   char * line = readline(prompt);
   if (line == nullptr)
      return false;

   output = line;

   auto pos = std::find_if(output.cbegin(), output.cend(),
                           [](const char ch) { return (ch > ' '); });

   if (pos != output.cend())
      add_history(line);

   free(line);

   return true;
}

int main(int argc, char * argv[])
{
   String input;

   while (readline("user> ", input))
   {
      std::cout << rep(input) << std::endl;
   }
   return 0;
}
