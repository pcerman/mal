#include <iostream>
#include <fstream>

#include "types.h"
#include "reader.h"
#include "printer.h"
#include "core.h"
#include "env.h"
#include "gc.h"

namespace MAL
{

Value * READ(const std::string & arg)
{
   reader rdr(arg);

   return rdr.read_form();
}

Value * eval_ast(Value * arg, EnvPtr & env)
{
   if (arg == nullptr)
      return nullptr;

   if (arg->get_type() == Type::Symbol)
   {
      return env->get((Symbol *)arg);
   }

   if (arg->get_type() == Type::List)
   {
      return ((List *) arg)->map([&](Value * elm)
                                 { return EVAL(elm, env); });
   }

   if (arg->get_type() == Type::Vector)
   {
      return ((Vector *) arg)->map([&](Value * elm)
                                   { return EVAL(elm, env); });
   }

   if (arg->get_type() == Type::AVLTree)
   {
      return ((AVLTree *)arg)->map([&](Value * elm)
                                   { return EVAL(elm, env); });
   }

   return arg;
}

Value * EVAL(Value * args, EnvPtr env)
{
   EnvStack ens;

   GCStack gcs;
   gcs.push(args);

   ValueP arg = args;

   ListP     lst;
   ValueP    fst;
   ValueP    var;
   ValueP    val;
   SequenceP seq;

   for (;;)
   {
      lst = nullptr;
      fst = nullptr;
      var = nullptr;
      val = nullptr;
      seq = nullptr;

      if (Value::is_type(arg, Type::List))
      {
         lst = arg.P<List>();
         fst = lst->first();

         if (fst == Symbol::Def)
         {
            if (lst->length() != 3)
               error(0, Symbol::Def, "syntax error");

            var = lst->nth(1);
            if (!Value::is_type(var, Type::Symbol))
               error_expected(1, "symbol", Symbol::Def);

            val = EVAL(lst->nth(2), env);

            env->set(var.P<Symbol>(), val);

            return val;
         }

         if (fst == Symbol::Let)
         {
            if (lst->length() != 3)
               error(0, Symbol::Let, "syntax error");

            Value * vars = lst->nth(1);
            if (!Value::is_sequence(vars))
               error_expected(1, "sequence of variable definition", Symbol::Let);

            seq = (Sequence *)vars;
            int count =  seq->length();

            if (count % 2 != 0)
               error(1, Symbol::Let, "variable definition syntax error");

            env = new Env(env);
            ens.push(env);

            for (int i=0; i < count; i+=2)
            {
               var = seq->nth(i);
               val = seq->nth(i + 1);

               if (!Value::is_type(var, Type::Symbol))
                  error_expected(i+1, "variable", Symbol::Let);

               env->set(var.P<Symbol>(), EVAL(val, env));
            }

            arg = lst->nth(2);
            continue;
         }

         if (fst == Symbol::If)
         {
            int len = lst->length();
            if (len != 3 && len != 4)
               error(0, Symbol::If, "syntax error");

            val = EVAL(lst->nth(1), env);

            if (val != nullptr &&
                val != Constant::Nil &&
                val != Constant::False)
            {
               arg = lst->nth(2);
               continue;
            }
            else if (len == 4)
            {
               arg = lst->nth(3);
               continue;
            }
            else
            {
               return Constant::Nil;
            }
         }

         if (fst == Symbol::Fn)
         {
            if (lst->length() != 3)
               error(0, Symbol::Fn, "syntax error");

            Value * pars = lst->nth(1);
            if (!Value::is_sequence(pars))
               error_expected(1, "sequence of symbols", Symbol::Fn);

            seq = (Sequence *)pars;
            int count = seq->length();

            for (int i=0; i < count; i++)
            {
               var = seq->nth(i);
               if (!Value::is_type(var, Type::Symbol) &&
                   !(var == Symbol::Amp && i == count - 2))
               {
                  error(i+1, Symbol::Fn, "parameter has to be symbol");
               }
            }

            return Closure::create(env, seq, lst->nth(2));
         }

         if (fst == Symbol::Do)
         {
            val = Constant::Nil;

            lst = lst->rest();
            lst->for_each([&](Value * elm)
                           {
                              Value * pv = val;
                              val = elm;
                              EVAL(pv, env);
                           });
            arg = val;
            continue;
         }

         arg = eval_ast(arg, env);

         if (arg != nullptr)
         {
            if (arg->get_type() == Type::List)
            {
               lst = P<List>(arg);

               if (lst->is_null())
                  return lst;

               fst = lst->first();

               if (Value::is_type(fst, Type::Function))
                  return fst.R<Function>().eval(lst->elements()->get_cdr());

               if (Value::is_type(fst, Type::Closure))
               {
                  env = fst.R<Closure>().create_env(lst->elements()->get_cdr());
                  ens.push(env);

                  arg = fst.R<Closure>().get_body();
                  continue;
               }

               error_expected(0, "function/closure", "apply");
            }
         }

         return arg;
      }

      return eval_ast(arg, env);
   }
}

std::string PRINT(Value * arg)
{
   return pr_str(arg, true);
}

std::string rep(const std::string & arg, EnvPtr & env)
{
   return PRINT(EVAL(READ(arg), env));
}

void repl(const char * prompt, EnvPtr & env)
{
   std::string input;

   while (readline(prompt, input))
   {
      try
      {
         std::cout << rep(input, env) << std::endl;
      }
      catch (MalException & ex)
      {
         std::cout << "Error: " << ex.get_msg() << std::endl;
      }
      catch (const char * msg)
      {
         std::cout << "Fatal Error: " << msg << std::endl;
         break;
      }
   }
}

void set_top_level_env(int argc, char * argv[])
{
   gTopEnv->set("+",       Function::create("+",       c_add));
   gTopEnv->set("-",       Function::create("-",       c_sub));
   gTopEnv->set("*",       Function::create("*",       c_mul));
   gTopEnv->set("/",       Function::create("/",       c_div));
   gTopEnv->set("<",       Function::create("<",       c_lt));
   gTopEnv->set("<=",      Function::create("<=",      c_le));
   gTopEnv->set(">",       Function::create(">",       c_gt));
   gTopEnv->set(">=",      Function::create(">=",      c_ge));
   gTopEnv->set("=",       Function::create("=",       c_equal));
   gTopEnv->set("list",    Function::create("list",    c_list));
   gTopEnv->set("list?",   Function::create("list?",   c_list_p));
   gTopEnv->set("empty?",  Function::create("empty?",  c_empty_p));
   gTopEnv->set("count",   Function::create("count",   c_count));
   gTopEnv->set("str",     Function::create("str",     c_str));
   gTopEnv->set("pr-str",  Function::create("pr-str",  c_pr_str));
   gTopEnv->set("prn",     Function::create("prn",     c_prn));
   gTopEnv->set("println", Function::create("println", c_println));

   gTopEnv->set("read-string", Function::create("read-string", c_read_string));
   gTopEnv->set("slurp",       Function::create("slurp",       c_slurp));
   gTopEnv->set("load-file",   Function::create("load-file",   c_load_file));

   gTopEnv->set("eval",        Function::create("eval",   c_eval));
   gTopEnv->set("atom",        Function::create("atom",   c_atom));
   gTopEnv->set("atom?",       Function::create("atom?",  c_atom_p));
   gTopEnv->set("deref",       Function::create("deref",  c_deref));
   gTopEnv->set("reset!",      Function::create("reset!", c_reset));
   gTopEnv->set("swap!",       Function::create("swap!",  c_swap));

   //-------------------------------------------------------------------

   rep("(def! not (fn* [x] (if x false true)))", gTopEnv);

   //-------------------------------------------------------------------

   GCStack gcs;

   ListP args = MAL::List::create();
   StringP str;

   for (int i=argc-1; i > 1; i--)
   {
      str = String::create(std::string(argv[i]));
      args->cons(str);
   }

   gTopEnv->set("*ARGV*", args);
}

}

int main(int argc, char * argv[])
{
   MAL::GC::initialize(1024*1024, true);

   MAL::EnvStack env_stack(MAL::gTopEnv);
   MAL::set_top_level_env(argc, argv);

   if (argc == 1)
   {
      MAL::repl("user> ", MAL::gTopEnv);
   }
   else
   {
      std::ifstream inp(argv[1]);
      if (!inp.is_open())
      {
         std::cerr << "file '" << argv[1] << "' does not exist";
         return 1;
      }

      eval_stream(inp, MAL::gTopEnv);

      inp.close();
   }

   return 0;
}
