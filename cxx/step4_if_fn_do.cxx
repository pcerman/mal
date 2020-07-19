#include <iostream>

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

Value * EVAL(Value * arg, EnvPtr env)
{
   if (Value::is_type(arg, Type::List))
   {
      List * lst = (List *)arg;

      Value * fst = lst->first();
      if (fst == Symbol::Def)
      {
         if (lst->length() != 3)
            error(0, Symbol::Def, "syntax error");

         Value * sym = lst->nth(1);
         if (!Value::is_type(sym, Type::Symbol))
            error_expected(1, "symbol", Symbol::Def);

         Value * val = EVAL(lst->nth(2), env);

         env->set((Symbol *)sym, val);

         return val;
      }

      if (fst == Symbol::Let)
      {
         if (lst->length() != 3)
            error(0, Symbol::Let, "syntax error");

         Value * vars = lst->nth(1);
         if (!Value::is_sequence(vars))
            error_expected(1, "sequence of variable definition", Symbol::Let);

         Sequence * seq = (Sequence *)vars;
         int count =  seq->length();

         if (count % 2 != 0)
            error(1, Symbol::Let, "variable definition syntax error");

         EnvPtr env2( new Env(env) );

         for (int i=0; i < count; i+=2)
         {
            Value * var = seq->nth(i);
            Value * val = seq->nth(i + 1);

            if (!Value::is_type(var, Type::Symbol))
               error_expected(i+1, "variable", Symbol::Let);

            env2->set((Symbol *)var, EVAL(val, env2));
         }

         Value * ret = EVAL(lst->nth(2), env2);

         env = nullptr;

         return ret;
      }

      if (fst == Symbol::If)
      {
         int len = lst->length();
         if (len != 3 && len != 4)
            error(0, Symbol::If, "syntax error");

         Value * tst = EVAL(lst->nth(1), env);
         if (tst != nullptr &&
             tst != Constant::Nil &&
             tst != Constant::False)
         {
            return EVAL(lst->nth(2), env);
         }
         else if (len == 4)
         {
            return EVAL(lst->nth(3), env);
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

         Sequence * seq = (Sequence *)pars;
         int count = seq->length();

         for (int i=0; i < count; i++)
         {
            Value * par = seq->nth(i);
            if (!Value::is_type(par, Type::Symbol) &&
                !(par == Symbol::Amp && i == count - 2))
            {
               error(i+1, Symbol::Fn, "parameter has to be symbol");
            }
         }

         return Closure::create(env, seq, lst->nth(2));
      }

      if (fst == Symbol::Do)
      {
         Value * last = Constant::Nil;

         (lst->rest())->for_each([&](Value * elm) { last = EVAL(elm, env); });

         return last;
      }

      Value * ast = eval_ast(arg, env);

      if (ast != nullptr)
      {
         if (ast->get_type() == Type::List)
         {
            List * lst = (List *)ast;

            if (lst->is_null())
               return lst;

            Value * fst = lst->first();

            if (Value::is_type(fst, Type::Function))
            {
               ListNode * args = lst->elements()->get_cdr();
               return R<Function>(fst).eval(args);
            }

            if (Value::is_type(fst, Type::Closure))
            {
               ListNode * args = lst->elements()->get_cdr();
               return R<Closure>(fst).eval(args);
            }

            error_expected(0, "function/closure", "apply");
         }
      }

      return ast;
   }

   return eval_ast(arg, env);
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
   }
}

void set_top_level_env(EnvPtr & env)
{
   env->set("+",       Function::create("+",       c_add));
   env->set("-",       Function::create("-",       c_sub));
   env->set("*",       Function::create("*",       c_mul));
   env->set("/",       Function::create("/",       c_div));
   env->set("<",       Function::create("<",       c_lt));
   env->set("<=",      Function::create("<=",      c_le));
   env->set(">",       Function::create(">",       c_gt));
   env->set(">=",      Function::create(">=",      c_ge));
   env->set("=",       Function::create("=",       c_equal));
   env->set("list",    Function::create("list",    c_list));
   env->set("list?",   Function::create("list?",   c_list_p));
   env->set("empty?",  Function::create("empty?",  c_empty_p));
   env->set("count",   Function::create("count",   c_count));
   env->set("str",     Function::create("str",     c_str));
   env->set("pr-str",  Function::create("pr-str",  c_pr_str));
   env->set("prn",     Function::create("prn",     c_prn));
   env->set("println", Function::create("println", c_println));

   rep("(def! not (fn* [x] (if x false true)))", env);
}

}

int main(int argc, char * argv[])
{
   MAL::GC::initialize(1024*1024);
   MAL::EnvPtr env(new MAL::Env());

   MAL::set_top_level_env(env);
   MAL::repl("user> ", env);

   return 0;
}
