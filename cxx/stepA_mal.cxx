#include <iostream>
#include <fstream>

#include "types.h"
#include "reader.h"
#include "printer.h"
#include "core.h"
#include "env.h"
#include "gc.h"

#define USE_QQUOTE

using namespace std::string_literals;

namespace MAL
{

Value * READ(const std::string & arg)
{
   reader rdr(arg);

   return rdr.read_form();
}

#ifdef USE_QQUOTE

Value * qquote(Value * arg, EnvPtr & env)
{
   GCStack gsc;
   gsc.push(arg);

   if (Value::is_type(arg, Type::Vector))
   {
      ListNode * nod = P<Vector>(arg)->to_node_list();
      if (nod == nullptr)
         return arg;

      arg = List::create(nod);
   }

   if (!Value::is_type(arg, Type::List) || R<List>(arg).is_null())
      return arg;

   Value * fst = R<List>(arg).first();

   if (fst == Symbol::unquote)
   {
      if (R<List>(arg).length() != 2)
         error(0, Symbol::unquote, "invalid syntax");

      return EVAL(R<List>(arg).nth(1), env);
   }

   if (fst == Symbol::splice_unquote)
   {
      error(0, Symbol::splice_unquote,
            "invalid syntax - splice is not in list");
   }

   ListNodeP beg;
   ListNodeP end;
   ListNodeP nod;

   R<List>(arg).for_each([&](Value * elm)
      {
         if (Value::is_type(elm, Type::List))
         {
            Value * sym = R<List>(elm).first();

            if (sym == Symbol::unquote)
            {
               if (R<List>(elm).length() != 2)
                  error(0, Symbol::unquote, "invalid syntax");

               Value * val = EVAL(R<List>(elm).nth(1), env);

               nod = ListNode::create(val);
            }
            else if (sym == Symbol::splice_unquote)
            {
               if (R<List>(elm).length() != 2)
                  error(0, Symbol::splice_unquote, "invalid syntax");

               Value * val = EVAL(R<List>(elm).nth(1), env);

               if (!Value::is_sequence(val))
                  error(0, Symbol::splice_unquote, "result is not sequence");

               if (Value::is_type(val, Type::List))
                  nod = R<List>(val).elements();
               else if (Value::is_type(val, Type::Vector))
                  nod = R<Vector>(val).to_node_list();
               else
                  nod = nullptr;
            }
            else
            {
               nod = ListNode::create(qquote(elm, env));
            }
         }
         else
         {
            nod = ListNode::create(qquote(elm, env));
         }

         if (nod != nullptr)
         {
            if (beg == nullptr)
               beg = nod;
            else
               end->set_cdr(nod);

            end = nod->last_node();
         }
      });


   return List::create(beg);
}

#else

Value * quasiquote(Value * arg)
{
   GCStack gsc;
   gsc.push(arg);

   if (Value::is_type(arg, Type::Vector))
   {
      ListNode * nod = P<Vector>(arg)->to_node_list();
      if (nod == nullptr)
         return arg;

      arg = List::create(nod);
   }

   if (!Value::is_type(arg, Type::List))
   {
      ListP lst = List::create();

      lst->cons(arg);
      lst->cons(Symbol::quote);

      return lst;
   }

   if (R<List>(arg).is_null())
      return arg;

   ValueP fst = R<List>(arg).first();

   if (fst == Symbol::unquote)
   {
      if (R<List>(arg).length() != 2)
         error(0, Symbol::unquote, "invalid syntax");

      return R<List>(arg).nth(1);
   }

   if (fst == Symbol::splice_unquote)
   {
      error(0, Symbol::splice_unquote,
            "invalid syntax - splice is not in list");
   }

   ValueP car;
   ValueP cdr;
   ValueP sym;

   ListP exp;

   if (Value::is_type(fst, Type::List))
   {
      sym = fst.R<List>().first();

      if (sym == Symbol::splice_unquote)
      {
         if (fst.R<List>().length() != 2)
         {
            error(0, Symbol::splice_unquote, "invalid syntax");
         }

         cdr = quasiquote(R<List>(arg).rest());

         exp = List::create();

         exp->cons(cdr);
         exp->cons(fst.R<List>().nth(1));
         exp->cons(Symbol::concat);

         return exp;
      }
   }

   car = quasiquote(R<List>(arg).first());
   cdr = quasiquote(R<List>(arg).rest());

   exp = List::create();

   exp->cons(cdr);
   exp->cons(car);
   exp->cons(Symbol::cons);

   return exp;
}

#endif

Value * is_macro_call(Value * ast, EnvPtr & env)
{
   if (!Value::is_type(ast, Type::List))
      return nullptr;

   if (R<List>(ast).is_null())
      return nullptr;

   Value * fst = R<List>(ast).first();

   if (!Value::is_type(fst, Type::Symbol))
      return nullptr;

   Value * mac = env->lookup(P<Symbol>(fst));

   if (Value::is_type(mac, Type::Function))
   {
      if (R<Function>(mac).is_macro())
         return mac;
   }
   else if (Value::is_type(mac, Type::Closure))
   {
      if (R<Closure>(mac).is_macro())
         return mac;
   }

   return nullptr;
}

Value * macroexpand(Value * ast, EnvPtr & env)
{
   GCStack gcs;
   gcs.push(ast);

   ValueP mac = is_macro_call(ast, env);

   while (mac != nullptr)
   {
      ListNode * args = R<List>(ast).elements()->get_cdr();

      if (Value::is_type(mac, Type::Function))
      {
         ast = mac.R<Function>().eval(args);
      }
      else if (Value::is_type(mac, Type::Closure))
      {
         ast = mac.R<Closure>().eval(args);
      }

      mac = is_macro_call(ast, env);
   }

   return ast;
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

      arg = macroexpand(arg, env);

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

         if (fst == Symbol::Defmacro)
         {
            if (lst->length() != 3)
               error(0, Symbol::Defmacro, "syntax error");

            var = lst->nth(1);
            if (!Value::is_type(var, Type::Symbol))
               error_expected(1, "symbol", Symbol::Defmacro);

            val = EVAL(lst->nth(2), env);
            if (Value::is_type(val, Type::Function))
            {
               val = Function::create(val.P<Function>());
               val.R<Function>().set_macro();
            }
            else if (Value::is_type(val, Type::Closure))
            {
               val = Closure::create(val.P<Closure>());
               val.R<Closure>().set_macro();
            }
            else
               error_expected(2, "function", Symbol::Defmacro);

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

         if (fst == Symbol::Macroexpand)
         {
            if (lst->length() != 2)
               error(0, fst, "syntax error");

            return macroexpand(lst->nth(1), env);
         }

         if (fst == Symbol::quote)
         {
            if (lst->length() != 2)
               error(0, Symbol::quote, "syntax error");

            return lst->nth(1);
         }

         if (fst == Symbol::quasiquote)
         {
            if (lst->length() != 2)
               error(0, Symbol::quasiquote, "syntax error");
#ifdef USE_QQUOTE
            return qquote(lst->nth(1), env);
#else
            arg = quasiquote(lst->nth(1));
            continue;
#endif
         }

         if (fst == Symbol::Try)
         {
            int len = lst->length();
            if (len != 2 && len != 3)
               error(0, Symbol::Try, "syntax error");

            if (len == 3)
            {
               val = lst->nth(2);
               if (!Value::is_type(val, Type::List))
                  error(2, Symbol::Try, "syntax error");

               if (val.R<List>().length() != 3)
                  error_expected(2, "catch*", Symbol::Try);

               if (val.R<List>().nth(0) != Symbol::Catch)
                  error_expected(2, "catch*", Symbol::Try);

               var = val.R<List>().nth(1);
               if (!Value::is_type(var, Type::Symbol))
                  error_expected(1, "symbol", Symbol::Catch);
            }

            try
            {
               return EVAL(lst->nth(1), env);
            }
            catch (MalException & ex)
            {
               fst = ex.get_value();

               if (len == 2)
                  return fst;

               arg = val.R<List>().nth(2);

               env = new Env(env);
               ens.push(env);

               env->set(var.P<Symbol>(), fst);
            }
            catch (const char * msg)
            {
               fst = String::create(msg);

               if (len == 2)
                  return fst;

               arg = val.R<List>().nth(2);

               env = new Env(env);
               ens.push(env);

               env->set(var.P<Symbol>(), fst);
            }

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
   std::string input = "(println (str \"Mal [\" *host-language* \"]\"))"s;

   rep(input, env);

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

void top_env_add(Function * fun)
{
   gTopEnv->set(fun->get_name(), fun);
}

void set_top_level_env(int argc, char * argv[])
{
   top_env_add(Function::create("+",           c_add));
   top_env_add(Function::create("-",           c_sub));
   top_env_add(Function::create("*",           c_mul));
   top_env_add(Function::create("/",           c_div));
   top_env_add(Function::create("<",           c_lt));
   top_env_add(Function::create("<=",          c_le));
   top_env_add(Function::create(">",           c_gt));
   top_env_add(Function::create(">=",          c_ge));
   top_env_add(Function::create("=",           c_equal));
   top_env_add(Function::create("list",        c_list));
   top_env_add(Function::create("list?",       c_list_p));
   top_env_add(Function::create("empty?",      c_empty_p));
   top_env_add(Function::create("count",       c_count));
   top_env_add(Function::create("str",         c_str));
   top_env_add(Function::create("pr-str",      c_pr_str));
   top_env_add(Function::create("prn",         c_prn));
   top_env_add(Function::create("println",     c_println));
   top_env_add(Function::create("read-string", c_read_string));
   top_env_add(Function::create("load-file",   c_load_file));
   top_env_add(Function::create("slurp",       c_slurp));
   top_env_add(Function::create("eval",        c_eval));
   top_env_add(Function::create("atom",        c_atom));
   top_env_add(Function::create("atom?",       c_atom_p));
   top_env_add(Function::create("deref",       c_deref));
   top_env_add(Function::create("reset!",      c_reset));
   top_env_add(Function::create("swap!",       c_swap));
   top_env_add(Function::create("cons",        c_cons));
   top_env_add(Function::create("concat",      c_concat));
   top_env_add(Function::create("nth",         c_nth));
   top_env_add(Function::create("first",       c_first));
   top_env_add(Function::create("rest",        c_rest));
   top_env_add(Function::create("apply",       c_apply));
   top_env_add(Function::create("map",         c_map));
   top_env_add(Function::create("nil?",        c_nil_p));
   top_env_add(Function::create("true?",       c_true_p));
   top_env_add(Function::create("false?",      c_false_p));
   top_env_add(Function::create("symbol?",     c_symbol_p));
   top_env_add(Function::create("keyword?",    c_keyword_p));
   top_env_add(Function::create("symbol",      c_symbol));
   top_env_add(Function::create("keyword",     c_keyword));
   top_env_add(Function::create("vector?",     c_vector_p));
   top_env_add(Function::create("vector",      c_vector));
   top_env_add(Function::create("sequential?", c_sequential_p));
   top_env_add(Function::create("assoc",       c_assoc));
   top_env_add(Function::create("dissoc",      c_dissoc));
   top_env_add(Function::create("map?",        c_map_p));
   top_env_add(Function::create("hash-map",    c_hash_map));
   top_env_add(Function::create("get",         c_get));
   top_env_add(Function::create("contains?",   c_contains_p));
   top_env_add(Function::create("keys",        c_keys));
   top_env_add(Function::create("vals",        c_vals));
   top_env_add(Function::create("throw",       c_throw));
   top_env_add(Function::create("readline",    c_readline));
   top_env_add(Function::create("time-ms",     c_time_ms));
   top_env_add(Function::create("conj",        c_conj));
   top_env_add(Function::create("string?",     c_string_p));
   top_env_add(Function::create("number?",     c_number_p));
   top_env_add(Function::create("integer?",    c_int_p));
   top_env_add(Function::create("int?",        c_int_p));
   top_env_add(Function::create("real?",       c_real_p));
   top_env_add(Function::create("int",         c_int));
   top_env_add(Function::create("real",        c_real));
   top_env_add(Function::create("fn?",         c_fn_p));
   top_env_add(Function::create("macro?",      c_macro_p));
   top_env_add(Function::create("seq",         c_seq));
   top_env_add(Function::create("with-meta",   c_with_meta));
   top_env_add(Function::create("meta",        c_meta));

   top_env_add(Function::create("gc",          c_gc));
   top_env_add(Function::create("gc-info",     c_gc_info));

   //-------------------------------------------------------------------

   const char* expr = R"""(
(def! *host-language* "cxx")

(def! not (fn* [x]
  (if x false true)))

(defmacro! cond (fn* (& xs)
  (if (> (count xs) 0)
     (list 'if
           (first xs)
           (if (> (count xs) 1)
              (nth xs 1)
              (throw "odd number of forms to cond"))
           (cons 'cond (rest (rest xs)))))))
)""";

   std::istringstream stm(expr);

   eval_stream(stm, gTopEnv);

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

   rep("(try* (load-file \".malrc\"))", gTopEnv);
}

}

int main(int argc, char * argv[])
{
   MAL::GC::initialize(128 * 1024*1024, true);

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
