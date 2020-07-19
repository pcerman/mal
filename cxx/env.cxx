#include <memory>
#include <algorithm>

#include "env.h"
#include "types.h"
#include "gc.h"

namespace MAL
{

void Env::set(Symbol * sym, Value * val)
{
   _map[sym->get_name()] = val;
}

Value * Env::lookup(Symbol * sym) const
{
   auto it = _map.find(sym->get_name());
   if (it != _map.end())
      return it->second;

   if (_outer)
      return _outer->lookup(sym);

   return nullptr;
}

Value * Env::find(Symbol * sym) const
{
   Value * val = lookup(sym);
   return val != nullptr ? val : Constant::Nil;
}

Value * Env::get(Symbol * sym) const
{
   Value * val = lookup(sym);
   if (val == nullptr)
      error(0, "", "'%s' not found", sym->get_name().c_str());

   return val;
}

void Env::move_objects(std::set<Env *> & visited)
{
   if (visited.find(this) == visited.end())
   {
      visited.insert(this);

      std::for_each(_map.begin(), _map.end(),
                     [&](decltype(_map)::value_type & p)
                     {
                        Value::move_object(p.second);
                     });

      if (_outer)
         _outer->move_objects(visited);
   }
}

EnvPtr gTopEnv(new Env());

//----------------------------------------------------------------------

std::vector<EnvPtr> EnvStack::gStack;
std::set<Env *> EnvStack::gVisited;

void EnvStack::move_objects()
{
   for (int i = (int)gStack.size() - 1; i >=0; i--)
   {
      if (gStack[i])
         gStack[i]->move_objects(gVisited);
   }
}

void EnvStack::move_objects(EnvPtr & env)
{
   if (env)
   {
      env->move_objects(gVisited);
   }
}

}
