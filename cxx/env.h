#pragma once

#include <string>
#include <vector>
#include <map>
#include <set>
#include <memory>

namespace MAL
{

class Value;
class Symbol;

class RefObj
{
public:
   RefObj() : _count {} {}

   int inc_ref()
   {
      return ++_count;
   }

   int dec_ref()
   {
      return --_count;
   }

private:
   int _count;
};

template <class T> class RefObjPtr
{
public:
   RefObjPtr() : _obj { }
   {}

   RefObjPtr(T * obj) : _obj { }
   {
      acquire(obj);
   }

   RefObjPtr(const RefObjPtr & ptr) : _obj { }
   {
      acquire(ptr._obj);
   }

   RefObjPtr(RefObjPtr && ptr)
      : _obj { ptr._obj }
   {
      ptr._obj = nullptr;
   }

   ~RefObjPtr()
   {
      release();
   }

   T * get() { return _obj; }

   const RefObjPtr & operator = (const RefObjPtr & ptr)
   {
      acquire(ptr._obj);

      return *this;
   }

   const RefObjPtr & operator = (RefObjPtr && ptr)
   {
      release();
      _obj = ptr._obj;

      ptr._obj = nullptr;

      return *this;
   }

   operator bool () const           { return _obj != nullptr; }
   T * operator -> ()               { return _obj; }
   const T * operator -> () const   { return _obj; }

private:
   void acquire(T * obj)
   {
      if (obj != nullptr)
         obj->inc_ref();
      release();
      _obj = obj;
   }

   void release()
   {
      if (_obj != nullptr && _obj->dec_ref() <= 0)
         delete _obj;
   }

private:
   T * _obj;
};

class Env : public RefObj
{
public:
   Env() {}

   Env(const RefObjPtr<Env> & outer)
      : _outer { outer }
   {
   }

   void set(const char * key, Value * val)
   {
      _map[key] = val;
   }

   void set(Symbol * sym, Value * val);

   Value * lookup(Symbol * sym) const;
   Value * find(Symbol * sym) const;
   Value * get(Symbol * sym) const;

private:
   void move_objects(std::set<Env *> & visited);

private:
   std::map<std::string, Value *> _map;
   RefObjPtr<Env> _outer;

   friend class EnvStack;
};

using EnvPtr = RefObjPtr<Env>;

extern EnvPtr gTopEnv;

//----------------------------------------------------------------------

class EnvStack
{
   const int IncCapacity = 256;
   const int Reserve = 128;

public:
   EnvStack()
   {
      _count = (int)gStack.size();
   }

   EnvStack(const EnvPtr & env)
   {
      _count = (int)gStack.size();
      gStack.emplace_back(env);
   }

   ~EnvStack()
   {
      gStack.erase(gStack.begin() + _count, gStack.end());
   }

   void push(const EnvPtr & env)
   {
      gStack.emplace_back(env);
   }

private:
   int _count;

   static std::vector<EnvPtr> gStack;
   static std::set<Env *> gVisited;

public:
   static void clear_visited()   { gVisited.clear(); }
   static void move_objects();
   static void move_objects(EnvPtr & env);

   friend class GC;
};

}
