#include <iostream>
#include <cstdlib>
#include <memory>
#include <chrono>

#include "types.h"
#include "env.h"
#include "gc.h"

using ms = std::chrono::milliseconds;

namespace MAL
{

GC::GC(int size, bool enable)
{
   if (size < 16384)
      size = 16384;

   _count = 0;
   _time_ms = 0;

   _size = (size + 7) & (~7);

   _poolA = (byte *)::malloc(_size);
   _freeA = _poolA;

   _poolB = nullptr;
   _freeB = nullptr;

   _state = enable ? State::Normal : State::Disabled;
}

GC::~GC()
{
   if (_poolA != nullptr)
      free(_poolA);

   if (_poolB != nullptr)
      free(_poolB);
}

#ifdef _DEBUG

void GC::dump()
{
   std::cout << "\n;;;------------------------------\n";

   std::cout << ";;; poolA: " << (void*)_poolA
             << " - " << (void*)(_poolA + _size) << std::endl;
   std::cout << ";;; freeA: " << (void*)_freeA << std::endl;

   std::cout << ";;; poolB: " << (void*)_poolB
             << " - " << (void*)(_poolB + _size) << std::endl;
   std::cout << ";;;\n";

   for (byte* ptr = _poolA; ptr < _freeA; )
   {
      SZ size = *(SZ*)ptr;

      Value* val = (Value*)(ptr + sizeof(SZ));

      std::cout << ";;; ";

      switch (val->get_type())
      {
         case Type::Reference:   std::cout << "Ref";   break;
         case Type::Symbol:      std::cout << "Sym";   break;
         case Type::Keyword:     std::cout << "Key";   break;
         case Type::Constant:    std::cout << "Con";   break;
         case Type::Int:         std::cout << "Int";   break;
         case Type::Real:        std::cout << "Rea";   break;
         case Type::String:      std::cout << "Str";   break;
         case Type::List:        std::cout << "Lst";   break;
         case Type::ListNode:    std::cout << "Pai";   break;
         case Type::Vector:      std::cout << "Vec";   break;
         case Type::Atom:        std::cout << "Atm";   break;
         case Type::AVLTree:     std::cout << "AVL";   break;
         case Type::TreeNode:    std::cout << "Tre";   break;
         case Type::Function:    std::cout << "Fun";   break;
         case Type::Closure:     std::cout << "Clo";   break;

         default:
            std::cout << (int)val->get_type();
            break;
      }

      std::cout << " " << (void*)val << ": ";

      val->pr_str(std::cout, true);
      std::cout << std::endl;

      ptr += sizeof(SZ) + size;
   }
   std::cout << "gc-count: " << _count << std::endl;
   std::cout << ";;;------------------------------\n\n";
}

#endif

void * GC::alloc_memory(int size)
{
   if (size <= 0)
      return nullptr;

   size = (size + 7) & (~7);

   byte * ptr = _freeA + sizeof(SZ);

   if (_size - (ptr - _poolA) > size)
   {
      *(SZ *)_freeA = (SZ)size;
      _freeA = ptr + size;
   }
   else switch(_state)
   {
      case State::Normal:
         run();

         ptr = _freeA + sizeof(SZ);

         if (_size - (ptr - _poolA) > size)
         {
            *(SZ *)_freeA = (SZ)size;
            _freeA = ptr + size;
         }
         else
         {
            _state = State::Disabled;
            throw "GC - not enough memory";
         }
         break;

      case State::Disabled:
         throw "GC is disabled";

      case State::Running:
         _state = State::Disabled;
         throw "GC is running - not enough memory";

      default:
         _state = State::Disabled;
         throw "GC - unknown state";
   }

   return (void *)ptr;
}

void * GC::malloc(int size)
{
   if (_add_more > 0)
   {
      void * ptr = gc->alloc_memory(size + _add_more);
      _add_more = 0;

      return ptr;
   }

   return gc->alloc_memory(size);
}

void GC::swap_pool()
{
   std::swap(_poolA, _poolB);
   std::swap(_freeA, _freeB);

   if (_poolA == nullptr)
      _poolA = (byte *)::malloc(_size);

   _freeA = _poolA;
}

void GC::run()
{
   if (_state == State::Running || _state == State::Disabled)
      return;

   auto tm_start = std::chrono::high_resolution_clock::now();

   _state = State::Running;

   swap_pool();

   EnvStack::clear_visited();

   EnvStack::move_objects();
   GCStack::move_objects();

   byte * ptr = move_objects();
   move_meta_data();
   move_objects(ptr);

   EnvStack::clear_visited();

   _state = State::Normal;

   _count++;

   auto tm_stop = std::chrono::high_resolution_clock::now();

   auto tm = std::chrono::duration_cast<ms>(tm_stop - tm_start);

   _time_ms += tm.count();
}

bool GC::in_poolA(void * ptr)
{
   return _poolA <= ptr && ptr < (_poolA + _size);
}

bool GC::in_poolB(void * ptr)
{
   return _poolB <= ptr && ptr < (_poolB + _size);
}

byte * GC::move_objects(byte * ptr)
{
   if (ptr == nullptr)
      ptr = _poolA;

   while (ptr < _freeA)
   {
      SZ size = *(SZ *)ptr;

      Value * val = (Value *)(ptr + sizeof(SZ));

      val->gc_move();

      ptr += sizeof(SZ) + size;
   }

   return ptr;
}

std::unique_ptr<GC> GC::gc;
int GC::_add_more = 0;

}
