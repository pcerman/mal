#pragma once

#include <memory>


using byte = unsigned char;

namespace MAL
{

class GC
{
   using SZ = uint32_t;

   enum class State
   {
      Normal,
      Disabled,
      Running,
   };

public:
   GC(int size, bool enable);
   ~GC();

#ifdef _DEBUG
   void dump();
#endif

private:
   void * alloc_memory(int size);

   void swap_pool();
   void run();

   bool in_poolA(void * ptr);
   bool in_poolB(void * ptr);

   byte * move_objects(byte * ptr = nullptr);

private:
   int _count;
   int64_t _time_ms;

   int _size;
   State _state;

   byte * _poolA;
   byte * _freeA;

   byte * _poolB;
   byte * _freeB;

public:
   static void initialize(int size, bool enable = false)
   {
      gc = std::make_unique<GC>(size, enable);
   }

   static void * malloc(int size);
   static void add_extra_size(int extra) { _add_more = extra; }

   static void do_swap_pool()           { gc->swap_pool(); }
   static bool is_in_pool_A(void * ptr) { return gc->in_poolA(ptr); }
   static bool is_in_pool_B(void * ptr) { return gc->in_poolB(ptr); }

   static void gc_run()       { gc->run(); }
   static int gc_count()      { return gc->_count; }
   static int64_t gc_time()   { return gc->_time_ms; }

private:
   static std::unique_ptr<GC> gc;
   static int _add_more;
};

}
