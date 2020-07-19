#pragma once

#include <string.h>
#include <vector>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <functional>
#include <type_traits>

#include "env.h"

namespace MAL
{

extern char buffer[1040];
extern const int buffer_size;

//----------------------------------------------------------------------

class Value;

Value * EVAL(Value * arg, EnvPtr env);

//----------------------------------------------------------------------

class MalException : public std::exception
{
public:
   MalException(const char * msg)
      : _msg { msg }, _val { nullptr }
   {}

   MalException(Value * val)
      : _msg { "" }, _val { val }
   {}

   const std::string & get_msg();

   Value * get_value();

private:
   std::string _msg;
   Value * _val;
};

class reader;

class Symbol;

void error(int idx, const char * fun, const char * msg ...);
void error(int idx, Value * sym, const char * msg ...);
void error(reader & rdr, const char * msg ...);
void error_expected(int idx, const char * type, const char * fun);
void error_expected(int idx, const char * type, Value * sym);

//----------------------------------------------------------------------

enum class Type {
   None,
   Reference,
   Symbol,
   Keyword,
   Constant,
   Int,
   Real,
   String,
   List,
   ListNode,
   Vector,
   Atom,
   AVLTree,
   TreeNode,
   Function,
   Closure
};

//----------------------------------------------------------------------

class Value
{
public:
   virtual ~Value() {}
   virtual Type get_type() const = 0;
   virtual Value * gc_clone() { return this; }
   virtual void gc_move() {}
   virtual void pr_str(std::ostream & os, bool readably) = 0;

   virtual bool equals_to(Value * val) const = 0;

   static bool equals(Value * val_a, Value * val_b);

   static bool is_number(Value * arg);
   static bool is_sequence(Value * arg);
   static bool is_type(Value * arg, Type type);

   static int compare(Value * val_a, Value * val_b);

   static void move_object_(Value ** p_val);

   template<class T>
   static std::enable_if_t<std::is_base_of_v<Value, T>, void>
   move_object(T * & val) { move_object_((Value **)&val); }
};

//----------------------------------------------------------------------

class GCValue : public Value
{
public:
   virtual Value * gc_clone() = 0;

   void * operator new (std::size_t size);
   void operator delete(void *);
};

//----------------------------------------------------------------------

class Reference : public Value
{
public:
   Reference(Value * ptr = nullptr) : _ptr { ptr } {}

   void set(Value * ptr)   { _ptr = ptr; }
   Value * get()           { return _ptr; }

   virtual Type get_type() const { return Type::Reference; }
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

private:
   Value * _ptr;
};

//----------------------------------------------------------------------

class Symbol : public Value
{
    using Table = std::unordered_map <std::string, std::unique_ptr<Symbol>>;

private:
   Symbol(const char * str) : _name { str } {}

public:
   virtual ~Symbol() {}
   virtual Type get_type() const { return Type::Symbol; }
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   int compare(const Symbol & val) const;

   const std::string & get_name() const { return _name; }

private:
   std::string _name;

protected:
   static Table _symbols;

public:
   static Symbol * create(const char * name);
   static bool is_symbol(const char * name, Symbol ** sym = nullptr);

   static Symbol * const Defmacro;
   static Symbol * const Macroexpand;

   static Symbol * const Def;
   static Symbol * const Let;
   static Symbol * const If;
   static Symbol * const Fn;
   static Symbol * const Do;
   static Symbol * const Amp;

   static Symbol * const quote;
   static Symbol * const quasiquote;
   static Symbol * const unquote;
   static Symbol * const splice_unquote;
   static Symbol * const deref;
   static Symbol * const with_meta;

   static Symbol * const Try;
   static Symbol * const Catch;

   static Symbol * const cons;
   static Symbol * const concat;
};

//----------------------------------------------------------------------

class Keyword : public Value
{
    using Table = std::unordered_map <std::string, std::unique_ptr<Keyword>>;

private:
   Keyword(const char * str) : _name { str } {}

public:
   virtual ~Keyword() {}
   virtual Type get_type() const { return Type::Keyword; }
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   int compare(const Keyword & val) const;

   const std::string & get_name() const { return _name; }

private:
   std::string _name;

protected:
   static Table _keywords;

public:
   static Keyword * create(const char * name);
   static bool is_keyword(const char * name, Keyword ** kwd = nullptr);
};

//----------------------------------------------------------------------

class Constant : public Value
{
private:
   Constant(const char * name) : _name { name } {}

public:
   virtual ~Constant() {}
   virtual Type get_type() const { return Type::Constant; }
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

private:
   std::string _name;

public:
   static Constant * const Nil;
   static Constant * const True;
   static Constant * const False;
};

//----------------------------------------------------------------------

class Number : public GCValue
{
public:
   virtual int64_t get_int() const = 0;
   virtual double get_real() const = 0;

   virtual Number * add(Number * num) const = 0;
   virtual Number * sub(Number * num) const = 0;
   virtual Number * mul(Number * num) const = 0;
   virtual Number * div(Number * num) const = 0;

   virtual Value * lt(Number * num) const = 0;
   virtual Value * le(Number * num) const = 0;
   virtual Value * gt(Number * num) const = 0;
   virtual Value * ge(Number * num) const = 0;
};

//----------------------------------------------------------------------

class Int : public Number
{
private:
   Int(int64_t num) : _num { num } {}

public:
   virtual Type get_type() const { return Type::Int; }
   virtual Value * gc_clone() { return new Int(_num); }
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   int64_t value() const { return _num; }

   int compare(const Int & val) const;

   virtual int64_t get_int() const { return _num; }
   virtual double get_real() const { return (double)_num; }

   virtual Number * add(Number * num) const;
   virtual Number * sub(Number * num) const;
   virtual Number * mul(Number * num) const;
   virtual Number * div(Number * num) const;

   virtual Value * lt(Number * num) const;
   virtual Value * le(Number * num) const;
   virtual Value * gt(Number * num) const;
   virtual Value * ge(Number * num) const;

private:
   int64_t _num;

public:
  static Int * create(int64_t num);
  static Int * create(Int * num) { return create(num->_num); }

  static Int * zero()  { return Int::create((int64_t) 0); }
  static Int * one()   { return Int::create((int64_t) 1); }
  static Int * n_one() { return Int::create((int64_t)-1); }
};

//----------------------------------------------------------------------

class Real : public Number
{
private:
   Real(double num) : _num { num } {}

public:
   virtual Type get_type() const { return Type::Real; }
   virtual Value * gc_clone() { return new Real(_num); }
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   double value() const { return _num; }

   int compare(const Real & val) const;

   virtual int64_t get_int() const { return (int64_t)_num; }
   virtual double get_real() const { return _num; }

   virtual Number * add(Number * num) const;
   virtual Number * sub(Number * num) const;
   virtual Number * mul(Number * num) const;
   virtual Number * div(Number * num) const;

   virtual Value * lt(Number * num) const;
   virtual Value * le(Number * num) const;
   virtual Value * gt(Number * num) const;
   virtual Value * ge(Number * num) const;

private:
   double _num;

public:
  static Real * create(double num);
  static Real * create(Real * num) { return create(num->_num); }

  static Real * zero()  { return Real::create( 0.0); }
  static Real * one()   { return Real::create( 1.0); }
  static Real * n_one() { return Real::create(-1.0); }
};

//----------------------------------------------------------------------

class String : public GCValue
{
private:
   String(const char * str, int add_extra_size);
   String(const std::string & str, int add_extra_size);

public:
   virtual Type get_type() const { return Type::String; }
   virtual Value * gc_clone();
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   int compare(const String & val) const;

   const char * str() const { return _str; }

private:
   int _add_extra_size;
   char _str[8];

public:
   static String * create(const char * str);
   static String * create(const std::string & str);
   static String * create(String * str) { return create(str->_str); }
};

//----------------------------------------------------------------------

class Sequence : public GCValue
{
public:
   virtual bool equals_to(Value * val) const;
   virtual int length() const = 0;
   virtual Value * nth(int idx) const = 0;
};

//----------------------------------------------------------------------

class ListNode : public GCValue
{
   ListNode() : _car { nullptr }, _cdr { nullptr } {}

   ListNode(Value * car, ListNode * cdr)
      : _car { car }, _cdr { cdr } {}

   Value    * _car;
   ListNode * _cdr;

   virtual Type get_type() const { return Type::ListNode; }
   virtual Value * gc_clone() { return new ListNode(_car, _cdr); };
   virtual void gc_move();
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   friend class List;

public:
   void set_car(Value * car)     { _car = car; }
   void set_cdr(ListNode * cdr)  { _cdr = cdr; }

   Value    * get_car() const    { return _car; }
   ListNode * get_cdr() const    { return _cdr; }

   Value * nth(int idx) const;
   ListNode * last_node();
   Value * last()                { return last_node()->_car; }

   int count() const;
   bool min_count_p(int min_cnt) const;

   static ListNode * create(Value * car, ListNode * cdr = nullptr);
};

//----------------------------------------------------------------------

class List : public Sequence
{
private:
   List(ListNode * elements = nullptr)
      : _elements { elements } {}

public:
   virtual Type get_type() const { return Type::List; }
   virtual Value * gc_clone() { return new List(_elements); }
   virtual void gc_move();
   virtual void pr_str(std::ostream & os, bool readably);

   ListNode * elements() const { return _elements; }
   void set_elements(ListNode * elements) { _elements = elements; }

   void cons(Value * val);

   List * map(std::function<Value * (Value *)> fn);
   void for_each(std::function<void (Value *)> fn);

   void reverse_d();

   void rev_prepend_d(ListNode * node);
   void rev_prepend_d(List * lst) { rev_prepend_d(lst->_elements); }

   bool is_null() const  { return _elements == nullptr; }
   Value * first() const { return _elements == nullptr
                                ? nullptr : _elements->_car; }
   List * rest() const   { return _elements == nullptr
                                ? (List *)this
                                : List::create(_elements->_cdr); }

   ListNode * last_node() const  { return _elements == nullptr
                                        ? nullptr
                                        : _elements->last_node(); }
   Value * last() const          { return _elements == nullptr
                                        ? nullptr
                                        : _elements->last(); }

   virtual int length() const { return _elements == nullptr
                                     ? 0 : _elements->count(); }

   virtual Value * nth(int idx) const;

private:
   ListNode * _elements;

public:
   static List * create(ListNode * elms = nullptr);
   static List * create(List * lst) { return create(lst->_elements); }
};

//----------------------------------------------------------------------

class Vector : public Sequence
{
private:
   Vector(int count, bool clear = true);

public:
   virtual Type get_type() const { return Type::Vector; }
   virtual Value * gc_clone();
   virtual void gc_move();
   virtual void pr_str(std::ostream & os, bool readably);

   Vector * map(std::function<Value * (Value *)> fn);
   List * map_to_list(std::function<Value * (Value *)> fn);
   ListNode * to_node_list();

   List * drop(int n);

   Value * & operator[](int idx) { return _elements[idx]; }

   virtual int length() const { return _count; }
   virtual Value * nth(int idx) const;

private:
   int _count;
   Value * _elements[1];

public:
   static Vector * create(int count, bool clear = true);
   static Vector * create(std::vector<Value *> & elms);
   static Vector * create(Vector * vec);
};

//----------------------------------------------------------------------

class Atom : public GCValue
{
   Atom(Value * val = nullptr) : _value { val } {}

public:
   virtual Type get_type() const { return Type::Atom; }
   virtual Value * gc_clone() { return new Atom(_value); }
   virtual void gc_move();
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   void set(Value * val) { _value = val; }
   Value * get()         { return _value; }

private:
   Value * _value;

public:
   static Atom * create(Value * val);
};

//----------------------------------------------------------------------

class TreeNode : public GCValue
{
   TreeNode()
      : _key { nullptr }, _data { nullptr },
        _ltn { nullptr }, _rtn { nullptr },
        _height { 0 } {}

   TreeNode(Value * key, Value * data, TreeNode * ltn, TreeNode * rtn);

   void set(Value * key, Value * data, TreeNode * ltn, TreeNode * rtn);

   int _height;
   Value * _key;
   Value * _data;
   TreeNode * _ltn;
   TreeNode * _rtn;

   static int balance(TreeNode * node);

   static TreeNode * rotateL(TreeNode * node);
   static TreeNode * rotateR(TreeNode * node);
   static TreeNode * rebalance(TreeNode * node);

   static TreeNode * insert(TreeNode * node, Value * key, Value * data);
   static TreeNode * remove(TreeNode * node, Value * key);
   static TreeNode * lookup(TreeNode * node, Value * key);

   static std::pair<TreeNode *, TreeNode *> pop_min(TreeNode * node);
   static std::pair<TreeNode *, TreeNode *> pop_max(TreeNode * node);

   virtual Type get_type() const { return Type::TreeNode; }
   virtual void gc_move();
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   TreeNode * map(std::function<Value * (Value *)> fn);
   void for_each(std::function<void (Value *, Value *)> fn);
   int count() const;

   friend class AVLTree;

public:
   virtual Value * gc_clone();

   int        get_height() const  { return _height; }
   Value    * get_key()    const  { return _key;    }
   Value    * get_data()   const  { return _data;   }
   TreeNode * get_left()   const  { return _ltn;    }
   TreeNode * get_right()  const  { return _rtn;    }

   static TreeNode * create(Value * key, Value * data,
                            TreeNode * ltn, TreeNode * rtn);
};

class AVLTree : public GCValue
{
private:
   AVLTree(TreeNode * root) : _root { root } {}

public:
   virtual Type get_type() const { return Type::AVLTree; }
   virtual Value * gc_clone() { return new AVLTree(_root); }
   virtual void gc_move();
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   void insert(Value * key, Value * data)
   {
      _root = TreeNode::insert(_root, key, data);
   }

   void remove(Value * key)
   {
      _root = TreeNode::remove(_root, key);
   }

   TreeNode * lookup(Value * key)
   {
      return TreeNode::lookup(_root, key);
   }

   bool empty_p() const { return _root == nullptr; }
   int count() const { return _root == nullptr
                            ? 0 : _root->count(); }

   TreeNode * get_root() const { return _root; }

   AVLTree * map(std::function<Value * (Value *)> fn);
   void for_each(std::function<void (Value *, Value *)> fn)
   {
      if (_root != nullptr)
         _root->for_each(fn);
   }

private:
   TreeNode * _root;

public:
   static AVLTree * create(TreeNode * root) { return new AVLTree(root); }
};

//----------------------------------------------------------------------

using Fun  = std::function<Value * (ListNode *, const char *)>;

class Function : public GCValue
{
protected:
   Function(const char * str, Fun fn, bool macro)
      : _name { str }
      , _fn { fn }
      , _is_macro { macro } {}
   Function(const Function & f)
      : _name { f._name }
      , _fn { f._fn }
      , _is_macro { f._is_macro } {}

public:
   virtual Type get_type() const { return Type::Function; }
   virtual Value * gc_clone() { return new Function(*this); }
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

   Value * eval(ListNode * args)
   { return _fn(args, _name); }

   const char * get_name() const     { return _name; }

   bool is_macro() const             { return _is_macro; }
   void set_macro(bool macro = true) { _is_macro = macro; }

private:
   const char * _name;
   bool _is_macro;
   Fun _fn;

public:
   static Function * create(const char * str,
                            Fun fn,
                            bool macro = false)
   { return new Function(str, fn, macro); }

   static Function * create(Function * fun)
   { return new Function(*fun); }
};

void check_args_N(ListNode * args, int cnt, const char * name);
void check_args_V(ListNode * args, int cnt, const char * name);

inline void check_args_0(ListNode * args, const char * name)
{ check_args_N(args, 0, name); }

inline void check_args_1(ListNode * args, const char * name)
{ check_args_N(args, 1, name); }

inline void check_args_2(ListNode * args, const char * name)
{ check_args_N(args, 2, name); }

//----------------------------------------------------------------------

class Closure : public GCValue
{
private:
   Closure(EnvPtr & env)
      : _is_macro{ false }, _env { env }
      , _pars { nullptr }, _body { nullptr }
   {}

   Closure(EnvPtr & env, Sequence * pars, Value * body, bool macro)
      : _is_macro{ macro }, _env { env }
      , _pars { pars }, _body { body }
   {}

   void set(Sequence * pars, Value * body)
   {
      _pars = pars;
      _body = body;
   }

   virtual Type get_type() const { return Type::Closure; }
   virtual Value * gc_clone();
   virtual void gc_move();
   virtual void pr_str(std::ostream & os, bool readably);

   virtual bool equals_to(Value * val) const;

public:
   EnvPtr create_env(ListNode * args);
   Value * get_body() { return _body; }
   Value * eval(ListNode * args);

   bool is_macro() const               { return _is_macro; }
   void set_macro(bool macro = true)   { _is_macro = macro; }

private:
   bool       _is_macro;
   EnvPtr     _env;
   Sequence * _pars;
   Value *    _body;

public:
   static Closure * create(EnvPtr & env, Sequence * pars, Value * body);
   static Closure * create(Closure * cls);
};

//----------------------------------------------------------------------

class GCStack
{
   const int IncCapacity = 8192;
   const int Reserve = 128;

public:
   GCStack()
      : _count { _size }
   {
      if (_size + Reserve > _capacity)
      {
         _capacity += IncCapacity;
         _values = (Value ***)realloc(_values, _capacity * sizeof(Value **));
      }
   }

   ~GCStack()
   {
      _size = _count;
   }

private:
   int _count;

   static Value *** _values;
   static int _capacity;
   static int _size;

public:
   template<class T>
   static std::enable_if_t<std::is_base_of_v<Value, T>, void>
   push(T * & var)
   {
      _values[_size++] = (Value **)&var;
   }

   static void move_objects();
};

//----------------------------------------------------------------------

template<class T>
class VP
{
   static_assert(std::is_base_of<Value, T>::value, "T is not derived from Value");

private:
   T * _ptr;

public:
   VP(T * ptr = nullptr) : _ptr { ptr }
   {
      GCStack::push(_ptr);
   }

   T * get() const            { return _ptr; }

   T * operator =(T * ptr)    { _ptr = ptr; return _ptr; }
   T & operator *() const     { return *_ptr; }

   operator bool() const      { return _ptr != nullptr; }
   operator T*() const        { return _ptr; }

   template<class C>
   std::enable_if_t<std::is_base_of_v<Value, C>, C*> P()
   { return (C *)_ptr; }

   template<class C>
   std::enable_if_t<std::is_base_of_v<Value, C>, C&> R()
   { return *(C *)_ptr; }

   T * operator ->()          { return _ptr; }
};

template<class C>
std::enable_if_t<std::is_base_of_v<Value, C>, C*> P(Value * ptr)
{ return (C *)ptr; }

template<class C>
std::enable_if_t<std::is_base_of_v<Value, C>, C&> R(Value * ptr)
{ return *(C *)ptr; }


using ValueP    = VP<Value>;
using SymbolP   = VP<Symbol>;
using KeywordP  = VP<Keyword>;
using ConstantP = VP<Constant>;
using NumberP   = VP<Number>;
using IntP      = VP<Int>;
using RealP     = VP<Real>;
using StringP   = VP<String>;
using SequenceP = VP<Sequence>;
using ListP     = VP<List>;
using ListNodeP = VP<ListNode>;
using VectorP   = VP<Vector>;
using AtomP     = VP<Atom>;
using AVLTreeP  = VP<AVLTree>;
using TreeNodeP = VP<TreeNode>;
using FunctionP = VP<Function>;
using ClosureP  = VP<Closure>;

//----------------------------------------------------------------------

extern std::map<Value *, Value *> gMetaData;

void move_meta_data();

}
