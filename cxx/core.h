#pragma once

#include <istream>

#include "types.h"
#include "env.h"

namespace MAL
{

Value * eval_stream(std::istream & stream, EnvPtr & env);

Value * c_add(ListNode * args, const char * name);
Value * c_sub(ListNode * args, const char * name);
Value * c_mul(ListNode * args, const char * name);
Value * c_div(ListNode * args, const char * name);
Value * c_lt(ListNode * args, const char * name);
Value * c_le(ListNode * args, const char * name);
Value * c_gt(ListNode * args, const char * name);
Value * c_ge(ListNode * args, const char * name);
Value * c_equal(ListNode * args, const char * name);
Value * c_list(ListNode * args, const char * name);
Value * c_list_p(ListNode * args, const char * name);
Value * c_empty_p(ListNode * args, const char * name);
Value * c_count(ListNode * args, const char * name);
Value * c_str(ListNode * args, const char * name);
Value * c_pr_str(ListNode * args, const char * name);
Value * c_prn(ListNode * args, const char * name);
Value * c_println(ListNode * args, const char * name);
Value * c_read_string(ListNode * args, const char * name);
Value * c_slurp(ListNode * args, const char * name);
Value * c_load_file(ListNode * args, const char * name);
Value * c_eval(ListNode * args, const char * name);
Value * c_atom(ListNode * args, const char * name);
Value * c_atom_p(ListNode * args, const char * name);
Value * c_deref(ListNode * args, const char * name);
Value * c_reset(ListNode * args, const char * name);
Value * c_swap(ListNode * args, const char * name);
Value * c_cons(ListNode * args, const char * name);
Value * c_concat(ListNode * args, const char * name);
Value * c_nth(ListNode * args, const char * name);
Value * c_first(ListNode * args, const char * name);
Value * c_rest(ListNode * args, const char * name);
Value * c_apply(ListNode * args, const char * name);
Value * c_map(ListNode * args, const char * name);
Value * c_nil_p(ListNode * args, const char * name);
Value * c_true_p(ListNode * args, const char * name);
Value * c_false_p(ListNode * args, const char * name);
Value * c_symbol_p(ListNode * args, const char * name);
Value * c_keyword_p(ListNode * args, const char * name);
Value * c_symbol(ListNode * args, const char * name);
Value * c_keyword(ListNode * args, const char * name);
Value * c_vector_p(ListNode * args, const char * name);
Value * c_vector(ListNode * args, const char * name);
Value * c_sequential_p(ListNode * args, const char * name);
Value * c_assoc(ListNode * args, const char * name);
Value * c_dissoc(ListNode * args, const char * name);
Value * c_map_p(ListNode * args, const char * name);
Value * c_hash_map(ListNode * args, const char * name);
Value * c_get(ListNode * args, const char * name);
Value * c_contains_p(ListNode * args, const char * name);
Value * c_keys(ListNode * args, const char * name);
Value * c_vals(ListNode * args, const char * name);
Value * c_throw(ListNode * args, const char * name);
Value * c_readline(ListNode * args, const char * name);
Value * c_time_ms(ListNode * args, const char * name);
Value * c_conj(ListNode * args, const char * name);
Value * c_string_p(ListNode * args, const char * name);
Value * c_number_p(ListNode * args, const char * name);
Value * c_int_p(ListNode * args, const char * name);
Value * c_real_p(ListNode * args, const char * name);
Value * c_int(ListNode * args, const char * name);
Value * c_real(ListNode * args, const char * name);
Value * c_fn_p(ListNode * args, const char * name);
Value * c_macro_p(ListNode * args, const char * name);
Value * c_seq(ListNode * args, const char * name);
Value * c_with_meta(ListNode * args, const char * name);
Value * c_meta(ListNode * args, const char * name);
Value * c_gc(ListNode * args, const char * name);
Value * c_gc_info(ListNode * args, const char * name);

}
