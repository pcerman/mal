(***** types.sml *****)

structure SDic = Table (type key' = string
                        val compare = String.compare)

structure Mal = struct

datatype Value = Nil
               | String of string
               | Symbol of string
               | Keyword of string
               | Int of int
               | Real of real
               | Bool of bool
               | Atom of (Value ref * Value)
               | List of (Value list * Value)
               | Vector of (Value vector * Value)
               | Hashmap of ((Value * Value) SDic.Table * Value)
               | Function of (string * (Value list -> Value) * Value)
               | Closure of (string list * string option * Value list * EnvType ref * Value)
               | Macro of (string list * string option * Value list * EnvType ref * Value)

and EnvType = None
            | Env of {data: Value SDic.Table, outer: EnvType ref}

(* Errors/Exceptions *)

exception Throw of Value
exception Exception of string

fun error msg = raise Exception msg

(* Symbols/Keywords *)

val Symbols : Value SDic.Table ref = ref SDic.empty

fun makeKeyword name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val sym = Keyword name
                    in Symbols := SDic.insert syms (name, sym);
                       sym
                    end
          | SOME sym => sym
    end

fun makeSymbol name =
    let val syms = !Symbols
    in
        case SDic.lookup syms name of
            NONE => let val sym = Symbol name
                    in Symbols := SDic.insert syms (name, sym);
                       sym
                    end
          | SOME sym => sym
    end

val Quote = makeSymbol "quote"
val Quasiquote = makeSymbol "quasiquote"
val Unquote = makeSymbol "unquote"
val Splice_unquote = makeSymbol "splice-unquote"
val Deref = makeSymbol "deref"
val With_meta = makeSymbol "with-meta"

val Concat = makeSymbol "concat"
val Cons = makeSymbol "cons"

end
