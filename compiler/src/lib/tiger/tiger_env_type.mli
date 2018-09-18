module Sym = Tiger_symbol

type unique

type t =
  | Unit
  | Nil
  | Int
  | String
  | Record of
      { unique : unique
      ; fields : record_fields
      }
  | Array of
      { unique : unique
      ; ty     : t
      }
  | Name of Sym.t * t option ref
and record_fields =
  (Sym.t * t) list

type env =
  (Sym.t, t ) Tiger_map.t

val built_in : env

val is_equal  : t -> t -> bool

val is_int    : t -> bool
val is_string : t -> bool
val is_array  : t -> bool
val is_record : t -> bool
val is_name   : t -> bool

val if_record : t -> f:(record_fields -> 'a) -> otherwise:(unit -> 'a) -> 'a
val if_array  : t -> f:(t -> 'a)             -> otherwise:(unit -> 'a) -> 'a

val new_record : name:Sym.t -> fields:record_fields -> t
val new_array  : name:Sym.t -> ty:t -> t

val to_string : t -> string
