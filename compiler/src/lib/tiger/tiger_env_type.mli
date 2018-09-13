type unique

type t =
  | Unit
  | Nil
  | Int
  | String
  | Record of
      { unique : unique
      ; fields : (Tiger_symbol.t * t) list
      }
  | Array of
      { unique : unique
      ; ty     : t
      }
  | Name of Tiger_symbol.t * t option ref

type env =
  (Tiger_symbol.t, t ) Tiger_map.t

val built_in : env

val is_equal  : t -> t -> bool
val is_record : t -> bool
val is_int    : t -> bool
val is_name   : t -> bool

val new_record : (Tiger_symbol.t * t) list -> t
val new_array  : t -> t

val to_string : t -> string
