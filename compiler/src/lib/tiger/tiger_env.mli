type t

val base : t

val get_typ : t -> Tiger_symbol.t -> Tiger_env_type.t option
val get_val : t -> Tiger_symbol.t -> Tiger_env_value.t option

val set_typ : t -> Tiger_symbol.t -> Tiger_env_type.t -> t
val set_val : t -> Tiger_symbol.t -> Tiger_env_value.t -> t

val loop_begin   : t -> (Tiger_symbol.t * t)
val loop_end     : t ->  Tiger_symbol.t -> t
val loop_current : t ->  Tiger_symbol.t option
