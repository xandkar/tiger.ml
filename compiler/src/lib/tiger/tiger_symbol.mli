type t

val new_of_string : string -> t

val of_string : string -> t

val to_string : t -> string
(* Reversable. Returns original. *)

val show : t -> string
(* Not-reversable. M-expression with name and symbol. *)

val is_equal : t -> t -> bool
