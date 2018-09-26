module Level : sig
  type t

  val init : t
  (** "outermost" in Applel's code *)

  val next : t -> name:Tiger_temp.Label.t -> formals:bool list -> t
  (** "newLevel" in Appel's code *)
end

type exp = unit

type access

val alloc_local : level:Level.t -> escapes:bool -> access

val formals : level:Level.t -> access list
