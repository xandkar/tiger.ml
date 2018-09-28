module Level : sig
  type t

  val init : t
  (** "outermost" in Applel's code *)

  val next : t -> name:Tiger_temp.Label.t -> formals:bool list -> t
  (** "newLevel" in Appel's code *)
end

type gen_stm =
  (Tiger_temp.Label.t * Tiger_temp.Label.t) -> Tiger_tree.stm

type exp

type access

val alloc_local : level:Level.t -> escapes:bool -> access

val formals : level:Level.t -> access list

val unEx : exp -> Tiger_tree.exp
val unNx : exp -> Tiger_tree.stm
val unCx : exp -> gen_stm

val dummy__FIXME : exp  (* FIXME: Remove dummy when real is ready *)
