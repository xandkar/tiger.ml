module type S = sig
  type t

  type access

  val word_size : int

  val make :
    name:Tiger_temp.Label.t -> formals:bool list -> t

  val name :
    t -> Tiger_temp.Label.t

  val formals :
    t -> access list

  val alloc_local :
    t -> escapes:bool -> access
end
