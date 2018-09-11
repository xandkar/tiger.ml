module type S = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t

  val set : ('k, 'v) t -> k:'k -> v:'v -> ('k, 'v) t

  val get : ('k, 'v) t -> k:'k -> 'v option

  val member : ('k, 'v) t -> k:'k -> bool

  val to_dot : ('k, 'v) t -> k_to_string:('k -> string) -> string

  val of_list : ('k * 'v) list -> ('k, 'v) t
end
