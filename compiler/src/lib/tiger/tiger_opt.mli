type 'a t = 'a option

val map : 'a t -> ('a -> 'b) -> 'b t

val iter : 'a t -> f:('a -> unit) -> unit

val get : 'a t -> default:'a -> 'a
