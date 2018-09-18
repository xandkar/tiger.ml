type 'a t = 'a option

val map : 'a t -> ('a -> 'b) -> 'b t

val get : 'a t -> default:'a -> 'a
