type t

val of_list : ('a * 'a) list -> (t, [`Cycle]) result
