open Printf

module Sym = Tiger_symbol

module Counter : sig
  type t

  val create : unit -> t

  val next : t -> int
end = struct
  type t = int ref

  let create () =
    ref 0

  let next t =
    incr t;
    !t
end

module Temp = struct
  type t = int

  let t = Counter.create ()

  let gen () =
    Counter.next t

  let to_string t =
    sprintf "t%d" t
end

module Label = struct
  type t = Sym.t

  let counter = Counter.create ()

  let of_string =
    Sym.of_string

  let gen () =
    of_string (sprintf "L%d" (Counter.next counter))

  let to_string =
    Sym.to_string
end
