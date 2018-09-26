(* "We use the word _temporary_ to mean a value that is temporarily held in a
 * register, and the word _label_ to mean some machine-language location whose
 * exact address is yet to be determined - just like a label in assembly
 * language." ch. 6.2, p. 139 *)

(* "temp" is an abstract name of "local variable" *)
module Temp : sig
  type t

  val gen : unit -> t
  (** "newtemp" -  new temporary from an infinite set of temps. *)

  val to_string : t -> string
end

(* "label" is an abstract name for "static memory address" *)
module Label : sig
  type t

  val gen : unit -> t
  (** "newlabel" -  new label from an infinite set of labels. *)

  val of_string : string -> t
  (** "namedlabel" -  new label whose assembly-language name is string. *)

  val to_string : t -> string
end
