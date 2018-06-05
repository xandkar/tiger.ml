type t

val case
  :  ?out_lexing  : Tiger_parser.token list
  -> ?out_parsing : Tiger_absyn.t
  -> code         : string
  -> string
  -> t

val run : t list -> unit
