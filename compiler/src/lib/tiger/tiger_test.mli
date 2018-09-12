type t

val case
  :  ?out_lexing  : Tiger_parser.token list
  -> ?out_parsing : Tiger_absyn.t
  -> ?is_error_expected : (Tiger_error.t -> bool)
  -> code         : string
  -> string
  -> t

val run : t list -> unit
