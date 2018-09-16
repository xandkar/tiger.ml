type t

val case
  :  ?out_lexing  : Tiger_parser.token list option
  -> ?out_parsing : Tiger_absyn.t option
  -> ?is_error_expected_parsing : (Tiger_error.t -> bool) option
  -> ?is_error_expected_semant : (Tiger_error.t -> bool) option
  -> code         : string
  -> string
  -> t

val run : t list -> unit
