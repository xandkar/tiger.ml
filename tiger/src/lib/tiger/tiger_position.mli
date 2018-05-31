type t =
  { file       : string
  ; start_char : int
  ; start_line : int
  ; end_char   : int
  ; end_line   : int
  }

val of_lexing_positions
  :  pos_start:Lexing.position
  -> pos_end:Lexing.position
  -> t
