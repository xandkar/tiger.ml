type t =
  { file       : string
  ; start_char : int
  ; start_line : int
  ; end_char   : int
  ; end_line   : int
  }

let of_lexing_positions
  ~pos_start:
    Lexing.({pos_fname=sfile; pos_lnum=sline; pos_bol=sbol; pos_cnum=scnum})
  ~pos_end:
    Lexing.({pos_fname=efile; pos_lnum=eline; pos_bol=ebol; pos_cnum=ecnum})
  =
    assert (sfile = efile);
    { file       = sfile
    ; start_char = scnum - sbol
    ; start_line = sline
    ; end_char   = ecnum - ebol
    ; end_line   = eline
    }

let to_string
  { file
  ; start_char
  ; start_line
  ; end_char
  ; end_line
  }
=
  Printf.sprintf
    "file: %S, between (line,char) %d,%d and %d,%d"
    file start_line start_char end_line end_char
