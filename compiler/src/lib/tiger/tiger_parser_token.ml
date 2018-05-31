open Printf

module T = Tiger_parser

type t = T.token

let to_string = function
  | T.TYPE      -> "TYPE"
  | T.VAR       -> "VAR"
  | T.FUNCTION  -> "FUNCTION"
  | T.BREAK     -> "BREAK"
  | T.OF        -> "OF"
  | T.END       -> "END"
  | T.IN        -> "IN"
  | T.NIL       -> "NIL"
  | T.LET       -> "LET"
  | T.DO        -> "DO"
  | T.TO        -> "TO"
  | T.FOR       -> "FOR"
  | T.WHILE     -> "WHILE"
  | T.ELSE      -> "ELSE"
  | T.THEN      -> "THEN"
  | T.IF        -> "IF"
  | T.ARRAY     -> "ARRAY"
  | T.ASSIGN    -> "ASSIGN"
  | T.OR        -> "OR"
  | T.AND       -> "AND"
  | T.GE        -> "GE"
  | T.GT        -> "GT"
  | T.LE        -> "LE"
  | T.LT        -> "LT"
  | T.NEQ       -> "NEQ"
  | T.EQ        -> "EQ"
  | T.DIVIDE    -> "DIVIDE"
  | T.TIMES     -> "TIMES"
  | T.MINUS     -> "MINUS"
  | T.PLUS      -> "PLUS"
  | T.DOT       -> "DOT"
  | T.RBRACE    -> "RBRACE"
  | T.LBRACE    -> "LBRACE"
  | T.RBRACK    -> "RBRACK"
  | T.LBRACK    -> "LBRACK"
  | T.RPAREN    -> "RPAREN"
  | T.LPAREN    -> "LPAREN"
  | T.SEMICOLON -> "SEMICOLON"
  | T.COLON     -> "COLON"
  | T.COMMA     -> "COMMA"
  | T.STRING s  -> sprintf "STRING (%S)" s
  | T.INT    i  -> sprintf "INT (%d)" i
  | T.ID     id -> sprintf "ID (%s)" id
  | T.EOF       -> "EOF"
