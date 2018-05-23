open Printf

module Token = struct
  type t =
    | AND
    | ARRAY
    | ASSIGN
    | BREAK
    | COLON
    | COMMA
    | DIVIDE
    | DO
    | DOT
    | ELSE
    | END
    | EOF
    | EQ
    | FOR
    | FUNCTION
    | GE
    | GT
    | ID of string
    | IF
    | IN
    | INT of int
    | LBRACE
    | LBRACK
    | LE
    | LET
    | LPAREN
    | LT
    | MINUS
    | NEQ
    | NIL
    | OF
    | OR
    | PLUS
    | RBRACE
    | RBRACK
    | RPAREN
    | SEMICOLON
    | STRING of string
    | THEN
    | TIMES
    | TO
    | TYPE
    | VAR
    | WHILE

  let to_string = function
    | TYPE      -> "TYPE"
    | VAR       -> "VAR"
    | FUNCTION  -> "FUNCTION"
    | BREAK     -> "BREAK"
    | OF        -> "OF"
    | END       -> "END"
    | IN        -> "IN"
    | NIL       -> "NIL"
    | LET       -> "LET"
    | DO        -> "DO"
    | TO        -> "TO"
    | FOR       -> "FOR"
    | WHILE     -> "WHILE"
    | ELSE      -> "ELSE"
    | THEN      -> "THEN"
    | IF        -> "IF"
    | ARRAY     -> "ARRAY"
    | ASSIGN    -> "ASSIGN"
    | OR        -> "OR"
    | AND       -> "AND"
    | GE        -> "GE"
    | GT        -> "GT"
    | LE        -> "LE"
    | LT        -> "LT"
    | NEQ       -> "NEQ"
    | EQ        -> "EQ"
    | DIVIDE    -> "DIVIDE"
    | TIMES     -> "TIMES"
    | MINUS     -> "MINUS"
    | PLUS      -> "PLUS"
    | DOT       -> "DOT"
    | RBRACE    -> "RBRACE"
    | LBRACE    -> "LBRACE"
    | RBRACK    -> "RBRACK"
    | LBRACK    -> "LBRACK"
    | RPAREN    -> "RPAREN"
    | LPAREN    -> "LPAREN"
    | SEMICOLON -> "SEMICOLON"
    | COLON     -> "COLON"
    | COMMA     -> "COMMA"
    | STRING s  -> sprintf "STRING (%S)" s
    | INT    i  -> sprintf "INT (%d)" i
    | ID     id -> sprintf "ID (%s)" id
    | EOF       -> "EOF"
end
