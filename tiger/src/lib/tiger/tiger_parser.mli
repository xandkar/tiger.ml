module Token : sig
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

  val to_string : t -> string
end
