{
  open Tiger_parser.Token

  let comment_level = ref 0
  let string_buf    = Buffer.create 100
}

let alpha = ['a'-'z' 'A'-'Z']
let num = ['0'-'9']
let newline = '\n' | '\r' | "\n\r"

rule token = parse
  | eof {
      EOF
  }

  (* Track line number *)
  | newline {
      Lexing.new_line lexbuf;
      token lexbuf
  }

  (* Comment *)
  | "/*" {
      incr comment_level;
      comment lexbuf
  }

  | ":=" {ASSIGN}
  | "<=" {LE}
  | ">=" {GE}
  | "<>" {NEQ}
  | '&'  {AND}
  | '('  {LPAREN}
  | ')'  {RPAREN}
  | '*'  {TIMES}
  | '+'  {PLUS}
  | '-'  {MINUS}
  | '/'  {DIVIDE}
  | ','  {COMMA}
  | '.'  {DOT}
  | ':'  {COLON}
  | ';'  {SEMICOLON}
  | '>'  {GT}
  | '<'  {LT}
  | '='  {EQ}
  | '['  {LBRACK}
  | ']'  {RBRACK}
  | '{'  {LBRACE}
  | '}'  {RBRACE}
  | '|'  {OR}

  (* String literal *)
  | '"' {
      string_literal lexbuf
  }

  (* Drop whitespace *)
  | [' ' '\t'] {
      token lexbuf
  }

  | (num+ as int) {
    INT (int_of_string int)
  }

  | (alpha (alpha | num | '_')* as id) {
      match id with
      | "array"    -> ARRAY
      | "break"    -> BREAK
      | "do"       -> DO
      | "else"     -> ELSE
      | "end"      -> END
      | "for"      -> FOR
      | "function" -> FUNCTION
      | "if"       -> IF
      | "in"       -> IN
      | "let"      -> LET
      | "nil"      -> NIL
      | "of"       -> OF
      | "then"     -> THEN
      | "to"       -> TO
      | "type"     -> TYPE
      | "var"      -> VAR
      | "while"    -> WHILE
      | _          -> ID id
  }
and string_literal = parse
  (* Keep escaped quote marks as part of the string literal *)
  | '\\' '"' {
      Buffer.add_char string_buf '"';
      string_literal lexbuf
  }

  | '"' {
      let string = Buffer.contents string_buf in
      Buffer.reset string_buf;
      STRING string
  }


  | (_ as c) {
      Buffer.add_char string_buf c;
      string_literal lexbuf
  }
and comment = parse
  | eof {
      (* TODO: Error: unterminated comment? or we don't care? *)
      EOF
  }

  (* Track line number *)
  | newline {
      Lexing.new_line lexbuf;
      comment lexbuf
  }

  | "/*" {
      incr comment_level;
      comment lexbuf
  }

  | "*/" {
      decr comment_level;
      match !comment_level with
      | 0            -> token lexbuf
      | n when n > 0 -> comment lexbuf
      | _            -> assert false
  }

  (* Drop comment contents *)
  | _ {
      comment lexbuf
  }
