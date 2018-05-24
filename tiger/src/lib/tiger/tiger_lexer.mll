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
      None
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

  | ":=" {Some ASSIGN}
  | "<=" {Some LE}
  | ">=" {Some GE}
  | "<>" {Some NEQ}
  | '&'  {Some AND}
  | '('  {Some LPAREN}
  | ')'  {Some RPAREN}
  | '*'  {Some TIMES}
  | '+'  {Some PLUS}
  | '-'  {Some MINUS}
  | '/'  {Some DIVIDE}
  | ','  {Some COMMA}
  | '.'  {Some DOT}
  | ':'  {Some COLON}
  | ';'  {Some SEMICOLON}
  | '>'  {Some GT}
  | '<'  {Some LT}
  | '='  {Some EQ}
  | '['  {Some LBRACK}
  | ']'  {Some RBRACK}
  | '{'  {Some LBRACE}
  | '}'  {Some RBRACE}
  | '|'  {Some OR}

  (* String literal *)
  | '"' {
      string_literal lexbuf
  }

  (* Drop whitespace *)
  | [' ' '\t'] {
      token lexbuf
  }

  | (num+ as int) {
    Some (INT (int_of_string int))
  }

  | (alpha (alpha | num | '_')* as id) {
      match id with
      | "array"    -> Some ARRAY
      | "break"    -> Some BREAK
      | "do"       -> Some DO
      | "else"     -> Some ELSE
      | "end"      -> Some END
      | "for"      -> Some FOR
      | "function" -> Some FUNCTION
      | "if"       -> Some IF
      | "in"       -> Some IN
      | "let"      -> Some LET
      | "nil"      -> Some NIL
      | "of"       -> Some OF
      | "then"     -> Some THEN
      | "to"       -> Some TO
      | "type"     -> Some TYPE
      | "var"      -> Some VAR
      | "while"    -> Some WHILE
      | _          -> Some (ID id)
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
      Some (STRING string)
  }


  | (_ as c) {
      Buffer.add_char string_buf c;
      string_literal lexbuf
  }
and comment = parse
  | eof {
      (* TODO: Error: unterminated comment? or we don't care? *)
      None
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
