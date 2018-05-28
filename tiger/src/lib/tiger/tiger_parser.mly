%{
  open Printf
%}

/* Declarations */
%token AND
%token ARRAY
%token ASSIGN
%token BREAK
%token COLON
%token COMMA
%token DIVIDE
%token DO
%token DOT
%token ELSE
%token END
%token EOF
%token EQ
%token FOR
%token FUNCTION
%token GE
%token GT
%token <string> ID
%token IF
%token IN
%token <int> INT
%token LBRACE
%token LBRACK
%token LE
%token LET
%token LPAREN
%token LT
%token MINUS
%token NEQ
%token NIL
%token OF
%token OR
%token PLUS
%token RBRACE
%token RBRACK
%token RPAREN
%token SEMICOLON
%token <string> STRING
%token THEN
%token TIMES
%token TO
%token TYPE
%token VAR
%token WHILE

/* from lowest precedence */
%left OR
%left AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
/* to highest precedence */

%type <string> program

%start program

%%

program:
  | exp EOF
    {
      sprintf "program[%s]" $1
    }

exp:
  | NIL
    {
      "nil[]"
    }
  | INT
    {
      sprintf "int[%d]" $1
    }
  | MINUS exp %prec UMINUS
    {
      sprintf "negation[%s]" $2
    }
  | ID LBRACK exp RBRACK OF exp
    {
      let type_id = $1 in
      let exp_1 = $3 in
      let exp_2 = $6 in
      sprintf "array[type[%s], size[%s], val[%s]]" type_id exp_1 exp_2
    }
  | ID LBRACE rec_field_assignments RBRACE
    {
      let type_id = $1 in
      let rec_field_assignments = $3 in
      sprintf
        "record[type[%s], rec_field_assignments[%s]]"
        type_id rec_field_assignments
    }
  | lvalue
    {
      $1
    }
  | lvalue ASSIGN exp
    {
      sprintf "assign[%s := %s]" $1 $3
    }
  | STRING
    {
      sprintf "string[%S]" $1
    }
  | ID unit
    {
      let id = $1 in
      sprintf "fun_call[%s, []]" id
    }
  | ID LPAREN fun_args RPAREN
    {
      let id = $1 in
      let fun_args = $3 in
      sprintf "fun_call[%s, %s]" id fun_args
    }
  | exp op exp
    {
      sprintf "op[%s %s %s]" $1 $2 $3
    }
  | IF exp THEN exp ELSE exp
    {
      let e1 = $2 in
      let e2 = $4 in
      let e3 = $6 in
      sprintf "if_then_else[%s, then[%s], else[%s]]" e1 e2 e3
    }
  | IF exp THEN exp
    {
      sprintf "if_then[%s, then[%s]]" $2 $4
    }
  | WHILE exp DO exp
    {
      sprintf "while[%s, do[%s]]" $2 $4
    }
  | FOR ID ASSIGN exp TO exp DO exp
    {
      let id = $2 in
      let e1 = $4 in
      let e2 = $6 in
      let e3 = $8 in
      sprintf "for[%s := %s, to[%s], do[%s]]" id e1 e2 e3
    }
  | BREAK
    {
      "break[]"
    }
  | LPAREN seq RPAREN
    {
      sprintf "seq[%s]" $2
    }
  | LET decs IN seq END
    {
      let decs = $2 in
      let seq = $4 in
      sprintf "let[decs[%s], in[seq[%s]]]" decs seq
    }
  | unit
    {
      $1
    }

seq:
  | exp
    {
      sprintf "%s" $1
    }
  | exp SEMICOLON seq
    {
      sprintf "%s; %s" $1 $3
    }

decs:
  | dec
    {
      sprintf "%s" $1
    }
  | dec decs
    {
      sprintf "%s %s" $1 $2
    }

dec:
  | tydec  {$1}
  | vardec {$1}
  | fundec {$1}

fundec:
  | FUNCTION ID unit EQ exp
    {
      let id       = $2 in
      let exp      = $5 in
      sprintf "fundec[%s, exp[%s]]" id exp
    }
  | FUNCTION ID LPAREN tyfields RPAREN EQ exp
    {
      let id       = $2 in
      let tyfields = $4 in
      let exp      = $7 in
      sprintf "fundec[%s, tyfields[%s], exp[%s]]" id tyfields exp
    }
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp
    {
      let id       = $2 in
      let tyfields = $4 in
      let type_id  = $7 in
      let exp      = $9 in
      sprintf
        "fundec[%s, tyfields[%s], type_id[%s], exp[%s]]"
        id tyfields type_id exp
    }

vardec:
  | VAR ID ASSIGN exp
    {
      let id = $2 in
      let exp = $4 in
      sprintf "vardec[%s, exp[%s]]" id exp
    }
  | VAR ID COLON ID ASSIGN exp
    {
      let id = $2 in
      let type_id = $4 in
      let exp = $6 in
      sprintf "vardec[%s, type_id[%s], exp[%s]]" id type_id exp
    }

tydec:
  | TYPE ID EQ ty
    {
      let type_id = $2 in
      let ty = $4 in
      sprintf "tydec[%s, %s]" type_id ty
    }

ty:
  | ID
    {
      let type_id = $1 in
      sprintf "type[type_id[%S]]" type_id
    }
  | LBRACE RBRACE
    {
      "record[]"
    }
  | LBRACE tyfields RBRACE
    {
      let tyfields = $2 in
      sprintf "record[%s]" tyfields
    }
  | ARRAY OF ID
    {
      let type_id = $3 in
      sprintf "array_of_type[%s]" type_id
    }

tyfields:
/*| epsilon */
  | tyfield
    {$1}
  | tyfield COMMA tyfields
    {
      let tyfield = $1 in
      let tyfields = $3 in
      sprintf "%s, %s" tyfield tyfields
    }

tyfield:
  | ID COLON ID
    {
      let id = $1 in
      let type_id = $3 in
      sprintf "tyfield[%s, %s]" id type_id
    }

/* Perhaps "void"? */
unit:
  | LPAREN RPAREN
    {
      "unit[]"
    }

rec_field_assignments:
  | ID EQ exp
    {
      let id = $1 in
      let exp = $3 in
      sprintf "%S = %s" id exp
    }
  | ID EQ exp COMMA rec_field_assignments
    {
      let id = $1 in
      let exp = $3 in
      let rec_field_assignments = $5 in
      sprintf "%S = %s, %s" id exp rec_field_assignments
    }

fun_args:
  | exp
    {
      $1
    }
  | exp COMMA fun_args
    {
      sprintf "%s, %s" $1 $3
    }

op:
  | PLUS   {"+"}
  | MINUS  {"-"}
  | TIMES  {"*"}
  | DIVIDE {"/"}
  | EQ     {"="}
  | NEQ    {"<>"}
  | GT     {">"}
  | LT     {"<"}
  | GE     {">="}
  | LE     {"<="}
  | AND    {"&"}
  | OR     {"|"}

lvalue:
  | ID
    {
      let id = $1 in
      sprintf "lvalue[%s]" id
    }
  | lvalue DOT ID
    {
      let record = $1 in
      let field = $3 in
      sprintf "get_record_field[%s, %s]" record field
    }
  | lvalue LBRACK exp RBRACK
    {
      let array = $1 in
      let subscript = $3 in
      sprintf "get_array_subscript[%s, %s]" array subscript
    }

%%
