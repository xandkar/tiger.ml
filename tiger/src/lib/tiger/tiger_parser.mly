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
%left OF DO
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
  ;

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
      let number_of_elements = $3 in
      let initial_value = $6 in
      sprintf
        "array[type[%s], size[%s], val[%s]]"
        type_id number_of_elements initial_value
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
  | ID LPAREN RPAREN
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
  | LPAREN exps RPAREN
    {
      sprintf "exps[%s]" $2
    }
  | LET decs IN exps END
    {
      let decs = $2 in
      let exps = $4 in
      sprintf "let[decs[%s], in[exps[%s]]]" decs exps
    }
  | LPAREN RPAREN
    {
      (* Perhaps "void"? *)
      "unit[]"
    }
  ;

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
  ;

exps:
  | exp
    {
      let exp = $1 in
      sprintf "%s" exp
    }
  | exp SEMICOLON exps
    {
      let exp = $1 in
      let exps = $3 in
      sprintf "%s; %s" exp exps
    }
  ;

decs:
  | dec
    {
      sprintf "%s" $1
    }
  | dec decs
    {
      sprintf "%s %s" $1 $2
    }
  ;

dec:
  /* Tydec */
  | TYPE ID EQ ID
    {
      let type_id_new = $2 in
      let type_id_orig = $4 in
      sprintf "tydec_alias[from[%s], to[%s]]" type_id_new type_id_orig
    }
  | TYPE ID EQ LBRACE RBRACE
    {
      let type_id = $2 in
      sprintf "tydec_empty_record[%s]" type_id
    }
  | TYPE ID EQ LBRACE tyfields RBRACE
    {
      let type_id = $2 in
      let tyfields = $5 in
      sprintf "tydec_record[%s, fields[%s]]" type_id tyfields
    }
  | TYPE ID EQ ARRAY OF ID
    {
      let type_id = $2 in
      let element_type_id = $6 in
      sprintf "tydec_array[%s, elements_of_type[%s]]" type_id element_type_id
    }

  /* Vardec */
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

  /* Fundec */
  | FUNCTION ID LPAREN RPAREN EQ exp
    {
      let id       = $2 in
      let exp      = $6 in
      sprintf "fundec[%s, arguments[], exp[%s]]" id exp
    }
  | FUNCTION ID LPAREN tyfields RPAREN EQ exp
    {
      let id       = $2 in
      let tyfields = $4 in
      let exp      = $7 in
      sprintf "fundec[%s, arguments[%s], exp[%s]]" id tyfields exp
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
  ;

tyfields:
  | ID COLON ID
    {
      let id_1 = $1 in
      let id_2 = $3 in
      sprintf "%s : %s" id_1 id_2
    }
  | ID COLON ID COMMA tyfields
    {
      let id_1 = $1 in
      let id_2 = $3 in
      let tyfield = sprintf "%s : %s" id_1 id_2 in
      let tyfields = $5 in
      sprintf "%s, %s" tyfield tyfields
    }
  ;

fun_args:
  | exp
    {
      $1
    }
  | exp COMMA fun_args
    {
      sprintf "%s, %s" $1 $3
    }
  ;

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
  ;

lvalue:
  | ID lvalue_part
    {
      let id = $1 in
      let part = $2 in
      sprintf "lvalue[%s, part[%s]]" id part
    }
  ;

lvalue_part:
  | {"epsilon[]"}
  | lvalue_subscript {$1}
  | lvalue_field_access {$1}
  ;

lvalue_subscript:
  | LBRACK exp RBRACK
    {
      let exp = $2 in
      sprintf "subscript[%s]" exp
    }
  ;

lvalue_field_access:
  | DOT ID
    {
      let field = $2 in
      sprintf "field_access[%s]" field
    }
  ;

%%
