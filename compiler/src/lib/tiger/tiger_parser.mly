%{
  module Ast = Tiger_absyn
  module Sym = Tiger_symbol

  let pos () =
    Tiger_position.of_lexing_positions
      ~pos_start:(Parsing.symbol_start_pos ())
      ~pos_end:(Parsing.symbol_end_pos ())
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
%nonassoc THEN
%nonassoc ELSE
%nonassoc ASSIGN
%nonassoc OF DO
%left OR
%left AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc HIGHEST
/* to highest precedence */

%type <Tiger_absyn.t> program

%start program

%%

program:
  | exp EOF { $1 }
  | error {Tiger_error.exn ~pos:(pos ()) ~msg:"invalid syntax"}
  ;

exp:
  | NIL
    { Ast.NilExp }
  | INT
    { Ast.IntExp $1 }
  | MINUS exp %prec HIGHEST
    {
      Ast.OpExp
        { left  = Ast.IntExp 0
        ; oper  = Ast.MinusOp
        ; right = $2
        ; pos   = pos ()
        }
    }
  | lvalue LBRACK exp RBRACK OF exp
    {
      match $1 with
      | Ast.SimpleVar {symbol=typ; _} ->
          Ast.ArrayExp
            { typ
            ; size = $3
            ; init = $6
            ; pos  = pos ()
            }
      | Ast.SubscriptVar _ | Ast.FieldVar _ ->
          raise Parse_error
    }
  | ID LBRACE rec_fields_bind RBRACE
    {
      let type_id = $1 in
      let fields  = $3 in
      let typ     = Sym.of_string type_id in
      let pos     = pos () in
      Ast.RecordExp {fields; typ; pos}
    }
  | lvalue
    { Ast.VarExp $1 }
  | lvalue ASSIGN exp
    {
      let var = $1 in
      let exp = $3 in
      let pos = pos () in
      Ast.AssignExp {var; exp; pos}
    }
  | STRING
    { Ast.StringExp {string = $1; pos = pos ()} }
  | ID LPAREN fun_args RPAREN
    {
      Ast.CallExp
        { func = Sym.of_string $1
        ; args = $3
        ; pos  = pos ()
        }
    }
  | exp PLUS exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.PlusOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp MINUS exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.MinusOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp TIMES exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.TimesOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp DIVIDE exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.DivideOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp EQ exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.EqOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp NEQ exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.NeqOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp GT exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.GtOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp LT exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.LtOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp GE exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.GeOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp LE exp
    {
      Ast.OpExp
        { left  = $1
        ; oper  = Ast.LeOp
        ; right = $3
        ; pos   = pos ()
        }
    }
  | exp AND exp
    {
      let e1 = $1 in
      let e2 = $3 in
      Ast.IfExp
        { test  = e1
        ; then' = e2
        ; else' = Some (Ast.IntExp 0)
        ; pos   = pos ()
        }
    }
  | exp OR exp
    {
      let e1 = $1 in
      let e2 = $3 in
      Ast.IfExp
        { test  = e1
        ; then' = Ast.IntExp 1
        ; else' = Some e2
        ; pos   = pos ()
        }
    }
  | IF exp THEN exp ELSE exp
    {
      let e1 = $2 in
      let e2 = $4 in
      let e3 = $6 in
      Ast.IfExp
        { test  = e1
        ; then' = e2
        ; else' = Some e3
        ; pos   = pos ()
        }
    }
  | IF exp THEN exp
    {
      let e1 = $2 in
      let e2 = $4 in
      Ast.IfExp
        { test  = e1
        ; then' = e2
        ; else' = None
        ; pos   = pos ()
        }
    }
  | WHILE exp DO exp
    {
      let e1 = $2 in
      let e2 = $4 in
      Ast.WhileExp
        { test = e1
        ; body = e2
        ; pos  = pos ()
        }
    }
  | FOR ID ASSIGN exp TO exp DO exp
    {
      let var = $2 in
      let e1  = $4 in
      let e2  = $6 in
      let e3  = $8 in
      Ast.ForExp
        { var    = Sym.of_string var
        ; escape = ref true
        ; lo     = e1
        ; hi     = e2
        ; body   = e3
        ; pos    = pos ()
        }
    }
  | BREAK
    { Ast.BreakExp (pos ()) }
  | LPAREN exps RPAREN
    { Ast.SeqExp $2 }
  | LET decs IN exps END
    {
      let decs = $2 in
      let exps = $4 in
      Ast.LetExp {decs; body = Ast.SeqExp exps; pos = pos ()}
    }
  ;

exps:
  |                    {                 [] }
  | exp                { ($1, pos ()) :: [] }
  | exp SEMICOLON exps { ($1, pos ()) :: $3 }
  ;

rec_fields_bind:
  | ID EQ exp                       { (Sym.of_string $1, $3, pos ()) :: [] }
  | ID EQ exp COMMA rec_fields_bind { (Sym.of_string $1, $3, pos ()) :: $5 }
  ;

/* ------------------------------------------------------------------------- */
/* BEGIN unintuitive rules for decs (which avoid shift/reduce conflicts)     */
/* ------------------------------------------------------------------------- */
/*
  In order to support mutual recursion, we need to group consecutive
  type and function declarations (see Tiger-book pages 97-99).

  Initially, I defined the rules to do so as:

    decs:
      | dec      { $1 :: [] }
      | dec decs { $1 :: $2 }
      ;
    dec:
      | var_dec  { $1 }
      | typ_decs { Ast.TypeDecs $1 }
      | fun_decs { Ast.FunDecs $1 }
      ;

  which, while straightforward (and working, because ocamlyacc defaults to
  shift in case of a conflict), nonetheless caused a shift/reduce conflict in
  each of: typ_decs and fun_decs; where the parser did not know whether to
  shift and stay in (typ|fun_)_dec state or to reduce and get back to dec
  state.

  Sadly, tagging the rules with a lower precedence (to explicitly favor
  shifting) - does not help :(

    %nonassoc LOWEST
    ...
    dec:
      | var_dec                { $1 }
      | typ_decs  %prec LOWEST { Ast.TypeDecs $1 }
      | fun_decs  %prec LOWEST { Ast.FunDecs $1 }
      ;

  The difficulty seems to be in the lack of a separator token which would be
  able to definitively mark the end of each sequence of consecutive
  (typ_|fun_) declarations.

  Keeping this in mind, another alternative is to manually capture the possible
  interspersion patterns in the rules like:

    (N * foo) followed-by (N * not-foo)

  for the exception of var_dec, which, since we do not need to group its
  consecutive sequences, can be reduced upon first sighting.
*/

decs:
  | var_dec   decs_any          { $1 :: $2 }
  | fun_decs  decs_any_but_fun  { (Ast.FunDecs  $1) :: $2 }
  | typ_decs  decs_any_but_typ  { (Ast.TypeDecs $1) :: $2 }
  ;

decs_any:
  |                             { [] }
  | var_dec   decs_any          { $1 :: $2 }
  | fun_decs  decs_any_but_fun  { (Ast.FunDecs  $1) :: $2 }
  | typ_decs  decs_any_but_typ  { (Ast.TypeDecs $1) :: $2 }
  ;

decs_any_but_fun:
  |                             { [] }
  | var_dec   decs_any          { $1 :: $2 }
  | typ_decs  decs_any_but_typ  { (Ast.TypeDecs $1) :: $2 }
  ;

decs_any_but_typ:
  |                             { [] }
  | var_dec   decs_any          { $1 :: $2 }
  | fun_decs  decs_any_but_fun  { (Ast.FunDecs $1) :: $2 }
  ;

/*---------------------------------------------------------------------------*/
/* END unintuitive rules for decs (which avoid shift/reduce conflicts)       */
/*---------------------------------------------------------------------------*/

typ_decs:
  | typ_dec          { $1 :: [] }
  | typ_dec typ_decs { $1 :: $2 }
  ;

typ_dec:
  | TYPE ID EQ ID
    {
      let type_id_left = $2 in
      let type_id_right = $4 in
      let pos = pos () in  (* FIXME: rhs id should have its own pos, no? *)
      Ast.TypeDec
        { name = Sym.of_string type_id_left
        ; ty   = Ast.NameTy {symbol = Sym.of_string type_id_right; pos}
        ; pos
        }
    }
  | TYPE ID EQ LBRACE type_fields RBRACE
    {
      let type_id = $2 in
      let type_fields = $5 in
      Ast.TypeDec
        { name = Sym.of_string type_id
        ; ty   = Ast.RecordTy type_fields
        ; pos  = pos ()
        }
    }
  | TYPE ID EQ ARRAY OF ID
    {
      let type_id         = Sym.of_string $2 in
      let element_type_id = Sym.of_string $6 in
      let pos = pos () in
      Ast.TypeDec
        { name = type_id
        ; ty   = Ast.ArrayTy {symbol = element_type_id; pos}
        ; pos
        }
    }
  ;

var_dec:
  | VAR ID maybe_type_sig ASSIGN exp
    {
      let var_id = Sym.of_string $2 in
      let maybe_type_sig = $3 in
      let exp = $5 in
      let pos = pos () in
      Ast.VarDec
        { name   = var_id
        ; escape = ref true
        ; typ    = maybe_type_sig
        ; init   = exp
        ; pos
        }
    }
  ;

fun_decs:
  | fun_dec          { $1 :: [] }
  | fun_dec fun_decs { $1 :: $2 }
  ;

fun_dec:
  | FUNCTION ID LPAREN type_fields RPAREN maybe_type_sig EQ exp
    {
      let name   = Sym.of_string $2 in
      let params = $4 in
      let result = $6 in
      let body   = $8 in
      let pos    = pos () in
      Ast.FunDec {name; params; result; body; pos}
    }
  ;

maybe_type_sig:
  |          { None }
  | COLON ID { Some (Sym.of_string $2, pos ()) }
  ;

type_fields:
  |
    { [] }
  | ID COLON ID
    {
      let field =
        Ast.Field
          { name   = Sym.of_string $1
          ; escape = ref true
          ; typ    = Sym.of_string $3
          ; pos    = pos ()
          }
      in
      field :: []
    }
  | ID COLON ID COMMA type_fields
    {
      let field =
        Ast.Field
          { name   = Sym.of_string $1
          ; escape = ref true
          ; typ    = Sym.of_string $3
          ; pos    = pos ()
          }
      in
      field :: $5
    }
  ;

fun_args:
  |                    {       [] }
  | exp                { $1 :: [] }
  | exp COMMA fun_args { $1 :: $3 }
  ;

lvalue:
  | ID
    {
      Ast.SimpleVar
        { symbol = Sym.of_string $1
        ; pos    = pos ()
        }
    }
  | lvalue LBRACK exp RBRACK
    {
      Ast.SubscriptVar
        { var = $1
        ; exp = $3
        ; pos = pos ()
        }
    }
  | lvalue DOT ID
    {
      Ast.FieldVar
        { var    = $1
        ; symbol = Sym.of_string $3
        ; pos    = pos ()
        }
    }
  ;

%%
