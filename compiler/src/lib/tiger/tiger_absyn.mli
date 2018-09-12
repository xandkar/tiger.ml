type oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

type exp =
  | NilExp
  | IntExp of
      int
  | StringExp of
      { string : string
      ; pos    : Tiger_position.t
      }
  | CallExp of
      { func : Tiger_symbol.t
      ; args : exp list
      ; pos  : Tiger_position.t
      }
  | OpExp of
      { left  : exp
      ; oper  : oper
      ; right : exp
      ; pos   : Tiger_position.t
      }
  | RecordExp of
      { fields : (Tiger_symbol.t * exp * Tiger_position.t) list
      ; typ    : Tiger_symbol.t
      ; pos    : Tiger_position.t
      }
  | SeqExp of
      (exp * Tiger_position.t) list
  | AssignExp of
      { var : var
      ; exp : exp
      ; pos : Tiger_position.t
      }
  | IfExp of
      { test  : exp
      ; then' : exp
      ; else' : exp option
      ; pos   : Tiger_position.t
      }
  | WhileExp of
      { test : exp
      ; body : exp
      ; pos  : Tiger_position.t
      }
  | ForExp of
      { var    : Tiger_symbol.t
      ; escape : bool ref  (* Whoa - why a mutable cell in AST? *)
      ; lo     : exp
      ; hi     : exp
      ; body   : exp
      ; pos    : Tiger_position.t
      }
  | BreakExp of
      Tiger_position.t
  | LetExp of
      { decs : dec list
      ; body : exp
      ; pos  : Tiger_position.t
      }
  | ArrayExp of
      { typ  : Tiger_symbol.t
      ; size : exp
      ; init : exp
      ; pos  : Tiger_position.t
      }
  | VarExp of
      var
and var =
  | SimpleVar of
      { symbol : Tiger_symbol.t
      ; pos    : Tiger_position.t
      }
  | FieldVar of
      { var    : var
      ; symbol : Tiger_symbol.t
      ; pos    : Tiger_position.t
      }
  | SubscriptVar of
      { var : var
      ; exp : exp
      ; pos : Tiger_position.t
      }
and dec =
  | FunDecs of  (* "FunctionDec" in Appel's code *)
      fundec list
  | VarDec of
      { name   : Tiger_symbol.t
      ; escape : bool ref  (* Again, why mutable? *)
      ; typ    : (Tiger_symbol.t * Tiger_position.t) option
      ; init   : exp
      ; pos    : Tiger_position.t
      }
  | TypeDecs of  (* "TypeDec" in Appel's code *)
      typedec list
and ty =
  | NameTy of
      { symbol : Tiger_symbol.t
      ; pos    : Tiger_position.t
      }
  | RecordTy of
      field list
  | ArrayTy of
      { symbol : Tiger_symbol.t
      ; pos    : Tiger_position.t
      }
and field =
  | Field of
    { name   : Tiger_symbol.t
    ; escape : bool ref
    ; typ    : Tiger_symbol.t
    ; pos    : Tiger_position.t
    }
and typedec =
  | TypeDec of  (* An anonymous record in Appel's code *)
      { name : Tiger_symbol.t
      ; ty   : ty
      ; pos  : Tiger_position.t
      }
and fundec =
  | FunDec of
    { name   : Tiger_symbol.t
    ; params : field list
    ; result : (Tiger_symbol.t * Tiger_position.t) option
    ; body   : exp
    ; pos    : Tiger_position.t
    }

type t = exp

val to_string : t -> string

val op_show : oper -> string
