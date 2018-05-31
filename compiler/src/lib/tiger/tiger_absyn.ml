type pos = Tiger_position.t

type symbol = Tiger_symbol.t

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
  | VarExp of
      var
  | NilExp
  | IntExp of
      int
  | StringExp of
      { string : string
      ; pos    : pos
      }
  | CallExp of
      { func : symbol
      ; args : exp list
      ; pos  : pos
      }
  | OpExp of
      { left  : exp
      ; oper  : oper
      ; right : exp
      ; pos   : pos
      }
  | RecordExp of
      { fields : (symbol * exp * pos) list
      ; typ    : symbol
      ; pos    : pos
      }
  | SeqExp of
      (exp * pos) list
  | AssignExp of
      { var : var
      ; exp : exp
      ; pos : pos
      }
  | IfExp of
      { test  : exp
      ; then' : exp
      ; else' : exp option
      ; pos   : pos
      }
  | WhileExp of
      { test : exp
      ; body : exp
      ; pos  : pos
      }
  | ForExp of
      { var    : symbol
      ; escape : bool ref  (* Whoa - why a mutable cell in AST? *)
      ; lo     : exp
      ; hi     : exp
      ; body   : exp
      ; pos    : pos
      }
  | BreakExp of
      pos
  | LetExp of
      { decs : dec list
      ; body : exp
      ; pos  : pos
      }
  | ArrayExp of
      { typ  : symbol
      ; size : exp
      ; init : exp
      ; pos  : pos
      }
and var =
  | SimpleVar of
      { symbol : symbol
      ; pos    : pos
      }
  | FieldVar of
      { var    : var
      ; symbol : symbol
      ; pos    : pos
      }
  | SubscriptVar of
      { var : var
      ; exp : exp
      ; pos : pos
      }
and dec =
  | FunDecs of  (* "FunctionDec" in Appel's code *)
      fundec list
  | VarDec of
      { name   : symbol
      ; escape : bool ref  (* Again, why mutable? *)
      ; typ    : (symbol * pos) option
      ; init   : exp
      ; pos    : pos
      }
  | TypeDecs of  (* "TypeDec" in Appel's code *)
      typedec list
and ty =
  | NameTy of
      { symbol : symbol
      ; pos    : pos
      }
  | RecordTy of
      field list
  | ArrayTy of
      { symbol : symbol
      ; pos    : pos
      }
and field =
  | Field of
    { name   : symbol
    ; escape : bool ref
    ; typ    : symbol
    ; pos    : pos
    }
and typedec =
  | TypeDec of  (* An anonymous record in Appel's code *)
      { name : symbol
      ; ty   : ty
      ; pos  : pos
      }
and fundec =
  | FunDec of
    { name   : symbol
    ; params : field list
    ; result : (symbol * pos) option
    ; body   : exp
    ; pos    : pos
    }

type t = exp

let to_string _ = "TODO: implement Tiger_absyn.to_string"
