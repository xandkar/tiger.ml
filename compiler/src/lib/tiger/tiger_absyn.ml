open Printf

module List   = ListLabels
module String = StringLabels

module Sym = Tiger_symbol
module Pos = Tiger_position

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
      ; pos    : Pos.t
      }
  | CallExp of
      { func : Sym.t
      ; args : exp list
      ; pos  : Pos.t
      }
  | OpExp of
      { left  : exp
      ; oper  : oper
      ; right : exp
      ; pos   : Pos.t
      }
  | RecordExp of
      { fields : (Sym.t * exp * Pos.t) list
      ; typ    : Sym.t
      ; pos    : Pos.t
      }
  | SeqExp of
      (exp * Pos.t) list
  | AssignExp of
      { var : var
      ; exp : exp
      ; pos : Pos.t
      }
  | IfExp of
      { test  : exp
      ; then' : exp
      ; else' : exp option
      ; pos   : Pos.t
      }
  | WhileExp of
      { test : exp
      ; body : exp
      ; pos  : Pos.t
      }
  | ForExp of
      { var    : Sym.t
      ; escape : bool ref  (* Whoa - why a mutable cell in AST? *)
      ; lo     : exp
      ; hi     : exp
      ; body   : exp
      ; pos    : Pos.t
      }
  | BreakExp of
      Pos.t
  | LetExp of
      { decs : dec list
      ; body : exp
      ; pos  : Pos.t
      }
  | ArrayExp of
      { typ  : Sym.t
      ; size : exp
      ; init : exp
      ; pos  : Pos.t
      }
  | VarExp of
      var
and var =
  | SimpleVar of
      { symbol : Sym.t
      ; pos    : Pos.t
      }
  | FieldVar of
      { var    : var
      ; symbol : Sym.t
      ; pos    : Pos.t
      }
  | SubscriptVar of
      { var : var
      ; exp : exp
      ; pos : Pos.t
      }
and dec =
  | FunDecs of  (* "FunctionDec" in Appel's code *)
      fundec list
  | VarDec of
      { name   : Sym.t
      ; escape : bool ref  (* Again, why mutable? *)
      ; typ    : (Sym.t * Pos.t) option
      ; init   : exp
      ; pos    : Pos.t
      }
  | TypeDecs of  (* "TypeDec" in Appel's code *)
      typedec list
and ty =
  | NameTy of
      { symbol : Sym.t
      ; pos    : Pos.t
      }
  | RecordTy of
      field list
  | ArrayTy of
      { symbol : Sym.t
      ; pos    : Pos.t
      }
and field =
  | Field of
    { name   : Sym.t
    ; escape : bool ref
    ; typ    : Sym.t
    ; pos    : Pos.t
    }
and typedec =
  | TypeDec of  (* An anonymous record in Appel's code *)
      { name : Sym.t
      ; ty   : ty
      ; pos  : Pos.t
      }
and fundec =
  | FunDec of
    { name   : Sym.t
    ; params : field list
    ; result : (Sym.t * Pos.t) option
    ; body   : exp
    ; pos    : Pos.t
    }

type t = exp

let op_to_string = function
  | PlusOp   -> "PlusOp"
  | MinusOp  -> "MinusOp"
  | TimesOp  -> "TimesOp"
  | DivideOp -> "DivideOp"
  | EqOp     -> "EqOp"
  | NeqOp    -> "NeqOp"
  | LtOp     -> "LtOp"
  | LeOp     -> "LeOp"
  | GtOp     -> "GtOp"
  | GeOp     -> "GeOp"

let xs_to_string ?(sep=", ") ~f xs =
  xs |> List.map ~f |> String.concat ~sep

let field_to_string (Field {name; typ; _}) =
  let name = Sym.to_string name in
  let typ  = Sym.to_string typ in
  name ^ " : " ^ typ

let fields_to_string fields =
  xs_to_string fields ~f:field_to_string
let rec exp_to_string exp =
  (match exp with
  | NilExp ->
      "NilExp[]"
  | IntExp i ->
      sprintf "IntExp[%d]" i
  | StringExp {string; _} ->
      sprintf "StringExp[%S]" string
  | CallExp {func; args; _} ->
      let func = Sym.to_string func in
      let args = xs_to_string args ~f:exp_to_string in
      sprintf "CallExp[%s, %s]" func args
  | OpExp {left; oper; right; _} ->
      let oper = op_to_string oper in
      let left = exp_to_string left in
      let right = exp_to_string right in
      sprintf "OpExp[%s[%s, %s]]" oper left right
  | RecordExp {fields; typ; _} ->
      let fields =
        xs_to_string
          fields
          ~f:(fun (s, e, _) -> (Sym.to_string s) ^ " = " ^ (exp_to_string e))
      in
      let typ = Sym.to_string typ in
      sprintf "RecordExp[%s, %s]" typ fields
  | SeqExp exps ->
      exps
      |> List.map ~f:(fun (exp, _pos) -> exp)
      |> xs_to_string ~f:exp_to_string
      |> sprintf "SeqExp[%s]"
  | AssignExp {var; exp; _} ->
      let var = var_to_string var in
      let exp = exp_to_string exp in
      sprintf "AssignExp[%s, %s]" var exp
  | IfExp {test; then'; else'; _} ->
      let test  = exp_to_string test in
      let then' = exp_to_string then' in
      (match else' with
      | None   -> sprintf "IfThen[%s, %s]"         test then'
      | Some e -> sprintf "IfThenElse[%s, %s, %s]" test then' (exp_to_string e)
      )
  | WhileExp {test; body; _} ->
      let test = exp_to_string test in
      let body = exp_to_string body in
      sprintf "WhileExp[%s, %s]" test body
  | ForExp {var; lo; hi; body; _} ->
      sprintf
        "ForExp[ForVar[%S], ForLo[%s], ForHi[%s], ForBody[%s]]"
        (Sym.to_string var)
        (exp_to_string lo)
        (exp_to_string hi)
        (exp_to_string body)
  | BreakExp _ ->
      "BreakExp[]"
  | LetExp {decs; body; _} ->
      let decs = xs_to_string decs ~f:dec_to_string in
      let body = exp_to_string body in
      sprintf "LetExp[LetDecs[%s], LetIn[%s]]" decs body
  | ArrayExp {typ; size; init; _} ->
      let typ = Sym.to_string typ in
      let size = exp_to_string size in
      let init = exp_to_string init in
      sprintf "ArrayExp[%s, %s, %s]" typ size init
  | VarExp var ->
      sprintf "VarExp[%s]" (var_to_string var)
  )
and var_to_string = function
  | SimpleVar {symbol; _} ->
      sprintf "SimpleVar[%s]" (Sym.to_string symbol)
  | FieldVar {var; symbol; _} ->
      sprintf "FieldVar[%s, %s]" (var_to_string var) (Sym.to_string symbol)
  | SubscriptVar {var; exp; _} ->
      sprintf "SubscriptVar[%s, %s]" (var_to_string var) (exp_to_string exp)
and dec_to_string = function
  | VarDec {name; typ; init; _} ->
      let name = Sym.to_string name in
      let init = exp_to_string init in
      (match typ with
      | Some (typ, _) ->
          let typ = Sym.to_string typ in
          sprintf "VarDec[%s, %s, %s]" name typ init
      | None ->
          sprintf "VarDec[%s, %s]" name     init
      )
  | TypeDecs type_decs ->
      sprintf "TypeDecs[%s]" (xs_to_string type_decs ~f:type_dec_to_string)
  | FunDecs fun_decs ->
      sprintf "FunDecs[%s]" (xs_to_string fun_decs ~f:fun_dec_to_string)
and fun_dec_to_string = function
  | FunDec {name; params; body; _} ->
      let name = Sym.to_string name in
      let params = fields_to_string params in
      let body = exp_to_string body in
      sprintf "FunDec[%s, FunParams[%s], FunBody[%s]]" name params body
and type_dec_to_string = function
  | TypeDec {name; ty; _} ->
      sprintf "TypeDec[%s, %s]" (Sym.to_string name) (ty_to_string ty)
and ty_to_string = function
  | NameTy   {symbol; _} -> sprintf "NameTy[%s]" (Sym.to_string symbol)
  | ArrayTy  {symbol; _} -> sprintf "ArrayTy[%s]" (Sym.to_string symbol)
  | RecordTy fields      -> sprintf "RecordTy[%s]" (fields_to_string fields)

let to_string = exp_to_string
