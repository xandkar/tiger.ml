open Printf

module List   = ListLabels
module String = StringLabels

module Sym = Tiger_symbol
module Pos = Tiger_position

module Indent : sig
  type t
  val init : enabled:bool -> unit:string -> t
  val next : t -> t
  val to_string : t -> string
end = struct
  type t =
    { unit   : string option
    ; levels : int
    }

  let init ~enabled ~unit =
    { unit   = if enabled then Some unit else None
    ; levels = 0
    }

  let next t =
    {t with levels = succ t.levels}

  let to_string = function
    | {unit=None; _} ->
        ""
    | {unit=Some u; levels} ->
        let rec add = function
          | 0 -> ""
          | n -> u ^ (add (pred n))
        in
        "\n" ^ (add levels)
end

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

(* For printing error messages *)
let op_show op =
  match op with
  | PlusOp   -> "+"
  | MinusOp  -> "-"
  | TimesOp  -> "*"
  | DivideOp -> "/"
  | EqOp     -> "="
  | NeqOp    -> "<>"
  | LtOp     -> "<"
  | LeOp     -> "<="
  | GtOp     -> ">"
  | GeOp     -> ">="

(* For printing AST *)
let op_to_string op =
  match op with
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

let xs_to_string ?(sep=",") ~f ~indent xs =
  let i = Indent.to_string indent in
  xs |> List.map ~f:(fun x -> i ^ (f x)) |> String.concat ~sep

let mexp name params ~indent =
  let params = xs_to_string ~f:(fun x -> x) ~indent params in
  sprintf "%s[%s]" name params

let field_to_string ~indent (Field {name; typ; _}) =
  let name   = Sym.to_string name in
  let typ    = Sym.to_string typ in
  let indent = Indent.to_string indent in
  sprintf "%s%s : %s" indent name typ

let fields_to_string fields ~indent =
  let fields = List.map fields ~f:(field_to_string ~indent) in
  mexp "" fields ~indent

let rec exp_to_string ~indent exp =
  let indent = Indent.next indent in
  let mexp = mexp ~indent in
  (match exp with
  | NilExp ->
      mexp "NilExp" []
  | IntExp i ->
      mexp "IntExp" [(string_of_int i)]
  | StringExp {string; _} ->
      mexp "StringExp" [sprintf "%S" string]
  | CallExp {func; args; _} ->
      let func = Sym.to_string func in
      let args = List.map args ~f:(exp_to_string ~indent) in
      mexp "CallExp" [func; mexp "" args]
  | OpExp {left; oper; right; _} ->
      let op_exp =
        let indent = Indent.next indent in
        let oper   = op_to_string oper in
        let left   = exp_to_string ~indent left in
        let right  = exp_to_string ~indent right in
        mexp oper [left; right]
      in
      mexp "OpExp" [op_exp]
  | RecordExp {fields; typ; _} ->
      let fields =
        List.map fields ~f:(fun (sym, exp, _) ->
          sprintf
            "%s = %s"
            (Sym.to_string sym)
            (exp_to_string ~indent exp)
        )
      in
      let typ = Sym.to_string typ in
      mexp "RecordExp" [typ; mexp "" fields]
  | SeqExp exps ->
      exps
      |> List.map ~f:(fun (e, _) -> exp_to_string e ~indent)
      |> mexp "SeqExp"
  | AssignExp {var; exp; _} ->
      let var = var_to_string ~indent var in
      let exp = exp_to_string ~indent exp in
      mexp "AssignExp" [var; exp]
  | IfExp {test; then'; else'; _} ->
      let test  = exp_to_string ~indent test in
      let then' = exp_to_string ~indent then' in
      (match else' with
      | None ->
          mexp "IfThen"     [test; then']
      | Some e ->
          mexp "IfThenElse" [test; then'; (exp_to_string ~indent e)]
      )
  | WhileExp {test; body; _} ->
      let test = exp_to_string ~indent test in
      let body = exp_to_string ~indent body in
      mexp "WhileExp" [test; body]
  | ForExp {var; lo; hi; body; _} ->
      mexp "ForExp"
        [ (Sym.to_string var)
        ; (exp_to_string ~indent lo)
        ; (exp_to_string ~indent hi)
        ; (exp_to_string ~indent body)
        ]
  | BreakExp _ ->
      mexp "BreakExp" []
  | LetExp {decs; body; _} ->
      let decs = List.map decs ~f:(dec_to_string ~indent) in
      let body = exp_to_string ~indent body in
      mexp "LetExp" [mexp "" decs; body]
  | ArrayExp {typ; size; init; _} ->
      let typ = Sym.to_string typ in
      let size = exp_to_string ~indent size in
      let init = exp_to_string ~indent init in
      mexp "ArrayExp" [typ; size; init]
  | VarExp var ->
      mexp "VarExp" [(var_to_string ~indent var)]
  )
and var_to_string ~indent var =
  let indent = Indent.next indent in
  let mexp = mexp ~indent in
  match var with
  | SimpleVar {symbol; _} ->
      mexp "SimpleVar" [(Sym.to_string symbol)]
  | FieldVar {var; symbol; _} ->
      mexp "FieldVar"
        [ (var_to_string ~indent var)
        ; (Sym.to_string symbol)
        ]
  | SubscriptVar {var; exp; _} ->
      mexp "SubscriptVar[%s]"
        [ (var_to_string ~indent var)
        ; (exp_to_string ~indent exp)
        ]
and dec_to_string ~indent dec =
  let indent = Indent.next indent in
  let mexp = mexp ~indent in
  match dec with
  | VarDec {name; typ; init; _} ->
      let name = Sym.to_string name in
      let init = exp_to_string ~indent init in
      (match typ with
      | Some (typ, _) ->
          let typ = Sym.to_string typ in
          mexp "VarDec" [name; typ; init]
      | None ->
          mexp "VarDec" [name; init]
      )
  | TypeDecs type_decs ->
      mexp "TypeDecs"
        (List.map type_decs ~f:(type_dec_to_string ~indent))
  | FunDecs fun_decs ->
      mexp "FunDecs"
        (List.map fun_decs ~f:(fun_dec_to_string ~indent))
and fun_dec_to_string ~indent fun_dec =
  let indent = Indent.next indent in
  let mexp = mexp ~indent in
  match fun_dec with
  | FunDec {name; params; body; _} ->
      let name = Sym.to_string name in
      let params = fields_to_string ~indent params in
      let body = exp_to_string ~indent body in
      mexp "FunDec" [name; params; body]
and type_dec_to_string ~indent type_dec =
  let indent = Indent.next indent in
  let mexp = mexp ~indent in
  match type_dec with
  | TypeDec {name; ty; _} ->
      mexp "TypeDec"
        [ (Sym.to_string name)
        ; (ty_to_string ~indent ty)
        ]
and ty_to_string ~indent ty =
  let mexp = mexp ~indent in
  match ty with
  | NameTy   {symbol; _} -> mexp "NameTy"   [(Sym.to_string symbol)]
  | ArrayTy  {symbol; _} -> mexp "ArrayTy"  [(Sym.to_string symbol)]
  | RecordTy fields      -> mexp "RecordTy" [(fields_to_string ~indent fields)]

let to_string =
  let unit = String.make 4 ' ' in
  let indent = Indent.init ~enabled:true ~unit in
  exp_to_string ~indent
