module List = ListLabels

module A   = Tiger_absyn
module Opt = Tiger_opt
module Map = Tiger_map
module Sym = Tiger_symbol

type info =
  { depth   : int
  ; escapes : bool ref
  }

type env =
  (Sym.t, info) Map.t

let rec traverseExp ~(env : env) ~depth (exp : A.exp) =
  (match exp with
  | A.NilExp
  | A.IntExp _
  | A.StringExp _ ->
      ()
  | A.CallExp {func=_; args; pos=_} ->
      List.iter args ~f:(traverseExp ~env ~depth)
  | A.OpExp {oper=_; left; right; pos=_} ->
      traverseExp ~env ~depth left;
      traverseExp ~env ~depth right
  | A.RecordExp {fields=field_exps; typ=_; pos=_} ->
      List.iter field_exps ~f:(fun (_, exp, _) -> traverseExp ~env ~depth exp)
  | A.SeqExp exps ->
      List.iter exps ~f:(fun (exp, _) -> traverseExp ~env ~depth exp)
  | A.AssignExp {var; exp; pos=_} ->
      traverseVar ~env ~depth var;
      traverseExp ~env ~depth exp
  | A.IfExp {test; then'; else'; pos=_} ->
      traverseExp ~env ~depth test;
      traverseExp ~env ~depth then';
      Opt.iter else' ~f:(fun e -> traverseExp ~env ~depth e)
  | A.WhileExp {test; body; pos=_} ->
      traverseExp ~env ~depth test;
      traverseExp ~env ~depth body
  | A.ForExp {var=_; lo; hi; body; pos=_; escape=_} ->
      traverseExp ~env ~depth lo;
      traverseExp ~env ~depth hi;
      traverseExp ~env ~depth body
  | A.BreakExp _ ->
      ()
  | A.LetExp {decs; body; pos=_} ->
      traverseDecs ~env ~depth decs;
      traverseExp ~env ~depth body
  | A.ArrayExp {typ=_; size; init; pos=_} ->
      traverseExp ~env ~depth size;
      traverseExp ~env ~depth init
  | A.VarExp var ->
      traverseVar ~env ~depth var
  )
and traverseVar ~env ~depth (var : A.var) =
  (match var with
  | A.SimpleVar _ ->
      ()
  | A.FieldVar {var; symbol=_; pos=_} ->
      traverseVar ~env ~depth var
  | A.SubscriptVar {var; exp; pos=_} ->
      traverseVar ~env ~depth var;
      traverseExp ~env ~depth exp
  )
and traverseDecs ~env ~depth (decs : A.dec list) =
  List.iter decs ~f:(traverseDec ~env ~depth)
and traverseDec ~env ~depth (dec : A.dec) =
  (match dec with
  | A.FunDecs fundecs ->
      List.iter fundecs ~f:(
        fun (A.FunDec {name=_; params; result=_; body; pos=_}) ->
          traverseFields ~env ~depth params;
          traverseExp ~env ~depth body
      )
  | A.VarDec {name=_; escape=_; typ=_; init; pos=_} ->
      traverseExp ~env ~depth init
  | A.TypeDecs typedecs ->
      List.iter typedecs ~f:(fun (A.TypeDec {name=_; ty; pos=_}) ->
        match ty with
        | A.NameTy _
        | A.ArrayTy _ ->
            ()
        | A.RecordTy fields ->
            traverseFields ~env ~depth fields
      )
  )
and traverseFields ~env:_ ~depth:_ fields =
  List.iter fields ~f:(fun (A.Field {name=_; escape=_; typ=_; pos=_}) -> ())

let find ~prog =
  traverseExp ~env:Map.empty ~depth:0 prog
