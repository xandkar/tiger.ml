module List = ListLabels

module A         = Tiger_absyn
module Env       = Tiger_env
module E         = Tiger_error
module Translate = Tiger_translate
module Type      = Tiger_env_type
module Value     = Tiger_env_value

(* The only reason for having this seemingly-superfluous inner module is to
 * have this nice signature as a summary of what each function does. *)
module Semant : sig
  type expty =
    { exp : Translate.exp
    ; ty  : Type.t
    }

  (* Violating normal naming convention just to make it easier to follow
   * Appel's
   *)
  val transExp : env:Env.t -> A.exp -> expty
  val transVar : env:Env.t -> A.var -> expty
  val transDec : env:Env.t -> A.dec -> Env.t
  val transTy  : env:Env.t -> A.ty  -> Type.t  (* needs only type env *)
end = struct
  type expty =
    { exp : Translate.exp
    ; ty  : Type.t
    }

  let unimplemented () =
    failwith "unimplemented"

  let return ty     = {exp = (); ty}
  let return_unit   = return Type.Unit
  let return_nil    = return Type.Nil
  let return_int    = return Type.Int
  let return_string = return Type.String

  let env_get_typ ~sym ~env ~pos : Type.t =
    match Env.get_typ env sym with
    | Some ty -> ty
    | None    -> E.raise (E.Unknown_type {ty_id=sym; pos})

  let env_get_val ~sym ~env ~pos : Value.t =
    match Env.get_val env sym with
    | Some ty -> ty
    | None    -> E.raise (E.Unknown_id {id=sym; pos})

  let check_same {exp=_; ty=ty_left} {exp=_; ty=ty_right} ~pos : unit =
    if Type.is_equal ty_left ty_right then
      ()
    else
      E.raise (E.Wrong_type {expected=ty_left; given=ty_right; pos})

  let check_int expty ~pos : unit =
    check_same {exp=(); ty=Type.Int} expty ~pos

  (* TODO: actual_ty *)

  let rec transExp ~env exp =
    let rec trexp exp =
      (match exp with
      | A.NilExp ->
          return_nil
      | A.IntExp _ ->
          return_int
      | A.StringExp {string=_; _} ->
          return_string
      | A.CallExp {func; args; pos} ->
          (match env_get_val ~sym:func ~env ~pos with
          | Value.Fun {formals; result} ->
              List.iter2 formals args ~f:(fun ty_expected exp_given ->
                check_same {exp=(); ty = ty_expected} (trexp exp_given) ~pos;
              );
              return result
          | Value.Var _ ->
              E.raise (E.Id_not_a_function {id=func; pos})
          )
      | A.OpExp {oper; left; right; pos} ->
          trop oper ~left ~right ~pos
      | A.RecordExp {fields=field_exps; typ; pos} ->
          let ty = env_get_typ ~sym:typ ~env ~pos in
          Type.if_record
            ty
            ~f:(fun field_tys ->
              List.iter field_exps ~f:(fun (field, exp, pos) ->
                (match List.assoc_opt field field_tys with
                | Some field_ty ->
                    check_same {exp=(); ty=field_ty} (trexp exp) ~pos
                | None ->
                    E.raise
                      (E.No_such_field_in_record {field; record=ty; pos})
                )
              )
            )
            ~otherwise:(fun () ->
              E.raise (E.Wrong_type_used_as_record {ty_id=typ; ty; pos})
            );
          return ty
      | A.SeqExp exps ->
          (* Ignoring value because we only care if a type-checking exception
           * is raised in one of trexp calls: *)
          List.iter exps ~f:(fun (exp, _) -> ignore (trexp exp));
          return_unit
      | A.AssignExp {var; exp; pos} ->
          check_same (trvar var) (trexp exp) ~pos;
          (* TODO: Add var->exp to val env? *)
          return_unit
      | A.IfExp {test; then'; else'; pos} ->
          (* test : must be int, because we have no bool *)
          (* then : must equal else *)
          (* else : must equal then or be None *)
          check_int (trexp test) ~pos;
          (match (trexp then', else') with
          | expty_then, None ->
              expty_then
          | expty_then, Some else' ->
              let expty_else = trexp else' in
              check_same expty_then expty_else ~pos;
              expty_then
          )
      | A.WhileExp {test; body; pos} ->
          (* test : must be int, because we have no bool *)
          check_int (trexp test) ~pos;
          ignore (trexp body);  (* Only care if a type-error is raised *)
          return_unit
      | A.ForExp {var; lo; hi; body; pos; escape=_} ->
          check_int (trexp lo) ~pos;
          check_int (trexp hi) ~pos;
          (* Only care if a type-error is raised *)
          ignore (transExp ~env:(Env.set_typ env var Type.Int) body);
          return_unit
      | A.BreakExp _ ->
          return_unit
      | A.LetExp {decs; body; pos=_} ->
          (* (1) decs augment env *)
          (* (2) body checked against the new env *)
          let env =
            List.fold_left decs ~init:env ~f:(fun env dec -> transDec dec ~env)
          in
          transExp body ~env
      | A.ArrayExp {typ; size; init; pos} ->
          check_int (trexp size) ~pos;
          let ty = env_get_typ ~sym:typ ~env ~pos in
          Type.if_array
            ty
            ~f:(fun ty_elements ->
              check_same {exp=(); ty=ty_elements} (trexp init) ~pos
            )
            ~otherwise:(fun () ->
              E.raise (E.Wrong_type_used_as_array {ty_id=typ; ty; pos})
            );
          return ty
      | A.VarExp var ->
          trvar var
      )
    and trvar =
      (function
      | A.SimpleVar {symbol=sym; pos} ->
          (match env_get_val ~sym ~env ~pos with
          | Value.Fun _    -> E.raise (E.Id_is_a_function {id=sym; pos})
          | Value.Var {ty} -> return ty
          )
      | A.FieldVar {var; symbol; pos} ->
          let {exp=_; ty} = trvar var in
          Type.if_record
            ty
            ~f:(fun fields ->
              (match List.assoc_opt symbol fields with
              | None ->
                  E.raise
                    (E.No_such_field_in_record {field=symbol; record=ty; pos})
              | Some ty ->
                  return ty
              )
            )
            ~otherwise:(fun () -> E.raise (E.Exp_not_a_record {ty; pos}))
      | A.SubscriptVar {var; exp; pos} ->
          let {exp=_; ty} = trvar var in
          check_int (trexp exp) ~pos;
          Type.if_array
            ty
            ~f:(fun ty_elements -> return ty_elements)
            ~otherwise:(fun () -> E.raise (E.Exp_not_an_array {ty; pos}))
      )
    and trop oper ~left ~right ~pos =
      let expty_left  = trexp left in
      let expty_right = trexp right in
      check_same expty_left expty_right ~pos;
      let {exp=_; ty} = expty_left in
      let module T = Type in
      (match oper with
      (* Arithmetic: int *)
      | A.PlusOp
      | A.MinusOp
      | A.TimesOp
      | A.DivideOp ->
          check_int expty_left ~pos;
          return_int
      (* Equality: int, string, array, record *)
      | A.EqOp
      | A.NeqOp ->
          if (T.is_int    ty)
          || (T.is_string ty)
          || (T.is_array  ty)
          || (T.is_record ty)
          then
            return ty
          else
            E.raise (E.Invalid_operand_type
              { oper
              ; valid = ["int"; "string"; "array"; "record"]
              ; given = ty
              ; pos
              })
      (* Order: int, string *)
      | A.LtOp
      | A.LeOp
      | A.GtOp
      | A.GeOp ->
          if (T.is_int    ty)
          || (T.is_string ty)
          then
            return ty
          else
            E.raise (E.Invalid_operand_type
              { oper
              ; valid = ["int"; "string"]
              ; given = ty
              ; pos
              })
      )
    in
    trexp exp
  and transDec ~env dec =
    (match dec with
    | A.VarDec {name; typ=typ_opt; init; pos=pos_outter; escape=_} ->
        let ty =
          (match (typ_opt, transExp ~env init) with
          | None, {ty; exp=()} ->
              ty
          | Some (sym, pos_inner), expty_init ->
              let ty = env_get_typ ~sym ~env ~pos:pos_inner in
              check_same {exp=(); ty} expty_init ~pos:pos_outter;
              ty
          )
        in
        Env.set_val env name (Value.Var {ty})
    | A.TypeDecs typedecs ->
        List.fold_left typedecs ~init:env ~f:(
          fun env (A.TypeDec {name; ty; pos=_}) ->
            let ty = transTy ~env ty in
            Env.set_typ env name ty
        )
    | A.FunDecs _ ->
        unimplemented ()
    )
  and transTy ~env typ =
    (match typ with
    | A.NameTy {symbol=sym; pos} ->
        env_get_typ ~sym ~env ~pos
    | A.RecordTy fields ->
        let fields =
          List.map fields ~f:(fun (A.Field {name; escape=_; typ; pos}) ->
            let ty = env_get_typ ~sym:typ ~env ~pos in
            (name, ty)
          )
        in
        Type.new_record fields
    | A.ArrayTy {symbol=sym; pos} ->
        let element_ty = env_get_typ ~sym ~env ~pos in
        Type.new_array element_ty
    )

  let transVar ~env:_ var =
    (match var with
    | A.SimpleVar {symbol=_; _} ->
        unimplemented ()
    | A.FieldVar {var=_; symbol=_; _} ->
        unimplemented ()
    | A.SubscriptVar {var=_; exp=_; _} ->
        unimplemented ()
    )
end

open Semant

let transProg absyn =
  let {exp = _; ty = _} = transExp absyn ~env:Env.base in
  ()
