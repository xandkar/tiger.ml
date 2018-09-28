module List = ListLabels

module A         = Tiger_absyn
module Dag       = Tiger_dag
module Env       = Tiger_env
module E         = Tiger_error
module Escape    = Tiger_semant_escape
module Pos       = Tiger_position
module Sym       = Tiger_symbol
module Temp      = Tiger_temp
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

  (* transVar does not seem to be needed, as trvar handles all our cases.
   * Am I wrong?
   *
   * val transVar : env:Env.t -> A.var -> expty
   *
   *)
end = struct
  type expty =
    { exp : Translate.exp
    ; ty  : Type.t
    }

  let rec actual_ty ty ~pos =
    match ty with
    | Type.Name (name, ty_opt_ref) ->
        (match !ty_opt_ref with
        | None ->
            E.raise (E.Unknown_type {ty_id=name; pos})
        | Some ty ->
            actual_ty ty ~pos
        )
    | Type.Unit
    | Type.Nil
    | Type.Int
    | Type.String
    | Type.Record _
    | Type.Array _ ->
        ty

  let return ty     = {exp = Translate.dummy__FIXME; ty}
  let return_unit   = return Type.Unit
  let return_nil    = return Type.Nil
  let return_int    = return Type.Int
  let return_string = return Type.String

  let env_get_typ ~sym ~env ~pos : Type.t =
    match Env.get_typ env sym with
    | Some ty -> ty
    | None    -> E.raise (E.Unknown_type {ty_id=sym; pos})

  let env_get_typ_actual ~sym ~env ~pos : Type.t =
    actual_ty (env_get_typ ~sym ~env ~pos) ~pos

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
    check_same return_int expty ~pos

  let paths_of_typedecs typedecs : (Sym.t * Sym.t * Pos.t) list list =
    let (path, paths) =
      List.fold_left typedecs ~init:([], []) ~f:(
        fun (path, paths) (A.TypeDec {name=child; ty; pos}) ->
          match ty with
          | A.NameTy {symbol=parent; _} ->
              (((parent, child, pos) :: path), paths)
          | A.RecordTy _
          | A.ArrayTy _ ->
              ([], path :: paths)
      )
    in
    List.map (path :: paths) ~f:List.rev

  let check_cycles (typedecs : A.typedec list) : unit =
    let non_empty_paths =
      List.filter
        (paths_of_typedecs typedecs)
        ~f:(function [] -> false | _ -> true)
    in
    List.iter non_empty_paths ~f:(
      fun path ->
        match Dag.of_list (List.map path ~f:(fun (p, c, _) -> (p, c))) with
        | Ok _ ->
            ()
        | Error `Cycle ->
            let (_, from_id, from_pos) = List.hd           path  in
            let (_,   to_id,   to_pos) = List.hd (List.rev path) in
            E.raise (E.Cycle_in_type_decs {from_id; from_pos; to_id; to_pos})
    )

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
          | Value.Fun {formals; result; level=_; label=_} ->
              let expected = List.length formals in
              let given    = List.length args in
              if given = expected then
                begin
                  List.iter2 formals args ~f:(fun ty_expected exp_given ->
                    check_same
                      (return (actual_ty ~pos ty_expected))
                      (trexp exp_given)
                      ~pos;
                  );
                  return (actual_ty ~pos result)
                end
              else
                E.raise (E.Wrong_number_of_args {func; expected; given; pos})
          | Value.Var _ ->
              E.raise (E.Id_not_a_function {id=func; pos})
          )
      | A.OpExp {oper; left; right; pos} ->
          trop oper ~left ~right ~pos
      | A.RecordExp {fields=field_exps; typ; pos} ->
          let ty = env_get_typ_actual ~sym:typ ~env ~pos in
          Type.if_record
            ty
            ~f:(fun field_tys ->
              List.iter field_exps ~f:(fun (field, exp, pos) ->
                (match List.assoc_opt field field_tys with
                | Some field_ty ->
                    check_same (return (actual_ty ~pos field_ty)) (trexp exp) ~pos
                | None ->
                    E.raise
                      (E.No_such_field_in_record {field; record=ty; pos})
                )
              )
            )
            ~otherwise:(fun () ->
              E.raise (E.Wrong_type_used_as_record {ty_id=typ; ty; pos})
            );
          return (actual_ty ~pos ty)
      | A.SeqExp [] ->
          return_unit
      | A.SeqExp exps ->
          let last xs =
            xs
            |> List.rev  (* Yes, redundant, but clean-looking ;-P *)
            |> List.hd   (* Empty is matched in above SeqExp match case *)
          in
          exps
          |> List.map ~f:(fun (exp, _) -> trexp exp)
          |> last
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
          let (loop, env) = Env.loop_begin env in
          (* Only care if an error is raised *)
          ignore (transExp ~env body);
          ignore (Env.loop_end env loop);
          return_unit
      | A.ForExp {var; lo; hi; body; pos; escape=_} ->
          check_int (trexp lo) ~pos;
          check_int (trexp hi) ~pos;
          let (loop, env) = Env.loop_begin env in
          let level = Env.level_get env in
          (* Assuming all escape, for now *)
          let access = Translate.alloc_local ~level ~escapes:true in
          let env = Env.set_val env var (Value.Var {ty = Type.Int; access}) in
          (* Only care if an error is raised *)
          ignore (transExp ~env body);
          ignore (Env.loop_end env loop);
          return_unit
      | A.BreakExp pos ->
          (match Env.loop_current env with
          | Some _ -> ()
          | None   -> E.raise (E.Break_outside_loop pos)
          );
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
          let ty = env_get_typ_actual ~sym:typ ~env ~pos in
          Type.if_array
            ty
            ~f:(fun ty_elements ->
              check_same (return (actual_ty ~pos ty_elements)) (trexp init) ~pos
            )
            ~otherwise:(fun () ->
              E.raise (E.Wrong_type_used_as_array {ty_id=typ; ty; pos})
            );
          return (actual_ty ~pos ty)
      | A.VarExp var ->
          trvar var
      )
    and trvar =
      (function
      | A.SimpleVar {symbol=sym; pos} ->
          (match env_get_val ~sym ~env ~pos with
          | Value.Fun _ ->
              E.raise (E.Id_is_a_function {id=sym; pos})
          | Value.Var {ty; access=_} ->
              return (actual_ty ~pos ty)
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
                  return (actual_ty ~pos ty)
              )
            )
            ~otherwise:(fun () -> E.raise (E.Exp_not_a_record {ty; pos}))
      | A.SubscriptVar {var; exp; pos} ->
          let {exp=_; ty} = trvar var in
          check_int (trexp exp) ~pos;
          Type.if_array
            ty
            ~f:(fun ty_elements -> return (actual_ty ~pos ty_elements))
            ~otherwise:(fun () -> E.raise (E.Exp_not_an_array {ty; pos}))
      )
    and trop oper ~left ~right ~pos =
      (* TODO: Refactor trop - all opers return bool/int *)
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
            return_int  (* Because we have no bool type *)
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
            return_int  (* Because we have no bool type *)
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
  and transDec ~(env : Env.t) (dec : A.dec) : Env.t =
    (match dec with
    | A.VarDec {name; typ=typ_opt; init; pos=pos_outter; escape=_} ->
        let ty =
          (match (typ_opt, transExp ~env init) with
          | None, {ty; exp=_} ->
              ty
          | Some (sym, pos_inner), expty_init ->
              let ty = env_get_typ_actual ~sym ~env ~pos:pos_inner in
              check_same (return ty) expty_init ~pos:pos_outter;
              ty
          )
        in
        let access =
          Translate.alloc_local
            ~level:(Env.level_get env)
            ~escapes:true  (* Assuming all escape, for now... *)
        in
        Env.set_val env name (Value.Var {ty; access})
    | A.TypeDecs typedecs ->
        check_cycles typedecs;
        let env =
          List.fold_left typedecs ~init:env ~f:(
            fun env (A.TypeDec {name; ty=_; pos=_}) ->
              Env.set_typ env name (Type.Name (name, ref None))
          )
        in
        List.iter typedecs ~f:(fun (A.TypeDec {name=ty_name; ty=ty_exp; pos}) ->
          let ty = transTy ~env ~ty_name ~ty_exp in
          (match env_get_typ ~sym:ty_name ~env ~pos with
          | Type.Name (_, ty_opt_ref) ->
              ty_opt_ref := Some ty
          | Type.Unit
          | Type.Nil
          | Type.Int
          | Type.String
          | Type.Record _
          | Type.Array _ ->
              ()
          )
        );
        env
    | A.FunDecs fundecs ->
        let env_with_fun_heads_only =
          List.fold_left fundecs ~init:env ~f:(
            fun env (A.FunDec {name; params; result; body=_; pos=_}) ->
              let formals =
                List.map params ~f:(
                  fun (A.Field {name=_; typ; pos; escape=_}) ->
                    env_get_typ_actual ~env ~sym:typ ~pos
                )
              in
              let result =
                match result with
                | Some (s, p) -> env_get_typ_actual ~sym:s ~env ~pos:p
                | None        -> Type.Unit
              in
              let label = Temp.Label.gen () in
              let level =
                Translate.Level.next
                  (Env.level_get env)
                  ~name:label
                  (* Assuming all escape (for now) *)
                  ~formals:(List.map formals ~f:(fun _ -> true))
              in
              let env = Env.level_set env level in
              Env.set_val env name (Value.Fun {formals; result; level; label})
          )
        in
        List.iter fundecs ~f:(
          fun (A.FunDec {name=_; params; result=_; body; pos=_}) ->
            let env_with_fun_heads_and_local_vars =
              List.fold_left params ~init:env_with_fun_heads_only ~f:(
                fun env (A.Field {name=var_name; escape=_; typ; pos}) ->
                  let var_ty = env_get_typ_actual ~env ~sym:typ ~pos in
                  let level = Env.level_get env in
                  (* Assuming all escape, for now *)
                  let access = Translate.alloc_local ~level ~escapes:true in
                  Env.set_val
                    env
                    var_name
                    (Value.Var {ty = var_ty; access})
              )
            in
            (* we only care if an exception is raised *)
            ignore (transExp ~env:env_with_fun_heads_and_local_vars body);
        );
        env_with_fun_heads_only
    )
  and transTy ~(env : Env.t) ~ty_name ~(ty_exp : A.ty) : Type.t =
    (match ty_exp with
    | A.NameTy {symbol=sym; pos} ->
        env_get_typ ~sym ~env ~pos
    | A.RecordTy fields ->
        let fields =
          List.map fields ~f:(fun (A.Field {name; escape=_; typ; pos}) ->
            let ty = env_get_typ ~sym:typ ~env ~pos in
            (name, ty)
          )
        in
        Type.new_record ~name:ty_name ~fields
    | A.ArrayTy {symbol=sym; pos} ->
        let element_ty = env_get_typ ~sym ~env ~pos in
        Type.new_array ~name:ty_name ~ty:element_ty
    )
end

open Semant

let transProg absyn =
  Escape.find ~prog:absyn;
  let {exp = _; ty = _} = transExp absyn ~env:Env.base in
  ()
