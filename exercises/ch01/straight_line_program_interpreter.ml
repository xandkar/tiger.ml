module List = ListLabels
module String = StringLabels

module Spl : sig
  type id = string

  type binop =
    | Plus
    | Minus
    | Times
    | Div

  type stm =
    | CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list
  and exp =
    | IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp

  exception Unknown_identifier of string

  val maxargs : stm -> int option
  (** Option because a program may not have any print statements at all. *)

  val interp : stm -> unit
  (** raises Unknown_identifier, if such is encountered *)
end = struct
  module Table : sig
    type ('k, 'v) t
    val empty : ('k, 'v) t
    val set : ('k, 'v) t -> k:'k -> v:'v -> ('k, 'v) t
    val get : ('k, 'v) t -> k:'k -> 'v option
  end = struct
    type ('k, 'v) t = ('k * 'v) list
    let empty = []
    let set t ~k ~v = (k, v) :: t
    let get t ~k =
      let rec search = function
        | [] -> None
        | (key, v) :: _ when key = k -> Some v
        | (_, _) :: rest -> search rest
      in
      search t
  end

  type id = string

  type binop =
    | Plus
    | Minus
    | Times
    | Div

  type stm =
    | CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list
  and exp =
    | IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp

  exception Unknown_identifier of string

  let interp_binop op v1 v2 =
    match op with
    | Plus  -> v1 + v2
    | Minus -> v1 - v2
    | Times -> v1 * v2
    | Div   -> v1 / v2

  let rec interp_stm tbl_0 stm =
    begin match stm with
    | PrintStm exps ->
        let (tbl_1, val_ints) =
          List.fold_right exps
            ~init:(tbl_0, [])
            ~f:(fun e (tbl0, vs) ->
                let (tbl1, v) = interp_exp tbl0 e in
                (tbl1, v :: vs)
            )
        in
        let val_strings = List.map val_ints ~f:string_of_int in
        print_endline (String.concat val_strings ~sep:" ");
        tbl_1
    | AssignStm (id, e) ->
        let (tbl_1, v) = interp_exp tbl_0 e in
        Table.set tbl_1 ~k:id ~v
    | CompoundStm (s1, s2) ->
        let tbl_1 = interp_stm tbl_0 s1 in
        interp_stm tbl_1 s2
    end
  and interp_exp tbl_0 exp =
    ( match exp with
    | IdExp id ->
        ( match Table.get tbl_0 ~k:id with
        | Some v -> (tbl_0, v)
        | None   -> raise (Unknown_identifier id)
        )
    | NumExp n -> (tbl_0, n)
    | OpExp (e1, op, e2) ->
        let (tbl_1, v1) = interp_exp tbl_0 e1 in
        let (tbl_2, v2) = interp_exp tbl_1 e2 in
        (tbl_2, interp_binop op v1 v2)
    | EseqExp (s, e) ->
        let tbl_1 = interp_stm tbl_0 s in
        interp_exp tbl_1 e
    )

  let interp stm : unit =
    ignore (interp_stm (Table.empty) stm)

    (* 01.p.1: Write ML function (maxargs : stm -> int) that tells the
     * maximum number of arguments of any print statement within any
     * subexpression of a given statement. For example, maxargs(prog)
     * is 2.
     *)
    let maxargs stm =
      let opt_max_update opt n =
        match opt with
        | None   -> Some n
        | Some m -> Some (max m n)
      in
      let opt_max_merge a b =
        match a, b with
        | None  , None   -> None
        | None  , b      -> b
        | Some _, None   -> a
        | Some _, Some n -> opt_max_update a n
      in
      let rec check_stm max_opt stm =
        match stm with
        | PrintStm exps ->
            List.fold_left exps
              ~init:(opt_max_update max_opt (List.length exps))
              ~f:check_exp
        | AssignStm (_, e) ->
            check_exp max_opt e
        | CompoundStm (s1, s2) ->
            opt_max_merge (check_stm max_opt s1) (check_stm max_opt s2)
      and check_exp max_opt exp =
        match exp with
        | IdExp _ | NumExp _ -> max_opt
        | OpExp (e1, _, e2) ->
            opt_max_merge (check_exp max_opt e1) (check_exp max_opt e2)
        | EseqExp (s, e) ->
            opt_max_merge (check_stm max_opt s) (check_exp max_opt e)
      in
      check_stm None stm
end

let spl_prog_orig =
  (*  a := 5 + 3;
   *  b := (print(a, a - 1), 10 * a);
   *  print(b)
   *)
  Spl.CompoundStm
    ( Spl.AssignStm ("a", Spl.OpExp (Spl.NumExp 5, Spl.Plus, Spl.NumExp 3))
    , Spl.CompoundStm
        ( Spl.AssignStm
          ( "b"
          , Spl.EseqExp
              ( Spl.PrintStm
                  [ Spl.IdExp "a"
                  ; Spl.OpExp (Spl.IdExp "a", Spl.Minus, Spl.NumExp 1)
                  ]
              , Spl.OpExp (Spl.NumExp 10, Spl.Times, Spl.IdExp "a")
              )
          )
        , Spl.PrintStm [Spl.IdExp "b"]
        )
    )

let spl_prog_noprint =
  (*  a := 5 + 3;
   *  b := 10 * a
   *)
  Spl.CompoundStm
    ( Spl.AssignStm
        ("a", Spl.OpExp (Spl.NumExp 5, Spl.Plus, Spl.NumExp 3))
    , Spl.AssignStm
        ("b", Spl.OpExp (Spl.NumExp 10, Spl.Times, Spl.IdExp "a"))
    )

let () =
  let string_of_maxargs int_opt =
    match int_opt with
    | Some n -> string_of_int n
    | None   -> "N/A"
  in
  Printf.printf "maxargs : spl_prog_orig -> %s\n"
    (string_of_maxargs (Spl.maxargs spl_prog_orig));
  Printf.printf "maxargs : spl_prog_noprint -> %s\n"
    (string_of_maxargs (Spl.maxargs spl_prog_noprint));
  print_endline "BEGIN Spl.interp spl_prog_orig";
  Spl.interp spl_prog_orig;
  print_endline "END Spl.interp spl_prog_orig"
