module List = ListLabels

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

  val maxargs : stm -> int option
  (** Option because a program may not have any print statements at all. *)
end = struct
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
    (string_of_maxargs (Spl.maxargs spl_prog_noprint))
