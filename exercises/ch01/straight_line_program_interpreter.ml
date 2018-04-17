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

  val maxargs : stm -> int
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
      let max = ref 0 in
      let rec check_stm = function
        | PrintStm exps ->
            let exps_length = List.length exps in
            if exps_length > !max then max := exps_length else ();
            List.iter exps ~f:check_exp
        | AssignStm (_, e) ->
            check_exp e
        | CompoundStm (s1, s2) ->
            check_stm s1;
            check_stm s2
      and check_exp = function
        | IdExp _ | NumExp _ -> ()
        | OpExp (e1, _, e2) ->
            check_exp e1;
            check_exp e2
        | EseqExp (s, e) ->
            check_stm s;
            check_exp e
      in
      check_stm stm;
      !max
end

let spl_prog =
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

let () =
  Printf.printf "maxargs: %d\n" (Spl.maxargs spl_prog)
