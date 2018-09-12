(* "exe" is for status of execution (whether any exceptions were raised)
 * "out" is for status of output comparison (whether what was outputted is
 *       what was expected)
 *
 * code |> pass_a_exe |> pass_a_out |> ... |> pass_z_exe |> pass_z_out
 *
 * pass a:
 *  exe: OK
 *  out: n/a
 * pass b:
 *  exe: OK
 *  out: OK
 * pass c:
 *  exe: OK
 *  out: ERROR
 * ...
 *
 * name     | pass a | ... | pass z
 * ---------+--------+-----+--------
 * exe foo  | OK     | ... | OK
 * out foo  | OK     | ... | ERROR
 *
 * *)

open Printf

module List = ListLabels
module String = StringLabels

module Option : sig
  type 'a t = 'a option

  val map : 'a t -> ('a -> 'b) -> 'b t
end = struct
  type 'a t = 'a option

  let map t f =
    match t with
    | None   -> None
    | Some x -> Some (f x)
end

type t =
  { name        : string
  ; code        : string
  ; out_lexing  : (Tiger_parser.token list) option
  ; out_parsing : Tiger_absyn.t option
  ; is_error_expected : (Tiger_error.t -> bool)
  }

type color =
  | Red
  | Yellow
  | Green


let color_to_ansi_code = function
  | Red    -> "\027[0;31m"
  | Yellow -> "\027[0;33m"
  | Green  -> "\027[0;32m"

let color color string =
  let color_on  = color_to_ansi_code color in
  let color_off = "\027[0m" in
  sprintf "%s%s%s" color_on string color_off

let case
    ?(out_lexing)
    ?(out_parsing)
    ?(is_error_expected=(fun _ -> false))
    ~code
    name
  =
  { name
  ; code
  ; out_lexing
  ; out_parsing
  ; is_error_expected
  }

let bar_sep = String.make 80 '-'
let bar_end = String.make 80 '='

let indent =
  let unit_spaces = 2 in
  fun n ->
    String.make (n * unit_spaces) ' '

let pass_lexing code : (Tiger_parser.token list, string) result =
  let lexbuf = Lexing.from_string code in
  let rec tokens () =
    let token = Tiger_lexer.token lexbuf in
    (* Avoiding fragile pattern-matching *)
    if token = Tiger_parser.EOF then [] else token :: tokens ()
  in
  match tokens () with
  | exception e -> Error (Printexc.to_string e)
  | tokens      -> Ok tokens

let pass_parsing code : (Tiger_absyn.t, string) result =
  let lb = Lexing.from_string code in
  match Tiger_parser.program Tiger_lexer.token lb with
  | exception Parsing.Parse_error ->
      let module L = Lexing in
      let L.({lex_curr_p = {pos_lnum=l; pos_bol=b; pos_cnum=c; _}; _}) = lb in
      let msg = sprintf "Syntax error around line: %d, column: %d" l (c - b) in
      Error msg
  | ast ->
      Ok ast

let s = sprintf
let p = printf
let p_ln = print_newline
let p_indent n = p "%s" (indent n)

let run tests =
  let error_count = ref 0 in
  let run_pass ~f ~input ~expect_output ~is_error_expected =
    let output_status = "n/a" in
    let output_value  = None in
    match f input with
    | exception e ->
        let c, e =
          (match e with
          | Tiger_error.T e when is_error_expected e ->
              (Green, Tiger_error.to_string e)
          | Tiger_error.T e ->
              incr error_count;
              (Red, Tiger_error.to_string e)
          | e ->
              incr error_count;
              (Red, Printexc.to_string e)
          )
        in
        ( s "%s: %s" (color c "ERROR") e
        , output_status
        , output_value
        )
    | Error msg ->
        incr error_count;
        ( s "%s: %s" (color Red "ERROR") msg
        , output_status
        , output_value
        )
    | Ok produced ->
        let execution_status = s "%s" (color Green "OK") in
        let output_status =
          match
            Option.map expect_output (fun expected -> expected = produced)
          with
          | None ->
              s "%s" (color Yellow "expected output not provided")
          | Some true ->
              s "%s" (color Green "OK")
          | Some false ->
              incr error_count;
              s "%s" (color Red "ERROR")
        in
        let output_value = Some produced in
        (execution_status, output_status, output_value)
  in
  List.iter tests ~f:(
    fun {name; code; out_lexing; out_parsing; is_error_expected} ->
      let (stat_lex_exe, stat_lex_out_cmp, _) =
        run_pass
          ~f:pass_lexing
          ~input:code
          ~expect_output:out_lexing
          ~is_error_expected
      in
      let (stat_pars_exe, stat_pars_out_cmp, _) =
        run_pass
          ~f:pass_parsing
          ~input:code
          ~expect_output:out_parsing
          ~is_error_expected
      in
      p "%s" bar_sep; p_ln ();
      p "Test: %S" name; p_ln ();
        p_indent 1; p "Lexing:"; p_ln ();
          p_indent 2; p "exe: %s" stat_lex_exe    ; p_ln ();
          p_indent 2; p "out: %s" stat_lex_out_cmp; p_ln ();
        p_indent 1; p "Parsing:"; p_ln ();
          p_indent 2; p "exe: %s" stat_pars_exe    ; p_ln ();
          p_indent 2; p "out: %s" stat_pars_out_cmp; p_ln ();
  );
  p "%s" bar_end; p_ln ();
  let failures = !error_count in
  let clr = (if failures = 0 then Green else Red) in
  p "Failures: %s" (color clr (string_of_int failures)); p_ln ();
  p "%s" bar_end; p_ln ();
  exit failures
