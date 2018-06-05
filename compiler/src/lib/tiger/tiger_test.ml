(*
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

let case ?(out_lexing) ?(out_parsing) ~code name =
  { name
  ; code
  ; out_lexing
  ; out_parsing
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

let pass_parsing code =
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
  let run_pass f input output : string * string =
    match f input with
    | exception e ->
        incr error_count;
        ( s "%s: %s" (color Red "ERROR") (Printexc.to_string e)
        , "n/a"
        )
    | Error msg ->
        incr error_count;
        ( s "%s: %s" (color Red "ERROR") msg
        , "n/a"
        )
    | Ok produced ->
        let exe = s "%s" (color Green "OK") in
        let out =
          match
            Option.map output (fun expected -> expected = produced)
          with
          | None ->
              s "%s" (color Yellow "n/a")
          | Some true ->
              s "%s" (color Green "OK")
          | Some false ->
              incr error_count;
              s "%s" (color Red "ERROR")
        in
        (exe, out)
  in
  List.iter tests ~f:(
    fun {name; code; out_lexing; out_parsing} ->
      let ( lexing_exe,  lexing_out) = run_pass pass_lexing  code out_lexing in
      let (parsing_exe, parsing_out) = run_pass pass_parsing code out_parsing in
      p "%s" bar_sep; p_ln ();
      p "Test: %S" name; p_ln ();
        p_indent 1; p "Lexing:"; p_ln ();
          p_indent 2; p "exe: %s" lexing_exe; p_ln ();
          p_indent 2; p "out: %s" lexing_out; p_ln ();
        p_indent 1; p "Parsing:"; p_ln ();
          p_indent 2; p "exe: %s" parsing_exe; p_ln ();
          p_indent 2; p "out: %s" parsing_out; p_ln ();
  );
  p "%s" bar_end; p_ln ();
  let failures = !error_count in
  let clr = (if failures = 0 then Green else Red) in
  p "Failures: %s" (color clr (string_of_int failures)); p_ln ();
  p "%s" bar_end; p_ln ();
  exit failures
