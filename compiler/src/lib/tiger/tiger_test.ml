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
(* TODO: Perhaps a global option whether to print non-fail info? *)

open Printf

module List = ListLabels
module String = StringLabels

module Err = Tiger_error
module Opt = Tiger_opt

(* TODO: ~expect:Output of 'a | Exception of (exn -> bool) *)
type t =
  { name        : string
  ; code        : string
  ; out_lexing  : (Tiger_parser.token list) option
  ; out_parsing : Tiger_absyn.t option
  ; is_error_expected_parsing : (Tiger_error.t -> bool) option
  ; is_error_expected_semant : (Tiger_error.t -> bool) option
  }

type status =
  | Pass
  | Fail
  | Skip

type 'a t_result =
  { exe_stat : status
  ; exe_msg  : string
  ; out_stat : status
  ; out_val  : 'a option
  ; out_msg  : string
  }

type color =
  | Red
  | Red_bold
  | Green_bold
  | Grey_bold


let color_to_ansi_code = function
  | Grey_bold  -> "\027[1;30m"
  | Red        -> "\027[0;31m"
  | Red_bold   -> "\027[1;31m"
  | Green_bold -> "\027[1;32m"

let color_off = "\027[0m"

let color color string =
  let color_on  = color_to_ansi_code color in
  sprintf "%s%s%s" color_on string color_off

let color_opt str = function
  | Some c -> (color_to_ansi_code c) ^ str ^ color_off
  | None   -> str

let status_to_color = function
  | Pass -> Some Green_bold
  | Fail -> Some Red_bold
  | Skip -> Some Grey_bold

let status_to_str = function
  (* Expected to be a single character, but using string to allow unicode. *)
  | Pass -> "✓"
  | Fail -> "X"
  | Skip -> "-"

let case
    ?(out_lexing=None)
    ?(out_parsing=None)
    ?(is_error_expected_parsing=None)
    ?(is_error_expected_semant=None)
    ~code
    name
  =
  { name
  ; code
  ; out_lexing
  ; out_parsing
  ; is_error_expected_parsing
  ; is_error_expected_semant
  }

let bar_horiz_minor = color Grey_bold (String.make 80 '-')
let bar_horiz_major = color Grey_bold (String.make 80 '=')
let bar_vert        = color Grey_bold "|"

let lexbuf_set_filename lb filename
: unit
=
  let Lexing.({lex_start_p; lex_curr_p; _}) = lb in
  lb.Lexing.lex_start_p <- {lex_start_p with Lexing.pos_fname = filename};
  lb.Lexing.lex_curr_p  <- {lex_curr_p  with Lexing.pos_fname = filename}

let lexbuf_create ~filename ~code =
  let lb = Lexing.from_string code in
  lexbuf_set_filename lb filename;
  lb

let pass_lexing ~fake_filename ~code
: Tiger_parser.token list
=
  let lexbuf = lexbuf_create ~filename:fake_filename ~code in
  let rec tokens () =
    let token = Tiger_lexer.token lexbuf in
    (* Avoiding fragile pattern-matching *)
    if token = Tiger_parser.EOF then [] else token :: tokens ()
  in
  tokens ()

let pass_parsing ~fake_filename ~code
: Tiger_absyn.t
=
  Tiger_parser.program
    Tiger_lexer.token
    (lexbuf_create ~filename:fake_filename ~code)

let pass_semant (absyn : Tiger_absyn.t)
: unit
=
  Tiger_semant.transProg absyn

let str_exact str exact =
  let len = String.length str in
  let take = if len > exact then exact else len in
  let str = String.sub str ~pos:0 ~len:take in
  let pad = exact - take in
  let pad = String.make pad ' ' in
  str ^ pad

let exn_to_string = function
  | Tiger_error.T e -> Tiger_error.to_string e
  |               e ->    Printexc.to_string e

let s = sprintf
let p = printf
let p_ln = print_newline

let run tests =
  Printexc.record_backtrace true;
  let fail, fail_count =
    let count_fail_all = ref 0 in
    ( (fun () -> incr count_fail_all; Fail)
    , (fun () -> !count_fail_all)
    )
  in
  let run_pass ~f ~expect_output ~is_error_expected =
    let execution = match f () with exception e -> `Exn e | o -> `Out o in
    (match execution, is_error_expected with
    | `Exn (Err.T e), Some is_error_expected when is_error_expected e ->
        { exe_stat = Pass
        ; exe_msg  = ""
        ; out_stat = Skip
        ; out_val  = None
        ; out_msg  = ""
        }
    | `Exn e, Some _
    | `Exn e, None ->
        let b = Printexc.get_backtrace () in
        let e = exn_to_string e in
        { exe_stat = fail ()
        ; exe_msg  = s "\n\tException: %s.\n\tBacktrace: %s" e b
        ; out_stat = Skip
        ; out_val  = None
        ; out_msg  = ""
        }
    | `Out output, Some _ ->
        { exe_stat = fail ()
        ; exe_msg  = "Expected exception, but got output."
        ; out_stat = fail ()
        ; out_val  = Some output  (* TODO: Do we really want to keep going? *)
        ; out_msg  = "Expected exception, but got output."
        }
    | `Out output, None ->
        let (out_stat, out_msg) =
          match
            Opt.map expect_output (fun expected -> expected = output)
          with
          | None ->
              (Skip, "expected output not provided")
          | Some true ->
              (Pass, "")
          | Some false ->
              (* TODO pretty print expected and output *)
              (fail (), "unexpected output")
        in
        { exe_stat = Pass
        ; exe_msg  = ""  (* old "info" goes here *)
        ; out_stat
        ; out_val  = Some output
        ; out_msg
        }
    )
  in
  let test_case_count = ref 0 in
  let col_1_width = 30 in
  let p_stat width (exe, out) =
    (* All this gymnastics to ignore color codes in cell width *)
    let min = 5 in
    let width = if width > min then width else min in
    p "%s" (String.concat ~sep:"" (List.init ~len:width ~f:(function
      | 0 -> " "
      | 1 -> bar_vert
      | 2 -> " "
      | 3 -> color_opt (status_to_str exe) (status_to_color exe)
      | 4 -> color_opt (status_to_str out) (status_to_color out)
      | _ -> " "
    )))

  in
  p "%s" bar_horiz_major; p_ln ();
  p "%s" (str_exact "Test case" col_1_width);
  List.iter ~f:(fun header -> p " %s %s" bar_vert header)
    [ "Lexing"
    ; "Parsing"
    ; "Semant"
    ];
  p_ln ();
  p "%s" bar_horiz_major; p_ln ();
  List.iter tests ~f:(
    fun
      { name
      ; code
      ; out_lexing
      ; out_parsing
      ; is_error_expected_parsing
      ; is_error_expected_semant
      }
    ->
      incr test_case_count;
      let res_lex =
        run_pass
          ~f:(fun () -> pass_lexing ~fake_filename:name ~code)
          ~expect_output:out_lexing
          ~is_error_expected:None
      in
      let res_pars =
        run_pass
          ~f:(fun () -> pass_parsing ~fake_filename:name ~code)
          ~expect_output:out_parsing
          ~is_error_expected:is_error_expected_parsing
      in
      let res_sem =
        (* TODO: Replace this hack with general test-dependency checking *)
        match res_pars.out_val with
        | None ->
            { exe_stat = Skip
            ; exe_msg  = "No AST provided"
            ; out_stat = Skip
            ; out_val  = None
            ; out_msg  = ""
            }
        | Some absyn ->
            run_pass
              ~f:(fun () -> pass_semant absyn)
              ~expect_output:(Some ())
              ~is_error_expected:is_error_expected_semant
      in
      let results =
        (* Replacing out_val for type compatibility *)
        [ "Lexing"  , {res_lex  with out_val = None}
        ; "Parsing" , {res_pars with out_val = None}
        ; "Semant"  , {res_sem  with out_val = None}
        ]
      in
      if !test_case_count > 1 then (p "%s" bar_horiz_minor; p_ln ());
      p "%s" (str_exact name col_1_width);
      List.iter results ~f:(fun (stage, {exe_stat=e; out_stat=o; _}) ->
        p_stat ((String.length stage) + 3) (e, o)
      );
      p_ln ();
      let printed_error = ref false in
      List.iter results ~f:(
        fun (stage, {exe_stat; exe_msg; out_stat; out_msg; _}) ->
          (match exe_stat with
          | Pass -> ()
          | Skip -> ()
          | Fail ->
              printed_error := true;
              p "%s: %s" (color Grey_bold stage) (color Red exe_msg);
              p_ln ()
          );
          (match out_stat with
          | Pass -> ()
          | Skip -> ()
          | Fail ->
              printed_error := true;
              p "%s: %s" (color Grey_bold stage) (color Red out_msg)
          );
      );
  );
  p "%s" bar_horiz_major; p_ln ();
  p "%s %d failures in %d test cases"
      (match fail_count () with
      | 0 -> color_opt (status_to_str Pass) (status_to_color Pass)
      | _ -> color_opt (status_to_str Fail) (status_to_color Fail)
      )
      (fail_count ())
      !test_case_count;
    p_ln ();
  p "%s" bar_horiz_major; p_ln ();
  exit (fail_count ())
