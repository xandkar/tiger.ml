module List = ListLabels

module Error = Tiger_error
module Test = Tiger_test

let read_file filepath =
  let {Unix.st_size=size; _} = Unix.stat filepath in
  let buf = Buffer.create size in
  let ic = open_in filepath in
  let rec read () =
    try
      Buffer.add_channel buf ic size;
      read ()
    with End_of_file ->
      ()
  in
  read ();
  close_in ic;
  Buffer.contents buf

let out_lexing_of_filename =
  let open Tiger_parser in
  function
  | "test01.tig" ->
      Some
        [ LET;
            TYPE; ID "arrtype"; EQ; ARRAY; OF; ID "int";
            VAR; ID "arr1"; COLON; ID "arrtype"; ASSIGN;
              ID "arrtype"; LBRACK; INT 10; RBRACK; OF; INT 0;
          IN;
            ID "arr1";
          END
        ]
  | "test02.tig" ->
      Some
        [ LET;
          TYPE; ID "myint"; EQ; ID "int";
          TYPE; ID "arrtype"; EQ; ARRAY; OF; ID "myint";
          VAR; ID "arr1"; COLON; ID "arrtype"; ASSIGN;
            ID "arrtype"; LBRACK; INT 10; RBRACK; OF; INT 0;
        IN;
          ID "arr1";
        END
      ]
  | "test03.tig" ->
      Some
        [ LET;
            TYPE; ID "rectype"; EQ;
              LBRACE; ID "name"; COLON; ID "string";
              COMMA;  ID "age";  COLON; ID "int";
              RBRACE;
            VAR; ID "rec1"; COLON; ID "rectype"; ASSIGN;
              ID "rectype";
              LBRACE; ID "name"; EQ; STRING "Nobody";
              COMMA;  ID "age";  EQ; INT 1000;
              RBRACE;
          IN;
            ID "rec1"; DOT; ID "name"; ASSIGN; STRING "Somebody"; SEMICOLON;
            ID "rec1";
          END
        ]
  | "test04.tig" ->
      Some
        [ LET;
            FUNCTION; ID "nfactor"; LPAREN; ID "n"; COLON; ID "int"; RPAREN; COLON; ID "int"; EQ;
              IF; ID "n"; EQ; INT 0;
              THEN; INT 1;
              ELSE; ID "n"; TIMES; ID "nfactor"; LPAREN; ID "n"; MINUS; INT 1; RPAREN;
          IN;
            ID "nfactor"; LPAREN; INT 10; RPAREN;
          END
        ]
  | "test09.tig" ->
      Some
        [IF; LPAREN; INT 5; GT; INT 4; RPAREN; THEN; INT 13; ELSE; STRING " "]
  | _ ->
    (* TODO: Fill-in other expected cases *)
    None

let out_parsing_of_filename _ =
  (* TODO: Fill-in expected cases *)
  None

let is_error_expected_parsing_of_filename =
  let module E = Tiger_error in
  function
  | "test49.tig" ->
      Some Error.is_invalid_syntax
      (* TODO: Be more specific - test position *)
  | _ ->
    (* TODO: Fill-in other expected cases *)
    None

(* TODO: test21.tig - error : procedure returns value  and procedure is used in arexpr *)

let is_error_expected_semant_of_filename =
  let module E = Tiger_error in
  function
  | "test17.tig"
  | "test33.tig" ->
      Some Error.is_unknown_type
      (* TODO: Be more specific - which type? *)
  | "test18.tig"
  | "test19.tig"
  | "test20.tig" ->
      Some Error.is_unknown_id
      (* TODO: Be more specific - the unknown id is "i" *)
  | "test22.tig" ->
      Some Error.is_no_such_field_in_record
      (* TODO: Be more specific - which field? *)
  | "test24.tig" ->
      Some Error.is_not_an_array
      (* TODO: Be more specific *)
  | "test25.tig" ->
      Some Error.is_not_a_record
      (* TODO: Be more specific *)
  | "test35.tig"
  | "test36.tig" ->
      Some Error.is_wrong_number_of_args
      (* TODO: Be more specific - how many expected, how many given? *)
  | "test09.tig"
  | "test11.tig"
  | "test13.tig"
  | "test14.tig"
  | "test21.tig"
  | "test23.tig"
  | "test26.tig"
  | "test28.tig"
  | "test29.tig"
  | "test31.tig"
  | "test32.tig"
  | "test34.tig"
  | "test43.tig" ->
      Some Error.is_wrong_type
      (* TODO: Be more specific - what expected, what given? Where? *)
  | _ ->
    (* TODO: Fill-in other expected cases *)
    None

let test_case_of_filename filename ~dir =
  Test.case
    filename
    ~code:(read_file (Filename.concat dir filename))
    ~out_lexing:(out_lexing_of_filename filename)
    ~out_parsing:(out_parsing_of_filename filename)
    ~is_error_expected_parsing:(is_error_expected_parsing_of_filename filename)
    ~is_error_expected_semant:(is_error_expected_semant_of_filename filename)

let is_filename_starts_with_dot filename =
  match filename.[0] with
  | exception Invalid_argument _ ->
      (* Filename should not be an empty string *)
      assert false
  | '.' -> true
  | _   -> false

let is_filename_not_hidden filename =
  not (is_filename_starts_with_dot filename)

let read ~from_dir:dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter ~f:is_filename_not_hidden
  |> List.filter ~f:(fun filename -> Filename.check_suffix filename ".tig")
  |> List.sort ~cmp:compare
  |> List.map ~f:(test_case_of_filename ~dir)
