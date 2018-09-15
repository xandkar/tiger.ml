module List = ListLabels

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

let is_error_expected_semant_of_filename _ =
  (* TODO: Fill-in expected cases *)
  None

let test_case_of_filename filename ~dir =
  Test.case
    filename
    ~code:(read_file (Filename.concat dir filename))
    ~out_lexing:(out_lexing_of_filename filename)
    ~out_parsing:(out_parsing_of_filename filename)
    ~is_error_expected_semant:(is_error_expected_semant_of_filename filename)

let read ~from_dir:dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort ~cmp:compare
  |> List.map ~f:(test_case_of_filename ~dir)
