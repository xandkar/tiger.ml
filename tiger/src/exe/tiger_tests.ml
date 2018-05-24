module List = ListLabels

let test_01 =
  let code =
    " \
    /* an array type and an array variable */ \
    let \
      type arrtype = array of int \
      var arr1:arrtype := \
        arrtype [10] of 0 \
    in \
      arr1 \
    end \
    "
  in
  let tokens =
    let open Tiger.Parser.Token in
    [ LET;
        TYPE; ID "arrtype"; EQ; ARRAY; OF; ID "int";
        VAR; ID "arr1"; COLON; ID "arrtype"; ASSIGN;
          ID "arrtype"; LBRACK; INT 10; RBRACK; OF; INT 0;
      IN;
        ID "arr1";
      END
    ]
  in
  ("test_01", code, tokens)

let test_02 =
  let code =
    " \
    /* arr1 is valid since expression 0 is int = myint */ \
    let \
      type myint = int \
      type arrtype = array of myint \
      var arr1:arrtype :=
        arrtype [10] of 0 \
    in \
      arr1 \
    end \
    "
  in
  let tokens =
    let open Tiger.Parser.Token in
    [ LET;
        TYPE; ID "myint"; EQ; ID "int";
        TYPE; ID "arrtype"; EQ; ARRAY; OF; ID "myint";
        VAR; ID "arr1"; COLON; ID "arrtype"; ASSIGN;
          ID "arrtype"; LBRACK; INT 10; RBRACK; OF; INT 0;
      IN;
        ID "arr1";
      END
    ]
  in
  ("test_02", code, tokens)

let test_03 =
  let code =
    " \
    /* a record type and a record variable */ \
    let \
      type rectype = \
        { name : string \
        , age  : int \
        } \
      var rec1 : rectype := \
        rectype \
        { name = \"Nobody\" \
        , age  = 1000 \
        } \
    in \
      rec1.name := \"Somebody\"; \
      rec1 \
    end \
    "
  in
  let tokens =
    let open Tiger.Parser.Token in
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
  in
  ("test_03", code, tokens)

let tokens_of_code code =
  let lexbuf = Lexing.from_string code in
  let rec tokens () =
    match Tiger.Lexer.token lexbuf with
    | Tiger.Parser.Token.EOF -> []
    | token -> token :: tokens ()
  in
  tokens ()

let tests =
  [ test_01
  ; test_02
  ; test_03
  ]

let () =
  let bar_sep = String.make 80 '-' in
  let bar_end = String.make 80 '=' in
  List.iter tests ~f:(fun (name, code, tokens_expected) ->
    let open Printf in
    printf "%s\n==> Test %S: " bar_sep name;
    let tokens_emitted = tokens_of_code code in
    (try
      assert (tokens_emitted = tokens_expected);
      print_endline "OK";
    with Assert_failure _ ->
      let tokens_to_string tokens =
        String.concat "; " (List.map Tiger.Parser.Token.to_string tokens)
      in
      printf
        "ERROR\n    Expected: %s\n    Emitted : %s\n\n"
        (tokens_to_string tokens_expected)
        (tokens_to_string tokens_emitted)
    );
  );
  print_endline bar_end;
