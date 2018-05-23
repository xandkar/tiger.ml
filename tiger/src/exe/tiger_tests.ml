open Printf

module List = ListLabels

let test_01 =
  let code =
    "
    /* an array type and an array variable */
    let
        type  arrtype = array of int
        var arr1:arrtype := arrtype [10] of 0
    in
        arr1
    end
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
  (code, tokens)

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
  ]

let () =
  List.iter tests ~f:(fun (code, tokens_expected) ->
    assert ((tokens_of_code code) = tokens_expected)
  )
