open Printf

module List = ListLabels

let test_case_from_book_01 =
  let name = "an array type and an array variable" in
  let code =
    " \
    /* "^name^" */ \
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
    let open Tiger.Parser in
    [ LET;
        TYPE; ID "arrtype"; EQ; ARRAY; OF; ID "int";
        VAR; ID "arr1"; COLON; ID "arrtype"; ASSIGN;
          ID "arrtype"; LBRACK; INT 10; RBRACK; OF; INT 0;
      IN;
        ID "arr1";
      END
    ]
  in
  (name, code, tokens)

let test_case_from_book_02 =
  let name = "arr1 is valid since expression 0 is int = myint" in
  let code =
    " \
    /* "^name^" */ \
    let \
      type myint = int \
      type arrtype = array of myint \
      var arr1:arrtype := \
        arrtype [10] of 0 \
    in \
      arr1 \
    end \
    "
  in
  let tokens =
    let open Tiger.Parser in
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
  (name, code, tokens)

let test_case_from_book_03 =
  let name = "a record type and a record variable" in
  let code =
    " \
    /* "^name^" */ \
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
    let open Tiger.Parser in
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
  (name, code, tokens)

let test_case_from_book_04 =
  let name = "define a recursive function" in
  let code =
    " \
    /* "^name^" */ \
    let \
    \
      /* calculate n! */ \
      function nfactor(n: int): int = \
        if n = 0  \
        then 1 \
        else n * nfactor(n-1) \
    \
    in \
      nfactor(10) \
    end \
    "
  in
  let tokens =
    let open Tiger.Parser in
    [ LET;
        FUNCTION; ID "nfactor"; LPAREN; ID "n"; COLON; ID "int"; RPAREN; COLON; ID "int"; EQ;
          IF; ID "n"; EQ; INT 0;
          THEN; INT 1;
          ELSE; ID "n"; TIMES; ID "nfactor"; LPAREN; ID "n"; MINUS; INT 1; RPAREN;
      IN;
        ID "nfactor"; LPAREN; INT 10; RPAREN;
      END
    ]
  in
  (name, code, tokens)

let test_case_from_book_09 =
  let name = "error : types of then - else differ" in
  let code =
    " \
    /* "^name^" */ \
    if (5>4) then 13 else  \" \" \
    "
  in
  let tokens =
    let open Tiger.Parser in
    [ IF; LPAREN; INT 5; GT; INT 4; RPAREN; THEN; INT 13; ELSE; STRING " "
    ]
  in
  (* TODO: Type error test case *)
  (name, code, tokens)

(*
let test_case_from_book_queens =
  let code =
    "\
    /* A program to solve the 8-queens problem */ \n\
 \n\
    let \n\
      var N := 8 \n\
 \n\
      type intArray = array of int \n\
 \n\
      var row := intArray [ N ] of 0 \n\
      var col := intArray [ N ] of 0 \n\
      var diag1 := intArray [N+N-1] of 0 \n\
      var diag2 := intArray [N+N-1] of 0 \n\
 \n\
      function printboard() = ( \n\
        for i := 0 to N-1 do ( \n\
          for j := 0 to N-1 do print(if col[i]=j then \" O\" else \" .\"); \n\
          print(\"\n\") \n\
        ); \n\
        print(\"\n\") \n\
      ) \n\
 \n\
      function try(c:int) = ( \n\
        /*  for i:= 0 to c do print(\".\"); print(\"\n\"); flush();*/ \n\
        if c=N \n\
        then printboard() \n\
        else \n\
          for r := 0 to N-1 \n\
          do \n\
            if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0 \n\
            then ( \n\
              row[r]       := 1; \n\
              diag1[r+c]   := 1; \n\
              diag2[r+7-c] := 1; \n\
              col[c]       := r; \n\
              try(c+1); \n\
              row[r]       := 0; \n\
              diag1[r+c]   := 0; \n\
              diag2[r+7-c] := 0 \n\
            ) \n\
      ) \n\
    in \n\
      try(0) \n\
    end \n\
    "
  in
  (code, code, [])
*)

let test_cases_from_book =
  [ test_case_from_book_01
  ; test_case_from_book_02
  ; test_case_from_book_03
  ; test_case_from_book_04
  ; test_case_from_book_09
  (*; test_case_from_book_queens*)
  ]

let tests_micro_cases =
  let open Tiger.Parser in
  [ (
      let code =
        "nil"
      in
      let tokens =
        [NIL]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "5"
      in
      let tokens =
        [INT 5]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "-5"
      in
      let tokens =
        [MINUS; INT 5]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "f()"
      in
      let tokens =
        [ID "f"; LPAREN; RPAREN]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "f(\"a\", 3, foo)"
      in
      let tokens =
        [ID "f"; LPAREN; STRING "a"; COMMA; INT 3; COMMA; ID "foo"; RPAREN]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "abc.i"
      in
      let tokens =
        [ID "abc"; DOT; ID "i"]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "abc [5] of nil"
      in
      let tokens =
        [ID "abc"; LBRACK; INT 5; RBRACK; OF; NIL]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "abc[0]"
      in
      let tokens =
        [ID "abc"; LBRACK; INT 0; RBRACK]
      in
      (code, code, tokens)
    )
  ; (
      let code =
        "abc[0] := foo()"
      in
      let tokens =
        [ID "abc"; LBRACK; INT 0; RBRACK; ASSIGN; ID "foo"; LPAREN; RPAREN]
      in
      (code, code, tokens)
    )
  ]

let tests =
  test_cases_from_book @ tests_micro_cases

let () =
  let tokens_of_code code =
    let lexbuf = Lexing.from_string code in
    let rec tokens () =
      let token = Tiger.Lexer.token lexbuf in
      (* Avoiding fragile pattern-matching *)
      if token = Tiger.Parser.EOF then [] else token :: tokens ()
    in
    tokens ()
  in
  let parsetree_of_code code =
    let lb = Lexing.from_string code in
    (match Tiger.Parser.program Tiger.Lexer.token lb with
    | exception Parsing.Parse_error ->
        let module L = Lexing in
        let L.({lex_curr_p = {pos_lnum=l; pos_bol=b; pos_cnum=c; _}; _}) = lb in
        let msg = sprintf "Syntax error around line: %d, column: %d" l (c - b) in
        Error msg
    | absyn ->
        Ok (Tiger.Absyn.to_string absyn)
    )
  in
  let bar_sep = String.make 80 '-' in
  let bar_end = String.make 80 '=' in
  let indent n = String.make (2 * n) ' ' in
  let color_on_green = "\027[0;32m" in
  let color_on_red   = "\027[1;31m" in
  let color_off      = "\027[0m" in
  List.iteri tests ~f:(fun i (name, code, tokens_expected) ->
    let i = i + 1 in  (* Because iteri starts with 0 *)
    printf "%s\n%sTest %d : %S\n" bar_sep (indent 0) i name;

    printf "%sLexing : " (indent 1);
    let tokens_emitted = tokens_of_code code in
    (try
      assert (tokens_emitted = tokens_expected);
      printf "%sOK%s\n" color_on_green color_off;
    with Assert_failure _ ->
      let tokens_to_string tokens =
        String.concat "; " (List.map ~f:Tiger.Parser_token.to_string tokens)
      in
      printf
        "%sERROR%s\n%sExpected: %s\n%sEmitted : %s\n\n"
        color_on_red
        color_off
        (indent 2)
        (tokens_to_string tokens_expected)
        (indent 2)
        (tokens_to_string tokens_emitted)
    );

    printf "%sParsing: " (indent 1);
    (match parsetree_of_code code with
    | Error errmsg -> printf "%sERROR:%s %s\n"   color_on_red   color_off errmsg
    | Ok parsetree -> printf "%sOK:%s\n\n%s\n\n" color_on_green color_off parsetree
    );

  );
  print_endline bar_end;
