module Error = Tiger_error
module Test = Tiger_test

let book =
  [ Test.case
      "Book test 1: an array type and an array variable"
      ~code:
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
      ~out_lexing:(
        let open Tiger_parser in
        [ LET;
            TYPE; ID "arrtype"; EQ; ARRAY; OF; ID "int";
            VAR; ID "arr1"; COLON; ID "arrtype"; ASSIGN;
              ID "arrtype"; LBRACK; INT 10; RBRACK; OF; INT 0;
          IN;
            ID "arr1";
          END
        ]
      )
  ; Test.case
      "Book test 2: arr1 is valid since expression 0 is int = myint"
      ~code:
        " \
        /* arr1 is valid since expression 0 is int = myint */ \
        let \
          type myint = int \
          type arrtype = array of myint \
          var arr1:arrtype := \
            arrtype [10] of 0 \
        in \
          arr1 \
        end \
        "
    ~out_lexing:(
      let open Tiger_parser in
      [ LET;
          TYPE; ID "myint"; EQ; ID "int";
          TYPE; ID "arrtype"; EQ; ARRAY; OF; ID "myint";
          VAR; ID "arr1"; COLON; ID "arrtype"; ASSIGN;
            ID "arrtype"; LBRACK; INT 10; RBRACK; OF; INT 0;
        IN;
          ID "arr1";
        END
      ]
    )
  ; Test.case
      "Book test 3: a record type and a record variable"
      ~code:
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
      ~out_lexing:(
        let open Tiger_parser in
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
      )
  ; Test.case
      "Book test 4: define a recursive function"
      ~code:
        " \
        /* define a recursive function */ \
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
      ~out_lexing:(
        let open Tiger_parser in
        [ LET;
            FUNCTION; ID "nfactor"; LPAREN; ID "n"; COLON; ID "int"; RPAREN; COLON; ID "int"; EQ;
              IF; ID "n"; EQ; INT 0;
              THEN; INT 1;
              ELSE; ID "n"; TIMES; ID "nfactor"; LPAREN; ID "n"; MINUS; INT 1; RPAREN;
          IN;
            ID "nfactor"; LPAREN; INT 10; RPAREN;
          END
        ]
      )
  ; Test.case
      "Book test 9: error : types of then - else differ"
      ~code:
        " \
        /* error : types of then - else differ */ \
        if (5>4) then 13 else  \" \" \
        "
      ~out_lexing:(
        let open Tiger_parser in
        [ IF; LPAREN; INT 5; GT; INT 4; RPAREN; THEN; INT 13; ELSE; STRING " "
        ]
      )
      ~is_error_expected_semant:Error.is_wrong_type (* TODO: Be more specific *)
  ; Test.case
    "Book test: 8-queens"
    ~code:
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
  ]

let micro =
  let open Tiger_parser in
  [ (let code = "nil"    in Test.case code ~code ~out_lexing:[NIL])
  ; (let code = "5"      in Test.case code ~code ~out_lexing:[INT 5])
  ; (let code = "-5"     in Test.case code ~code ~out_lexing:[MINUS; INT 5])
  ; ( let code = "f()" in
      Test.case
        code
        ~code
        ~out_lexing:[ID "f"; LPAREN; RPAREN]
        ~is_error_expected_semant:Error.is_unknown_id (* TODO: Be more specific *)
    )
  ; ( let code = "abc.i" in
      Test.case
        code
        ~code
        ~out_lexing:[ID "abc"; DOT; ID "i"]
        ~is_error_expected_semant:Error.is_unknown_id (* TODO: Be more specific *)
    )
  ; ( let code = "abc[0]" in
      Test.case
        code
        ~code
        ~out_lexing:[ID "abc"; LBRACK; INT 0; RBRACK]
        ~is_error_expected_semant:Error.is_unknown_id (* TODO: Be more specific *)
    )
  ; ( let code = "abc[0] := foo()" in
      Test.case
        code
        ~code
        ~out_lexing:
          [ID "abc"; LBRACK; INT 0; RBRACK; ASSIGN; ID "foo"; LPAREN; RPAREN]
        ~is_error_expected_semant:Error.is_unknown_id (* TODO: Be more specific *)
    )
  ; ( let code = "abc [5] of nil" in
      Test.case
        code
        ~code
        ~out_lexing:[ID "abc"; LBRACK; INT 5; RBRACK; OF; NIL]
        ~is_error_expected_semant:Error.is_unknown_type (* TODO: Be more specific *)
    )
  ; ( let code = "f(\"a\", 3, foo)" in
      Test.case
        code
        ~code
        ~out_lexing:
          [ID "f"; LPAREN; STRING "a"; COMMA; INT 3; COMMA; ID "foo"; RPAREN]
        ~is_error_expected_semant:Error.is_unknown_id
    )
  ; ( let code =
        "let
            type a = int
            type b = a
            type c = b
            var i : a := 2
            var j : c := 3
        in
            i := j
        end
        "
      in
      Test.case
        "Type aliases"
        ~code
    )
  ; ( let code =
        "let
            type a = {x:int, y:int}
            type b = {x:int, y:int}  /* new type generated */
            var foo : a := a {x = 1, y = 2}
            var bar : b := b {x = 1, y = 2}
        in
            foo = bar  /* incompatible types */
        end
        "
      in
      Test.case
        code
        ~code
				~is_error_expected_semant:Error.is_wrong_type (* TODO: Be more specific *)
    )
  ]

let all =
  book @ micro 
