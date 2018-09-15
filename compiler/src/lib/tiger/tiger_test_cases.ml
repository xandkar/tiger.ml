module Error = Tiger_error
module Test = Tiger_test

let micro =
  let open Tiger_parser in
  [ (let code = "nil"    in Test.case code ~code ~out_lexing:(Some [NIL]))
  ; (let code = "5"      in Test.case code ~code ~out_lexing:(Some [INT 5]))
  ; (let code = "-5"     in Test.case code ~code ~out_lexing:(Some [MINUS; INT 5]))
  ; ( let code = "f()" in
      Test.case
        code
        ~code
        ~out_lexing:(Some [ID "f"; LPAREN; RPAREN])
        (* TODO: Be more specific *)
        ~is_error_expected_semant:(Some Error.is_unknown_id)
    )
  ; ( let code = "abc.i" in
      Test.case
        code
        ~code
        ~out_lexing:(Some [ID "abc"; DOT; ID "i"])
        (* TODO: Be more specific *)
        ~is_error_expected_semant:(Some Error.is_unknown_id)
    )
  ; ( let code = "abc[0]" in
      Test.case
        code
        ~code
        ~out_lexing:(Some [ID "abc"; LBRACK; INT 0; RBRACK])
        (* TODO: Be more specific *)
        ~is_error_expected_semant:(Some Error.is_unknown_id)
    )
  ; ( let code = "abc[0] := foo()" in
      Test.case
        code
        ~code
        ~out_lexing:
          (Some [ID "abc"; LBRACK; INT 0; RBRACK; ASSIGN; ID "foo"; LPAREN; RPAREN])
        (* TODO: Be more specific *)
        ~is_error_expected_semant:(Some Error.is_unknown_id)
    )
  ; ( let code = "abc [5] of nil" in
      Test.case
        code
        ~code
        ~out_lexing:(Some [ID "abc"; LBRACK; INT 5; RBRACK; OF; NIL])
        (* TODO: Be more specific *)
        ~is_error_expected_semant:(Some Error.is_unknown_type)
    )
  ; ( let code = "f(\"a\", 3, foo)" in
      Test.case
        code
        ~code
        ~out_lexing:
          (Some [ID "f"; LPAREN; STRING "a"; COMMA; INT 3; COMMA; ID "foo"; RPAREN])
        ~is_error_expected_semant:(Some Error.is_unknown_id)
    )
  ; ( let code =
        "let \
            type a = int \
            type b = a \
            type c = b \
            var i : a := 2 \
            var j : c := 3 \
        in \
            i := j \
        end \
        "
      in
      Test.case
        "Type aliases"
        ~code
    )
  ; ( let code =
        "let \
            type a = {x:int, y:int} \
            type b = {x:int, y:int}  /* new type generated */ \
            var foo : a := a {x = 1, y = 2} \
            var bar : b := b {x = 1, y = 2} \
        in \
            foo = bar  /* incompatible types */ \
        end \
        "
      in
      Test.case
        "Incompatible records"
        ~code
        (* TODO: Be more specific *)
        ~is_error_expected_semant:(Some Error.is_wrong_type)
    )
  ]

let book ~dir =
  Tiger_test_cases_book.read ~from_dir:dir

let all ~dir =
  (book ~dir) @ micro
