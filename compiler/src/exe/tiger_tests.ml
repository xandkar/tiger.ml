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

module Test : sig
  type t

  val case
    :  ?out_lexing  : Tiger.Parser.token list
    -> ?out_parsing : Tiger.Absyn.t
    -> code         : string
    -> string
    -> t

  val run : t list -> unit
end = struct
  type t =
    { name        : string
    ; code        : string
    ; out_lexing  : (Tiger.Parser.token list) option
    ; out_parsing : Tiger.Absyn.t option
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

  let pass_lexing code : (Tiger.Parser.token list, string) result =
    let lexbuf = Lexing.from_string code in
    let rec tokens () =
      let token = Tiger.Lexer.token lexbuf in
      (* Avoiding fragile pattern-matching *)
      if token = Tiger.Parser.EOF then [] else token :: tokens ()
    in
    match tokens () with
    | exception e -> Error (Printexc.to_string e)
    | tokens      -> Ok tokens

  let pass_parsing code =
    let lb = Lexing.from_string code in
    match Tiger.Parser.program Tiger.Lexer.token lb with
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
    let error_count = !error_count in
    let clr = (if error_count = 0 then Green else Red) in
    p "Errors: %s" (color clr (string_of_int error_count)); p_ln ();
    p "%s" bar_end; p_ln ();
    exit error_count
end

let test_cases_from_book =
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
        let open Tiger.Parser in
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
      )
  ; Test.case
      "Book test 9: error : types of then - else differ"
      ~code:
        " \
        /* error : types of then - else differ */ \
        if (5>4) then 13 else  \" \" \
        "
      ~out_lexing:(
        let open Tiger.Parser in
        [ IF; LPAREN; INT 5; GT; INT 4; RPAREN; THEN; INT 13; ELSE; STRING " "
        ]
      )
  ]

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

let tests_micro_cases =
  let open Tiger.Parser in
  [ (let code = "nil"    in Test.case code ~code ~out_lexing:[NIL])
  ; (let code = "5"      in Test.case code ~code ~out_lexing:[INT 5])
  ; (let code = "-5"     in Test.case code ~code ~out_lexing:[MINUS; INT 5])
  ; (let code = "f()"    in Test.case code ~code ~out_lexing:[ID "f"; LPAREN; RPAREN])
  ; (let code = "abc.i"  in Test.case code ~code ~out_lexing:[ID "abc"; DOT; ID "i"])
  ; (let code = "abc[0]" in Test.case code ~code ~out_lexing:[ID "abc"; LBRACK; INT 0; RBRACK])

  ; (let code = "abc[0] := foo()" in Test.case code ~code
      ~out_lexing:
        [ID "abc"; LBRACK; INT 0; RBRACK; ASSIGN; ID "foo"; LPAREN; RPAREN])

  ; (let code = "abc [5] of nil" in Test.case code ~code
      ~out_lexing:
        [ID "abc"; LBRACK; INT 5; RBRACK; OF; NIL])

  ; (let code = "f(\"a\", 3, foo)" in Test.case code ~code
      ~out_lexing:
        [ID "f"; LPAREN; STRING "a"; COMMA; INT 3; COMMA; ID "foo"; RPAREN])
  ]

let tests =
  test_cases_from_book  @ tests_micro_cases 

let () =
  Test.run tests
