let () =
  let path_to_program_file = Sys.argv.(1) in
  let ic = open_in path_to_program_file in
  let lexbuf = Lexing.from_channel ic in
  (match Tiger.Parser.program Tiger.Lexer.token lexbuf with
  | exception Parsing.Parse_error ->
      let
        Lexing.({lex_curr_p = {pos_lnum; pos_bol; pos_cnum; _}; _}) = lexbuf
      in
      Printf.printf
        "Syntax error in file %S, around line: %d, column: %d\n"
        path_to_program_file pos_lnum (pos_cnum - pos_bol)
  | program ->
      print_endline program
  );
  close_in ic;
