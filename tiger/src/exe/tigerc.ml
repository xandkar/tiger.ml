open Printf

let () =
  let path_to_program_file = Sys.argv.(1) in
  let ic = open_in path_to_program_file in
  let lexbuf = Lexing.from_channel ic in
  let rec parse_and_print () =
    let token = Tiger.Lexer.token lexbuf in
    printf "%s\n" (Tiger.Parser.Token.to_string token);
    match token with
    | Tiger.Parser.Token.EOF -> ()
    | _ -> parse_and_print ()
  in
  parse_and_print ();
  close_in ic;
