let lexbuf_set_filename lb filename : unit =
  let Lexing.({lex_start_p; lex_curr_p; _}) = lb in
  lb.Lexing.lex_start_p <- {lex_start_p with Lexing.pos_fname = filename};
  lb.Lexing.lex_curr_p  <- {lex_curr_p  with Lexing.pos_fname = filename}

let () =
  let path_to_program_file = Sys.argv.(1) in
  let ic = open_in path_to_program_file in
  let lexbuf = Lexing.from_channel ic in
  lexbuf_set_filename lexbuf path_to_program_file;
  (match Tiger.Parser.program Tiger.Lexer.token lexbuf with
  | exception Tiger.Error.T error ->
      Printf.eprintf "%s\n" (Tiger.Error.to_string error);
      exit 1;
  | absyn ->
      Tiger.Semant.transProg absyn;
      print_endline (Tiger.Absyn.to_string absyn)
  );
  close_in ic;
