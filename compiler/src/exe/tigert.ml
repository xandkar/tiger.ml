type suite =
  | All  of {dir : string}
  | Book of {dir : string}
  | Micro

let () =
  let suite = ref Micro in
  let spec = ref [] in
  Arg.parse_dynamic
    spec
    (function
    | "micro" ->
        spec := []
    | "book" ->
        spec := [("-dir", Arg.String (fun dir -> suite := Book {dir}), "")]
    | "all" ->
        spec := [("-dir", Arg.String (fun dir -> suite := All  {dir}), "")]
    | _ -> ()
    )
    "";
  let suite =
    match !suite with
    | All  {dir} -> Tiger.Test_cases.all  ~dir
    | Book {dir} -> Tiger.Test_cases.book ~dir
    | Micro      -> Tiger.Test_cases.micro
  in
  Tiger.Test.run suite
