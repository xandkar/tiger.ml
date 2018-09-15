let () =
  let dir = ref "testcases" in
  Arg.parse [("-dir", String (fun s -> dir := s), "")] (fun _ -> ()) "";
  let dir = !dir in
  Tiger.Test.run (Tiger.Test_cases.all ~dir)
