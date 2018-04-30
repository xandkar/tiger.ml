let () =
  let bar = String.make 80 '-' in
  Printf.printf "%s\nTiger says: %S\n%s\n" bar Tiger.Growl.text bar;
