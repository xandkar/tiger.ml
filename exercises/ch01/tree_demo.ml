module Array = ArrayLabels

let () =
  let (module T : Tree.S) =
    match Sys.argv.(1) with
    | "balanced"   -> (module Tree_balanced_red_black : Tree.S)
    | "unbalanced" -> (module Tree_unbalanced_vanilla : Tree.S)
    | other ->
        failwith ("Expected: \"balanced\" | \"unbalanced\". Got: " ^ other)
  in

  let t = T.empty in
  let t = T.set t ~k:"k1" ~v:"v1" in
  let t = T.set t ~k:"k2" ~v:"v2" in
  assert (T.member t ~k:"k1");
  assert (T.member t ~k:"k2");
  assert (Some "v1" = T.get t ~k:"k1");
  assert (Some "v2" = T.get t ~k:"k2");
  Sys.argv
  |> Array.sub ~pos:2 ~len:((Array.length Sys.argv) - 2)
  |> Array.fold_left ~init:T.empty ~f:(fun t k -> T.set t ~k ~v:())
  |> T.to_dot ~k_to_string:(fun x -> x)
  |> print_endline
