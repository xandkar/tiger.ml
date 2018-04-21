module Array = ArrayLabels

module Tree_vanilla  = Tree_unbalanced_vanilla
module Tree_redblack = Tree_balanced_red_black

let () =
  let unbalanced = Tree_vanilla.empty in
  let unbalanced = Tree_vanilla.set unbalanced ~k:"k1" ~v:"v1" in
  let unbalanced = Tree_vanilla.set unbalanced ~k:"k2" ~v:"v2" in
  assert (Tree_vanilla.member unbalanced ~k:"k1");
  assert (Tree_vanilla.member unbalanced ~k:"k2");
  assert (Some "v1" = Tree_vanilla.get unbalanced ~k:"k1");
  assert (Some "v2" = Tree_vanilla.get unbalanced ~k:"k2");

  let balanced = Tree_redblack.empty in
  let balanced = Tree_redblack.set balanced ~k:"k1" ~v:"v1" in
  let balanced = Tree_redblack.set balanced ~k:"k2" ~v:"v2" in
  assert (Tree_redblack.member balanced ~k:"k1");
  assert (Tree_redblack.member balanced ~k:"k2");
  assert (Some "v1" = Tree_redblack.get balanced ~k:"k1");
  assert (Some "v2" = Tree_redblack.get balanced ~k:"k2");

  (*let unbalanced =*)
    (*Array.fold_left (Sys.argv)*)
      (*~init:Tree_vanilla.empty*)
      (*~f:(fun t k -> Tree_vanilla.set t ~k ~v:())*)
  (*in*)
  (*print_endline (Tree_vanilla.to_dot unbalanced ~k_to_string:(fun x -> x));*)

  let balanced =
    Array.fold_left (Sys.argv)
      ~init:Tree_redblack.empty
      ~f:(fun t k -> Tree_redblack.set t ~k ~v:())
  in
  print_endline (Tree_redblack.to_dot balanced ~k_to_string:(fun x -> x))
