module Array = ArrayLabels
module List = ListLabels

module type TREE = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t

  val set : ('k, 'v) t -> k:'k -> v:'v -> ('k, 'v) t

  val get : ('k, 'v) t -> k:'k -> 'v option

  val member : ('k, 'v) t -> k:'k -> bool

  val to_dot : ('k, 'v) t -> k_to_string:('k -> string) -> string
end

module BinaryTree : TREE = struct
  type ('k, 'v) t =
    | Node of 'k * 'v * ('k, 'v) t * ('k, 'v) t
    | Leaf

  let empty = Leaf

  let rec set t ~k ~v =
    match t with
    | Leaf -> Node (k, v, Leaf, Leaf)
    | Node (k', v', l, r) when k < k' -> Node (k', v', set l ~k ~v, r)
    | Node (k', v', l, r) when k > k' -> Node (k', v', l, set r ~k ~v)
    | Node (k, _, l, r) -> Node (k, v, l, r)

  let rec get t ~k =
    match t with
    | Leaf -> None
    | Node (k', _, l, _) when k < k' -> get l ~k
    | Node (k', _, _, r) when k > k' -> get r ~k
    | Node (_, v, _, _) -> Some v

  let rec member t ~k =
    match t with
    | Leaf -> false
    | Node (k', _, l, _) when k < k' -> member l ~k
    | Node (k', _, _, r) when k > k' -> member r ~k
    | Node _ -> true

  let to_edges t =
    let rec to_edges_from k1 t =
      match t with
      | Leaf -> []
      | Node (k2, _, l, r) ->
          (k1, k2) :: ((to_edges_from k2 l) @ (to_edges_from k2 r))
    in
    match t with
    | Leaf -> []
    | Node (k, _, l, r) -> (to_edges_from k l) @ (to_edges_from k r)

  let to_dot t ~k_to_string =
    let (edges, _) =
      List.fold_left (to_edges t)
        ~init:("", "\n")
        ~f:(fun (edges, sep) (k1, k2) ->
          let k1, k2 = k_to_string k1, k_to_string k2 in
          (Printf.sprintf "%s%s    %S -> %S;\n" edges sep k1 k2, "")
      )
    in
    "digraph G {" ^ edges ^ "}"
end

let () =
  let tree_a = BinaryTree.empty in
  let tree_a = BinaryTree.set tree_a ~k:"k1" ~v:"v1" in
  let tree_a = BinaryTree.set tree_a ~k:"k2" ~v:"v2" in
  assert (BinaryTree.member tree_a ~k:"k1");
  assert (BinaryTree.member tree_a ~k:"k2");
  assert (Some "v1" = BinaryTree.get tree_a ~k:"k1");
  assert (Some "v2" = BinaryTree.get tree_a ~k:"k2");
  let tree_b =
    Array.fold_left (Sys.argv)
      ~init:BinaryTree.empty
      ~f:(fun t k -> BinaryTree.set t ~k ~v:())
  in
  print_endline (BinaryTree.to_dot tree_b ~k_to_string:(fun x -> x))
