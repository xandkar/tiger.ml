module Array = ArrayLabels
module List = ListLabels

module type TREE = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t

  val set : ('k, 'v) t -> k:'k -> v:'v -> ('k, 'v) t

  val get : ('k, 'v) t -> k:'k -> 'v option

  val member : ('k, 'v) t -> k:'k -> bool

  val print
    : ('k, 'v) t
    -> k_to_string:('k -> string)
    -> indent_level:string
    -> unit
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

  let rec fold t ~f ~init:acc =
    match t with
    | Leaf -> acc
    | Node (k, v, l, r) -> fold r ~f ~init:(f (fold l ~f ~init:acc) (k, v))

  let print t ~k_to_string ~indent_level =
    let (_, nodes) =
      fold t
        ~f:(fun (indentation, nodes) (k, _) ->
            let indentation = indent_level ^ indentation in
            let node = indentation ^ (k_to_string k) in
            (indentation, node :: nodes)
          )
        ~init:(indent_level, [])
    in
    List.iter (List.rev nodes) ~f:print_endline
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
    Array.fold_left
      (Sys.argv)
      ~init:BinaryTree.empty
      ~f:(fun t k -> BinaryTree.set t ~k ~v:())
  in
  Printf.printf "%B\n" (BinaryTree.member tree_b ~k:"a");
  BinaryTree.print tree_b ~k_to_string:(fun x -> x) ~indent_level:"-";
