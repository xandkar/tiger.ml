module Array = ArrayLabels

module type TREE = sig
  type 'a t

  val empty : 'a t

  val add : 'a t -> 'a -> 'a t

  val member : 'a t -> 'a -> bool
end

module BinaryTree : TREE = struct
  type 'a t =
    | Node of 'a * 'a t * 'a t
    | Leaf

  let empty = Leaf

  let rec add t x =
    match t with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (x', left, right) when x < x' -> Node (x', add left x, right)
    | Node (x', left, right) when x > x' -> Node (x', left, add right x)
    | (Node _) as t' -> t'

  let rec member t x =
    match t with
    | Leaf -> false
    | Node (x', left, _) when x < x' -> member left x
    | Node (x', _, right) when x > x' -> member right x
    | Node _ -> true
end

let () =
  let tree =
    Array.fold_left
      (Sys.argv)
      ~init:BinaryTree.empty
      ~f:(fun t str -> BinaryTree.add t str)
  in
  Printf.printf "%B\n" (BinaryTree.member tree "a")
