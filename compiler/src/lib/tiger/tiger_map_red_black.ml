module List = ListLabels

type color = R | B
type ('k, 'v) t =
  | Leaf
  | Node of color * ('k * 'v) * ('k, 'v) t * ('k, 'v) t

let empty = Leaf

let set t ~k ~v =
  let balance = function
             | Leaf -> assert false
    (* LL *) | Node (B, x, Node (R, lx, Node (R, llx, lll, llr), lr                     ), r                                                              ) -> Node (R, lx , Node (B, llx, lll, llr), Node (B, x  , lr , r  ))
    (* LR *) | Node (B, x, Node (R, lx, ll                     , Node (R, lrx, lrl, lrr)), r                                                              ) -> Node (R, lrx, Node (B, lx , ll , lrl), Node (B, x  , lrr, r  ))
    (* RL *) | Node (B, x, l                                                             , Node (R, rx, Node (R, rlx, rll, rlr), rr                      )) -> Node (R, rlx, Node (B, x  , l  , rll), Node (B, rx , rlr, rr ))
    (* RR *) | Node (B, x, l                                                             , Node (R, rx, rl                      , Node (R, rrx, rrl, rrr))) -> Node (R, rx , Node (B, x  , l  , rl ), Node (B, rrx, rrl, rrr))
             | node -> node  (* Fragile pattern. Shall we reconsider? *)
  in
  let rec set t k v =
    match t with
    (* Because we recur, we cannot at this stage know if the following Node
     * with Leaf children will end-up as root (in which case it should be
     * black) or as the last node before leaves (in which case it should be
     * red).  We begin by assuming that it is the later and at the very end,
     * before returning to the caller, force the actual root node (which is
     * easy to identify at that point) to be black, regardless of what it
     * already is.
     *)
    | Leaf -> Node (R, (k, v), Leaf, Leaf)  (* Maybe root or last node, so R *)
    | Node (c, ((k', _) as x), l, r) when k < k' -> balance (Node (c, x, set l k v, r))
    | Node (c, ((k', _) as x), l, r) when k > k' -> balance (Node (c, x, l        , set r k v))
    | Node (c, _             , l, r) -> Node (c, (k, v), l, r)
  in
  match set t k v with
  | Leaf -> assert false  (* Can we GADT this away? *)
  | Node (_, (k, v), l, r) -> Node (B, (k, v), l, r)  (* Root is always Black *)

let rec get t ~k =
  match t with
  | Leaf -> None
  | Node (_, (k', _), l, _) when k < k' -> get l ~k
  | Node (_, (k', _), _, r) when k > k' -> get r ~k
  | Node (_, (_ , v), _, _) -> Some v

let rec member t ~k =
  match t with
  | Leaf -> false
  | Node (_, (k', _), l, _) when k < k' -> member l ~k
  | Node (_, (k', _), _, r) when k > k' -> member r ~k
  | Node (_, (_ , _), _, _) -> true

let to_edges t =
  let rec to_edges_from node1 t =
    match t with
    | Leaf -> [(node1, `Leaf)]
    | Node (c2, (k2, _), l, r) ->
        let node2 = k2, c2 in
        (node1, `Node node2) :: ((to_edges_from node2 l) @ (to_edges_from node2 r))
  in
  match t with
  | Leaf ->
      []
  | Node (c, (k, _), l, r) ->
      let node1 = k, c in
      (to_edges_from node1 l) @ (to_edges_from node1 r)

let color_to_string = function
  | B -> "black"
  | R -> "red"

let to_dot t ~k_to_string =
  let (dot_edges_and_nodes, _, _) =
    List.fold_left
      (to_edges t)
      ~init:("", "\n", 0)
      ~f:(fun (dot_edges_and_nodes, sep, leaves) ((k1, c1), node2_or_leaf) ->
        let k1 = k_to_string k1 in
        let k2, c2, leaves =
          match node2_or_leaf with
          | `Leaf ->
              let leaves = succ leaves in
              let label = Printf.sprintf "Leaf_%d" leaves in
              (label, B, leaves)
          | `Node (k2, c2) ->
              let label = k_to_string k2 in
              (label, c2, leaves)
        in
        let dot_edges_and_nodes =
          Printf.sprintf
            "%s%s    %S -> %S;\n    %S [color=%s,fontcolor=white,style=filled];\n    %S [color=%s,fontcolor=white,style=filled];\n"
            dot_edges_and_nodes
            sep
            k1 k2
            k1 (color_to_string c1)  (* Yes, it's redundant... *)
            k2 (color_to_string c2)
        in
        let sep = "" in
        (dot_edges_and_nodes, sep, leaves)
    )
  in
  "digraph G {" ^ dot_edges_and_nodes ^ "}"

let of_list pairs =
  List.fold_left pairs ~init:empty ~f:(fun t (k, v) -> set t ~k ~v)
