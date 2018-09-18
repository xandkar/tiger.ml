module List = ListLabels

module Map = Tiger_map
module Opt = Tiger_opt

type t =
  unit

type count =
  { parents  : int
  ; children : int
  }

let of_list pairs =
  let incr counts k incr =
    let prev = Opt.get (Map.get counts ~k) ~default:{parents=0; children=0} in
    Map.set counts ~k ~v:(incr prev)
  in
  let incr_parents  count = {count with parents  = succ count.parents} in
  let incr_children count = {count with children = succ count.children} in
  let zero_children counts =
    List.filter (Map.to_list counts) ~f:(fun (_, {children=c; _}) -> c = 0 )
  in
  let zero_parents counts =
    List.filter (Map.to_list counts) ~f:(fun (_, {parents=p; _}) -> p = 0 )
  in
  let counts =
    List.fold_left pairs ~init:Map.empty ~f:(
      fun counts (p, c) ->
        let counts = incr counts p incr_children in
        let counts = incr counts c incr_parents  in
        counts
    )
  in
  (* At least one node with no in-coming links and
   * at least one node with no out-going links. *)
  match (zero_parents counts, zero_children counts) with
  | _ :: _, _ :: _ -> Ok ()
  |      _,      _ -> Error `Cycle
