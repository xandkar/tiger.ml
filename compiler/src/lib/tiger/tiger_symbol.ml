module H = MoreLabels.Hashtbl

type t =
  { name   : string
  ; symbol : int
  }

let next =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let symbols = H.create 16

let unique_of_string name =
  {name; symbol = next ()}

let of_string name =
  match H.find_opt symbols name with
  | Some symbol ->
      {name; symbol}
  | None ->
      let t = unique_of_string name in
      H.replace symbols ~key:t.name ~data:t.symbol;
      t

let to_string {name; _} =
  name

let is_equal {symbol=s1; _} {symbol=s2; _} =
  s1 = s2

let show {name; symbol} =
  Printf.sprintf "Symbol[%S, %d]" name symbol
