module H = MoreLabels.Hashtbl

type t =
  { name   : string
  ; symbol : int
  }

let counter = ref 0

let symbols = H.create 16

let next name =
  incr counter;
  let symbol = !counter in
  {name; symbol}

let new_of_string name =
  let t = next name in
  H.replace symbols ~key:t.name ~data:t.symbol;
  t

let of_string name =
  match H.find_opt symbols name with
  | Some s -> {name; symbol=s}
  | None   -> new_of_string name

let to_string {name; _} =
  name

let is_equal {symbol=s1; _} {symbol=s2; _} =
  s1 = s2

let show {name; symbol} =
  Printf.sprintf "Symbol[%S, %d]" name symbol
