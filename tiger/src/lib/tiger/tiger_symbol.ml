module H = MoreLabels.Hashtbl

type t =
  { name   : string
  ; symbol : int
  }

let nextsym = ref 0

let symbols = H.create 16

let of_string name =
  match H.find_opt symbols name with
  | Some symbol ->
      {name; symbol}
  | None ->
      incr nextsym;
      let symbol = !nextsym in
      H.replace symbols ~key:name ~data:symbol;
      {name; symbol}

let to_string {name; _} =
  name
