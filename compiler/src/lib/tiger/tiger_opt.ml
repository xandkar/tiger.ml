type 'a t = 'a option

let map t f =
  match t with
  | None   -> None
  | Some x -> Some (f x)

let iter t ~f =
  ignore (map t f)

let get t ~default =
  match t with
  | None   -> default
  | Some x -> x
