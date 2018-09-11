module Pos = Tiger_position

exception T of string

let exn ~pos msg =
  let msg = Printf.sprintf "Error: %s. In %s." msg (Pos.to_string pos) in
  raise (T msg)
