open Printf

module List    = ListLabels

module Map    = Tiger_map
module Symbol = Tiger_symbol

type unique =
  unit ref

type t =
  | Unit
  | Nil
  | Int
  | String
  | Record of
      { unique : unique
      ; fields : record_fields
      }
  | Array of
      { unique : unique
      ; ty     : t
      }
  | Name of Symbol.t * t option ref
and record_fields =
  (Tiger_symbol.t * t) list

type env =
  (Symbol.t, t ) Map.t

let new_unique () =
  ref ()

let new_record fields =
  Record
    { fields
    ; unique = new_unique ()
    }

let new_array ty =
  Array
    { ty
    ; unique = new_unique ()
    }

let is_equal t1 t2 =
  match t1, t2 with
  | Record {unique=u1; _},  Record {unique=u2; _} -> u1 == u2
  | Array  {unique=u1; _},  Array  {unique=u2; _} -> u1 == u2
  | t1                   , t2                     -> t1 =  t2
  (* The above pattern matching is "fragile" and I'm OK with it.
   * TODO: Can we ignore the warning locally?
   * *)

let is_int t =
  t = Int

let is_string t =
  t = String

let is_array = function
  | Unit
  | Int
  | String
  | Name _
  | Nil
  | Record _ -> false
  | Array  _ -> true

let is_record = function
  | Unit
  | Int
  | String
  | Name _
  | Nil
  | Array  _ -> false
  | Record _ -> true

let is_name = function
  | Unit
  | Nil
  | String
  | Int
  | Record _
  | Array  _ -> false
  | Name _   -> true

let if_record t ~f ~otherwise =
  match t with
  | Record {fields; _} ->
      f fields
  | Unit
  | Int
  | String
  | Name _
  | Nil
  | Array  _ ->
      otherwise ()

let if_array t ~f ~otherwise =
  match t with
  | Array {ty=t; _} ->
      f t
  | Unit
  | Int
  | String
  | Name _
  | Nil
  | Record _ ->
      otherwise ()

let to_string = function
  | Unit               -> "unit"
  | Nil                -> "nil"
  | String             -> "string"
  | Record {unique; _} -> sprintf "record(%d)" (Obj.magic unique)
  | Array  {unique; _} -> sprintf "array(%d)"  (Obj.magic unique)
  | Int                -> "int"
  | Name (name, _)     -> Symbol.to_string name

let built_in =
  [ ("unit"   , Unit)
  ; ("nil"    , Nil)
  ; ("int"    , Int)
  ; ("string" , String)
  ]
  |> List.map ~f:(fun (k, v) -> (Symbol.of_string k, v))
  |> Map.of_list
