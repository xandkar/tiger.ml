open Printf

module List = ListLabels

module Map = Tiger_map
module Sym = Tiger_symbol

type unique =
  Sym.t

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
  | Name of Sym.t * t option ref
and record_fields =
  (Tiger_symbol.t * t) list

type env =
  (Sym.t, t ) Map.t

let new_record ~name ~fields =
  Record
    { fields
    ; unique = Sym.unique_of_string (Sym.to_string name)
    }

let new_array ~name ~ty =
  Array
    { ty
    ; unique = Sym.unique_of_string (Sym.to_string name)
    }

let is_equal t1 t2 =
  match t1, t2 with
  | Name   (s1, _)       ,  Name   (s2, _)        -> Sym.is_equal s1 s2
  | Record {unique=s1; _},  Record {unique=s2; _} -> Sym.is_equal s1 s2
  | Record _             ,  Nil                   -> true
  | Nil                  ,  Record _              -> true
  | Array  {unique=s1; _},  Array  {unique=s2; _} -> Sym.is_equal s1 s2
  | t1                   , t2                     -> t1 = t2
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
  | Record {unique; _} -> sprintf "Record[%s]" (Sym.to_string unique)
  | Array  {unique; _} -> sprintf "Array[%s]"  (Sym.to_string unique)
  | Int                -> "int"
  | Name (name, _)     -> Sym.to_string name

let built_in =
  [ ("unit"   , Unit)
  ; ("nil"    , Nil)
  ; ("int"    , Int)
  ; ("string" , String)
  ]
  |> List.map ~f:(fun (k, v) -> (Sym.of_string k, v))
  |> Map.of_list
