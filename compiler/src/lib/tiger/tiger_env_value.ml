module List    = ListLabels

module Map    = Tiger_map
module Symbol = Tiger_symbol
module Type   = Tiger_env_type

type t =
  | Var of
      {ty : Type.t}
  | Fun of
      { formals : Type.t list
      ; result  : Type.t
      }

type env =
  (Symbol.t, t ) Map.t

let built_in =
  [ ("print"     , [Type.String]                     , Type.Unit   )
  ; ("flush"     , []                                , Type.Unit   )
  ; ("getchar"   , []                                , Type.String )
  ; ("ord"       , [Type.String]                     , Type.Int    )
  ; ("chr"       , [Type.Int]                        , Type.String )
  ; ("size"      , [Type.String]                     , Type.Int    )
  ; ("substring" , [Type.String; Type.Int; Type.Int] , Type.String )
  ; ("concat"    , [Type.String; Type.String]        , Type.String )
  ; ("not"       , [Type.Int]                        , Type.Int    )
  ; ("exit"      , [Type.Int]                        , Type.Unit   )
  ]
  |> List.map ~f:(fun (name, formals, result) ->
      let key   = Symbol.of_string name in
      let value = Fun {formals; result} in
      (key, value)
    )
  |> Map.of_list
