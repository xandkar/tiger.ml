module Map   = Tiger_map
module Sym   = Tiger_symbol
module Translate = Tiger_translate
module Type  = Tiger_env_type
module Value = Tiger_env_value

type t =
  { typs : Type.env
  ; vals : Value.env
  ; loop : Sym.t option
  ; level : Translate.Level.t
  }

let base =
  { typs = Type.built_in
  ; vals = Value.built_in
  ; loop = None
  ; level = Translate.Level.init
  }

let get_typ {typs; _} k =
  Map.get typs ~k

let get_val {vals; _} k =
  Map.get vals ~k

let set_typ t k v =
  {t with typs = Map.set t.typs ~k ~v}

let set_val t k v =
  {t with vals = Map.set t.vals ~k ~v}

let loop_begin t =
  let loop = Sym.unique_of_string "loop" in
  let t = {t with loop = Some loop} in
  (loop, t)

let loop_end t given =
  match t.loop with
  | None ->
      assert false
  | Some current when (not (Sym.is_equal current given)) ->
      assert false
  | Some _ ->
      {t with loop = None}

let loop_current {loop; _} =
  loop

let level_set t level =
  {t with level}

let level_get {level; _} =
  level
