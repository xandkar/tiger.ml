module Map   = Tiger_map
module Type  = Tiger_env_type
module Value = Tiger_env_value

type t =
  { typs : Type.env
  ; vals : Value.env
  }

let base =
  { typs = Type.built_in
  ; vals = Value.built_in
  }

let get_typ {typs; _} k =
  Map.get typs ~k

let get_val {vals; _} k =
  Map.get vals ~k

let set_typ t k v =
  {t with typs = Map.set t.typs ~k ~v}

let set_val t k v =
  {t with vals = Map.set t.vals ~k ~v}
