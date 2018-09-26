module List = ListLabels

module Frame = Tiger_frame
module Temp  = Tiger_temp

module Level = struct
  type t =
    { parent  : t option
    ; name    : Temp.Label.t
    ; formals : bool list
    ; frame   : Frame.t
    }

  let init =
    let name    = Temp.Label.gen () in
    let formals = [] in
    { parent  = None
    ; name
    ; formals
    ; frame   = Frame.make ~name ~formals
    }

  let next t ~name  ~formals =
    (* Adding the extra parameter for the static link. See p. 142 *)
    let formals = true :: formals in
    { parent = Some t
    ; name
    ; formals
    ; frame = Frame.make ~name ~formals
    }

  let formals = function {formals; _} ->
    formals

  let frame = function {frame; _} ->
    frame
end

type exp = unit

type access =
  (* must know about static links *)
  { level        : Level.t
  ; frame_access : Frame.access
  }

let alloc_local ~level ~escapes =
  { level
  ; frame_access = Frame.alloc_local (Level.frame level) ~escapes
  }

let formals ~level =
  (* FIXME: This seems wrong. Should we call Frame.formals? *)
  List.map (Level.formals level) ~f:(fun escapes ->
    alloc_local ~level ~escapes
  )
