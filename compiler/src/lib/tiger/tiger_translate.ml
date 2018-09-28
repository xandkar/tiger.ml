module List = ListLabels

module Frame = Tiger_frame
module Temp  = Tiger_temp
module T     = Tiger_tree

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

type gen_stm =
  (Tiger_temp.Label.t * Tiger_temp.Label.t) -> Tiger_tree.stm

type exp =
  | Ex of Tiger_tree.exp
  | Nx of Tiger_tree.stm
  | Cx of gen_stm

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

let rec seq = function
  (* TODO: Is appending 0 OK? How else can the empty case be handled? *)
  | []      -> T.EXP (T.CONST 0)
  | s :: ss -> T.SEQ (s, seq ss)

let cond_stm gen_stm =
  let t = Temp.Label.gen () in
  let f = Temp.Label.gen () in
  let r = Temp.Temp.gen () in
  let stms =
    [ T.MOVE (T.TEMP r, T.CONST 1)
    ; gen_stm (t, f)
    ; T.LABEL f
    ; T.MOVE (T.TEMP r, T.CONST 0)
    ; T.LABEL t
    ]
  in
  (seq stms, T.TEMP r)

let unEx = function
  | Ex exp -> exp
  | Nx stm -> T.ESEQ (stm, T.CONST 0)
  | Cx gen ->
      let stm, exp = cond_stm gen in
      T.ESEQ (stm, exp)

let unNx = function
  | Ex exp -> T.EXP exp
  | Nx stm -> stm
  | Cx gen -> fst (cond_stm gen)

let unCx = function
  (* "should never occur in compiling a well typed Tiger program" p.154 *)
  | Nx _ -> assert false
  | Ex e -> fun (_, _) -> T.EXP e  (* TODO: Is this right? *)
  | Cx g -> g

let dummy__FIXME = Ex (T.CONST 0)
