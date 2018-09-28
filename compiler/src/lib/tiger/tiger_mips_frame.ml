module List = ListLabels

module Temp = Tiger_temp

type access =
  | InFrame of {offset_from_frame_pointer : int}
  | InReg of {register : Temp.Temp.t}

type t =
(* p.136 Frame.t is a data structure holding:
 * - the locations of all the formals
 * - instructions required to implement the "view shift"
 * - the number of locals allocated so far
 * - the `label` at which the function's machine code is to begin (see p.140)
 * *)
  { name         : Temp.Label.t
  ; formals      : access list
  ; locals_count : int
  ; instructions : unit  (* TODO: instructions for view shift *)
  }

let word_size_bits  = 32
let word_size_bytes = word_size_bits / 8
let word_size       = word_size_bytes

let name {name; _} =
  name

let formals {formals; _} =
  formals

let alloc offset_from_frame_pointer ~escapes =
  if escapes then
    InFrame {offset_from_frame_pointer}
  else
    InReg {register = Temp.Temp.gen ()}

let alloc_local _ ~escapes =
    (* FIXME: offset_from_frame_pointer. With neither mutation nor new frame? *)
    let offset_from_frame_pointer = 0 in
    alloc offset_from_frame_pointer ~escapes

let make ~name ~formals =
  (* p.136: For each formal parameter, "newFrame" must calculate two things:
   * - How the parameter will be seen from inside the function
   *   (in a register, or in a frame location);
   * - What instructions must be produced to implement the "view shift"
   * *)
  let formals, locals_count =
   (* TODO: What should offset increment be? Word? *)
    List.fold_left formals ~init:([], 0) ~f:(fun (formals, offset) escapes ->
      ((alloc offset ~escapes) :: formals, succ offset)
    )
  in
  { name
  ; formals
  ; locals_count
  ; instructions = ()  (* TODO: instructions for view shift *)
  }
