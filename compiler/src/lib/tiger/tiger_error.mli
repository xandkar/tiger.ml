module Abs = Tiger_absyn
module Pos = Tiger_position
module Sym = Tiger_symbol
module Typ = Tiger_env_type

type t =
  | Cycle_in_type_decs of
      { from_id  : Sym.t
      ; from_pos : Pos.t
      ;   to_id  : Sym.t
      ;   to_pos : Pos.t
      }
  | Invalid_syntax    of Pos.t
  | Unknown_id        of {id    : Sym.t; pos : Pos.t}
  | Unknown_type      of {ty_id : Sym.t; pos : Pos.t}
  | Id_is_a_function  of {id    : Sym.t; pos : Pos.t}
  | Id_not_a_function of {id    : Sym.t; pos : Pos.t}
  | No_such_field_in_record of {field : Sym.t; record : Typ.t; pos : Pos.t}
  | Exp_not_a_record  of {ty    : Typ.t; pos : Pos.t}
  | Exp_not_an_array  of {ty    : Typ.t; pos : Pos.t}
  | Wrong_type of
      { expected : Typ.t
      ; given    : Typ.t
      ; pos      : Pos.t
      }
  | Wrong_type_of_expression_in_var_dec of
      { var_id   : Sym.t
      ; expected : Typ.t
      ; given    : Typ.t
      ; pos      : Pos.t
      }
  | Wrong_type_used_as_record of
      { ty_id    : Sym.t
      ; ty       : Typ.t
      ; pos      : Pos.t
      }
  | Wrong_type_used_as_array of
      { ty_id    : Sym.t
      ; ty       : Typ.t
      ; pos      : Pos.t
      }
  | Wrong_type_of_field_value of
      { field_id : Sym.t
      ; expected : Typ.t
      ; given    : Typ.t
      ; pos      : Pos.t
      }
  | Wrong_type_of_arg of
      { func     : Sym.t
      ; expected : Typ.t
      ; given    : Typ.t
      ; pos      : Pos.t
      }
  | Wrong_number_of_args of
      { func     : Sym.t
      ; expected : int
      ; given    : int
      ; pos      : Pos.t
      }
  | Invalid_operand_type of
      { oper  : Abs.oper
      ; valid : string list
      ; given : Typ.t
      ; pos   : Pos.t
      }
  | Different_operand_types of
      { oper  : Abs.oper
      ; left  : Typ.t
      ; right : Typ.t
      ; pos   : Pos.t
      }

exception T of t

val raise : t -> 'a

val to_string : t -> string

val is_unknown_id   : t -> bool
val is_unknown_type : t -> bool
val is_wrong_type   : t -> bool
val is_wrong_number_of_args : t -> bool
val is_invalid_syntax : t -> bool
val is_not_a_record : t -> bool
val is_not_an_array : t -> bool
val is_no_such_field_in_record : t -> bool
val is_cycle_in_type_dec : t -> bool
