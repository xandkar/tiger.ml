type t =
  | Invalid_syntax    of Tiger_position.t
  | Unknown_id        of {id    : Tiger_symbol.t; pos : Tiger_position.t}
  | Unknown_type      of {ty_id : Tiger_symbol.t; pos : Tiger_position.t}
  | Id_not_a_function of {id    : Tiger_symbol.t; pos : Tiger_position.t}
  | Wrong_type_of_expression_in_var_dec of
      { var_id   : Tiger_symbol.t
      ; expected : Tiger_env_type.t
      ; given    : Tiger_env_type.t
      ; pos      : Tiger_position.t
      }
  | Wrong_type_used_as_record of
      { ty_id    : Tiger_symbol.t
      ; ty       : Tiger_env_type.t
      ; pos      : Tiger_position.t
      }
  | Wrong_type_of_field_value of
      { field_id : Tiger_symbol.t
      ; expected : Tiger_env_type.t
      ; given    : Tiger_env_type.t
      ; pos      : Tiger_position.t
      }
  | Wrong_type_of_arg of
      { func     : Tiger_symbol.t
      ; expected : Tiger_env_type.t
      ; given    : Tiger_env_type.t
      ; pos      : Tiger_position.t
      }
  | Wrong_number_of_args of
      { func     : Tiger_symbol.t
      ; expected : int
      ; given    : int
      ; pos      : Tiger_position.t
      }
  | Invalid_operand_type of
      { oper  : Tiger_absyn.oper
      ; valid : string list
      ; given : Tiger_env_type.t
      ; pos   : Tiger_position.t
      }
  | Different_operand_types of
      { oper  : Tiger_absyn.oper
      ; left  : Tiger_env_type.t
      ; right : Tiger_env_type.t
      ; pos   : Tiger_position.t
      }

exception T of t

val raise : t -> 'a

val to_string : t -> string

val is_unknown_id : t -> bool
