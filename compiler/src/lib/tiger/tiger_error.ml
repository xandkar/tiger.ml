module Abs = Tiger_absyn
module Pos = Tiger_position
module Sym = Tiger_symbol
module Typ = Tiger_env_type

type t =
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

let raise t =
  raise (T t)

let to_string =
  let s = Printf.sprintf in
  function
  | Invalid_syntax pos ->
      s "Invalid syntax in %s" (Pos.to_string pos)
  | Unknown_id  {id; pos} ->
      s "Unknown identifier %S in %s" (Sym.to_string id) (Pos.to_string pos)
  | Unknown_type {ty_id; pos} ->
      s "Unknown type %S in %s" (Sym.to_string ty_id) (Pos.to_string pos)
  | Id_is_a_function {id; pos} ->
      s "Identifier %S is a function, it cannot be used as a variable in %s"
        (Sym.to_string id) (Pos.to_string pos)
  | Id_not_a_function {id; pos} ->
      s "Identifier %S is not a function, it cannot be called in %s"
        (Sym.to_string id) (Pos.to_string pos)
  | No_such_field_in_record {field; record; pos} ->
      s "No field %S in record %S in %s"
        (Sym.to_string field) (Typ.to_string record) (Pos.to_string pos)
  | Exp_not_a_record {ty; pos} ->
      s ( "The expression of type %S is not a record, it cannot be"
        ^^"accessed in %s")
        (Typ.to_string ty) (Pos.to_string pos)
  | Exp_not_an_array {ty; pos} ->
      s ( "The expression of type %S is not an array, it cannot be"
        ^^"accessed in %s")
        (Typ.to_string ty) (Pos.to_string pos)
  | Wrong_type {expected; given; pos} ->
      s "Type error: expected: %S, but given: %S, in %s"
        (Typ.to_string expected)
        (Typ.to_string given)
        (Pos.to_string pos)
  | Wrong_type_of_expression_in_var_dec {var_id; expected; given; pos} ->
      s ( "Wrong type of expression in declaration of %S. "
        ^^"Expected: %S, given: %S. In %s")
        (Sym.to_string var_id)
        (Typ.to_string expected)
        (Typ.to_string given)
        (Pos.to_string pos)
  | Wrong_type_used_as_array {ty_id; ty; pos} ->
      s ( "Identifier %S is bound to type %S, not an array. "
        ^^"It cannot be used in %s")
        (Sym.to_string ty_id) (Typ.to_string ty) (Pos.to_string pos)
  | Wrong_type_used_as_record {ty_id; ty; pos} ->
      s ( "Identifier %S is bound to type %S, not a record. "
        ^^"It cannot be used in %s")
        (Sym.to_string ty_id) (Typ.to_string ty) (Pos.to_string pos)
  | Wrong_type_of_field_value {field_id; expected; given; pos} ->
      s ( "Field %S is declared to be of type %S, but is bound to expression "
        ^^"of type %S in %s")
        (Sym.to_string field_id)
        (Typ.to_string expected)
        (Typ.to_string given)
        (Pos.to_string pos)
  | Wrong_type_of_arg {func; expected; given; pos} ->
      s ( "Incorrect type of argument to function %S, expected: %S, given: %S,"
        ^^" in %s")
        (Sym.to_string func)
        (Typ.to_string expected)
        (Typ.to_string given)
        (Pos.to_string pos)
  | Wrong_number_of_args {func; expected; given; pos} ->
      s ( "Incorrect number of arguments to function %S, "
        ^^"expected: %d, given: %d,"
        ^^" in %s")
        (Sym.to_string func) expected given (Pos.to_string pos)
  | Invalid_operand_type {oper; valid; given; pos} ->
      s ( "Invalid operand type %S for operator %S, which expects only: %s"
        ^^". In %s")
        (Typ.to_string given)
        (Abs.op_show oper)
        (String.concat ", " valid)
        (Pos.to_string pos)
  | Different_operand_types {oper; left; right; pos} ->
      s "Operands of different types (%S %S %S) given in %s"
        (Typ.to_string left)
        (Abs.op_show oper)
        (Typ.to_string right)
        (Pos.to_string pos)

let is_unknown_id t =
  match t with
  | Unknown_id _ ->
      true
  | Invalid_syntax _
  | Unknown_type _
  | Id_is_a_function _
  | Id_not_a_function _
  | No_such_field_in_record _
  | Exp_not_a_record _
  | Exp_not_an_array _
  | Wrong_type _
  | Wrong_type_of_expression_in_var_dec _
  | Wrong_type_used_as_array _
  | Wrong_type_used_as_record _
  | Wrong_type_of_field_value _
  | Wrong_type_of_arg _
  | Wrong_number_of_args _
  | Invalid_operand_type _
  | Different_operand_types _ ->
      false

let is_unknown_type t =
  match t with
  | Unknown_type _ ->
      true
  | Unknown_id _
  | Invalid_syntax _
  | Id_is_a_function _
  | Id_not_a_function _
  | No_such_field_in_record _
  | Exp_not_a_record _
  | Exp_not_an_array _
  | Wrong_type _
  | Wrong_type_of_expression_in_var_dec _
  | Wrong_type_used_as_array _
  | Wrong_type_used_as_record _
  | Wrong_type_of_field_value _
  | Wrong_type_of_arg _
  | Wrong_number_of_args _
  | Invalid_operand_type _
  | Different_operand_types _ ->
      false

let is_wrong_type t =
  match t with
  | Wrong_type _ ->
      true
  | Unknown_type _
  | Unknown_id _
  | Invalid_syntax _
  | Id_is_a_function _
  | Id_not_a_function _
  | No_such_field_in_record _
  | Exp_not_a_record _
  | Exp_not_an_array _
  | Wrong_type_of_expression_in_var_dec _
  | Wrong_type_used_as_array _
  | Wrong_type_used_as_record _
  | Wrong_type_of_field_value _
  | Wrong_type_of_arg _
  | Wrong_number_of_args _
  | Invalid_operand_type _
  | Different_operand_types _ ->
      false

let is_invalid_syntax t =
  match t with
  | Invalid_syntax _ ->
      true
  | Wrong_type _
  | Unknown_type _
  | Unknown_id _
  | Id_is_a_function _
  | Id_not_a_function _
  | No_such_field_in_record _
  | Exp_not_a_record _
  | Exp_not_an_array _
  | Wrong_type_of_expression_in_var_dec _
  | Wrong_type_used_as_array _
  | Wrong_type_used_as_record _
  | Wrong_type_of_field_value _
  | Wrong_type_of_arg _
  | Wrong_number_of_args _
  | Invalid_operand_type _
  | Different_operand_types _ ->
      false
