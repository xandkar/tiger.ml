type exp =
  | CONST of int
  | NAME of Tiger_temp.Label.t
  | TEMP of Tiger_temp.Temp.t
  | BINOP of binop * exp * exp
  | MEM of exp
  | CALL of exp * exp list
  | ESEQ of stm * exp
and stm =
  | MOVE of exp * exp
  | EXP of exp
  | JUMP of exp * Tiger_temp.Label.t list
  (* List is for possible locations that exp can evaluate to, which will be
   * needed for dataflow analysis, but for now, the common case will be
   * JUMP(NAME, l, [l]) *)
  | CJUMP of relop * exp * exp * Tiger_temp.Label.t * Tiger_temp.Label.t
  | SEQ of stm * stm
  | LABEL of Tiger_temp.Label.t
and binop =
  | PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
and relop =
  | EQ | NE | LT | GT | LE | GE
  | ULT | ULE | UGT | UGE

type t = exp
