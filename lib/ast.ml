type loc = { start_pos : int; end_pos : int }

type expr =
  | Identity (* . *)
  | Literal of literal
  | Field of { name : string; optional : bool; loc : loc }
  | Index of { expr : expr; index : index_expr; loc : loc }
  | Pipe of { left : expr; right : expr }
  | FnCall of { name : string; args : expr list; loc : loc }
  | BinOp of { op : binop; left : expr; right : expr; loc : loc }
  | UnaryOp of { op : unaryop; expr : expr; loc : loc }
  | ObjectConstruct of { fields : obj_field list; loc : loc }
  | ArrayConstruct of { elements : expr list; loc : loc }
  | If of { cond : expr; then_ : expr; else_ : expr option; loc : loc }
  | Let of { name : string; value : expr; body : expr; loc : loc }
  | Lambda of { param : string; body : expr; loc : loc }
  | StringInterp of { parts : interp_part list; loc : loc }

and interp_part =
  | LitPart of string
  | ExprPart of expr

and literal =
  | Null
  | Bool of bool
  | Int of int
  | BigInt of Z.t
  | Float of float
  | String of string

and index_expr =
  | Single of expr
  | Slice of expr option * expr option (* [start:end] *)

and obj_field =
  | Punned of string (* {name} => {name: .name} *)
  | Explicit of { key : string; value : expr } (* {key: expr} *)

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  | Concat (* ++ *)
  | NullCoalesce (* ?? *)

and unaryop = Not

let dummy_loc = { start_pos = 0; end_pos = 0 }
