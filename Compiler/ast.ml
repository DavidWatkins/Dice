type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or
type scope = Private | Public
type bool = True | False
type datatype = Int | Double | Void

type expr =
    Literal of int
  | Id of string
  | Deref of string * string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type vdecl = datatype * string
type field = scope * datatype * string
type include = string

type func_decl = {
  fname : string;
  formals : string list;
  locals : string list;
  body : stmt list;
}

type class_decl = {
  cname : string;
  extends : string;
  constructors : func_decl list;
  fields : field list;
  methods : func_decl list;
}

type program = include list * class_decl list
