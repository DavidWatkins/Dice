type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or
type scope = Private | Public
type bool = True | False
type datatype = 
    Arraytype of string 
  | Datatype of string

type expr =
    Literal of int
  | This (* WHAT SHOULD GO HERE???*)
  | Boolean_Lit of bool
  | Float_Lit of float
  | String_Lit of string
  | Char_Lit of char
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | ObjCreate of string * expr list
  | ObjAccess of string * expr list
  | ArrayCreate of datatype * string * expr list
  | ArrayAccess of expr * expr
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type vdecl = Vdecl of datatype * string
type field = Field of scope * datatype * string
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

type program = Program of include list * class_decl list
