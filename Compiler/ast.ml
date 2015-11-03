type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or
type scope = Private | Public
type bool = True | False
type datatype = 
    Arraytype of string 
  | Datatype of string

type expr =
    Int_Lit of int
  | Boolean_Lit of bool
  | Float_Lit of float
  | String_Lit of string
  | Char_Lit of char
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | ObjCreate of string * expr list
  | ObjAccess of string * expr list (* Replaced "Call" *)
  | ArrayCreate of datatype * string * expr list
  | ArrayAccess of string * expr
  | ThisArrayAccess of string * expr
  | SelfAccess of string
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
  scope : scope;
  fname : string;
  formals : string list;
  locals : string list;
  body : stmt list;
}

type class_decl = {
  cname : string;
  extends : string;
  body: cbody;
}

type cbody = {
  fields : field list;
  constructors : func_decl list;
  methods : func_decl list;
}

type program = Program of include list * class_decl list
