type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or
type scope = Private | Public
type bool = True | False
type datatype =   Arraytype of primitive * int
                | Datatype of primitive 
type primitive = 
    Int
  | Float
  | Void
  | Bool
  | Char
  | Objecttype of string
  | ConstructorType

type extends = NoParent | Parent of string
type fname = Constructor | FName of string

type expr =
    Int_Lit of int
  | Boolean_Lit of bool
  | Float_Lit of float
  | String_Lit of string
  | Char_Lit of char
  | This
  | Id of string
  | Binop of expr * op * expr
  | Assign of expr * expr
  | Noexpr
  | Primitive of primitive
  | ArrayOp of expr * expr list
  | ObjAccess of expr * expr
  | Call of string * expr list  
  | ArrayPrimitive of expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type vdecl = Vdecl of datatype * string
type field = Field of scope * datatype * string
type include = Include of string

type func_decl = {
  scope : scope;
  fname : fname;
  returnType : datatype;
  formals : vdecl list;
  locals : vdecl list;
  body : stmt list;
}

type class_decl = {
  cname : string;
  extends : extends;
  body: cbody;
}

type cbody = {
  fields : field list;
  constructors : func_decl list;
  methods : func_decl list;
}

type program = Program of include list * class_decl list