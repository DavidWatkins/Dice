type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or
type scope = Private | Public
type primitive = Int_t | Float_t | Void_t | Bool_t | Char_t | Objecttype of string | ConstructorType | Null_t
type datatype = Arraytype of primitive * int | Datatype of primitive | Any

type extends = NoParent | Parent of string
type fname = Constructor | FName of string
type formal = Formal of datatype * string | Many of datatype

type expr =
		Int_Lit of int
	| 	Boolean_Lit of bool
	| 	Float_Lit of float
	| 	String_Lit of string
	| 	Char_Lit of char
	| 	This
	| 	Id of string
	| 	Binop of expr * op * expr
	| 	Assign of expr * expr
	| 	Noexpr
	| 	ArrayCreate of datatype * expr list
	| 	ArrayAccess of expr * expr list
	| 	ObjAccess of expr * expr
	| 	Call of string * expr list  
	|   ObjectCreate of string * expr list
	| 	ArrayPrimitive of expr list
	|  	Unop of op * expr
	| 	Null

type stmt =
		Block of stmt list
	| 	Expr of expr
	| 	Return of expr
	| 	If of expr * stmt * stmt
	| 	For of expr * expr * expr * stmt
	| 	While of expr * stmt
	|  	Break
	|   Continue
	|   Local of datatype * string * expr

type field = Field of scope * datatype * string
type include_stmt = Include of string

type func_decl = {
	scope : scope;
	fname : fname;
	returnType : datatype;
	formals : formal list;
	body : stmt list;
}

type cbody = {
	fields : field list;
	constructors : func_decl list;
	methods : func_decl list;
}

type class_decl = {
	cname : string;
	extends : extends;
	cbody: cbody;
}

type program = Program of include_stmt list * class_decl list