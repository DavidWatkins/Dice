open Ast

type sexpr =
		Int_Lit of int
	| 	Boolean_Lit of bool
	| 	Float_Lit of float
	| 	String_Lit of string
	| 	Char_Lit of char
	| 	This
	| 	Id of string
	| 	Binop of sexpr * op * sexpr
	| 	Assign of sexpr * sexpr
	| 	Noexpr
	|  	ArrayCreate of datatype * sexpr list
	| 	ArrayAccess of sexpr * sexpr list
	| 	ObjAccess of sexpr * sexpr
	| 	Call of string * sexpr list  
	| 	ArrayPrimitive of sexpr list
	| 	Null

type sstmt =
		Block of sstmt list
	| 	Expr of sexpr
	| 	Return of sexpr
	| 	If of sexpr * sstmt * sstmt
	| 	For of sexpr * sexpr * sexpr * sstmt
	| 	While of sexpr * sstmt
	|  	Break
	|   Continue

type sfunc_decl = {
	fname : fname;
	returnType : datatype;
	formals : formal list;
	locals : vdecl list;
	body : sstmt list;
}

type sclass_decl = {
	cname : string;
	fields : field list;
	methods : sfunc_decl list;
}

(* Class Declarations | All method declarations | Main entry method *)
type sprogram = Program of sclass_decl list * sfunc_decl list * sfunc_decl