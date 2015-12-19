open Ast

type sexpr =
		SInt_Lit of int * datatype
	| 	SBoolean_Lit of bool * datatype
	| 	SFloat_Lit of float * datatype
	| 	SString_Lit of string * datatype
	| 	SChar_Lit of char * datatype
	| 	SId of string * datatype
	| 	SBinop of sexpr * op * sexpr * datatype
	| 	SAssign of sexpr * sexpr * datatype
	| 	SNoexpr of datatype
	| 	SArrayCreate of datatype * sexpr list * datatype
	| 	SArrayAccess of sexpr * sexpr list * datatype
	| 	SObjAccess of sexpr * sexpr * datatype
	| 	SCall of string * sexpr list * datatype
	|   SObjectCreate of string * sexpr list * datatype
	| 	SArrayPrimitive of sexpr list * datatype
	|  	SUnop of op * sexpr * datatype
	| 	SNull of datatype

type sstmt =
		SBlock of sstmt list
	| 	SExpr of sexpr * datatype
	| 	SReturn of sexpr  * datatype
	| 	SIf of sexpr * sstmt * sstmt
	| 	SFor of sexpr * sexpr * sexpr * sstmt
	| 	SWhile of sexpr * sstmt
	|  	SBreak
	|   SContinue
	|   SLocal of datatype * string * sexpr

type func_type = User | Reserved

type sfunc_decl = {
	sfname : fname;
	sreturnType : datatype;
	sformals : formal list;
	sbody : sstmt list;
	func_type : func_type;
}

type sclass_decl = {
	scname : string;
	sfields : field list;
    sfuncs: sfunc_decl list;
}

(* Class Declarations | All method declarations | Main entry method *)
type sprogram =  {
	classes : sclass_decl list;
	functions : sfunc_decl list;
	main : sfunc_decl;
	reserved : sfunc_decl list;
}
