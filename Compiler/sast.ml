open Ast

type sexpr =
		SInt_Lit of int
	| 	SBoolean_Lit of bool
	| 	SFloat_Lit of float
	| 	SString_Lit of string
	| 	SChar_Lit of char
	| 	SThis
	| 	SId of string
	| 	SBinop of sexpr * op * sexpr
	| 	SAssign of sexpr * sexpr
	| 	SNoexpr
	|  	SArrayCreate of datatype * sexpr list
	| 	SArrayAccess of sexpr * sexpr list
	| 	SObjAccess of sexpr * sexpr
	| 	SCall of string * sexpr list  
	| 	SArrayPrimitive of sexpr list
	| 	SNull

type sstmt =
		SBlock of sstmt list
	| 	SExpr of sexpr
	| 	SReturn of sexpr
	| 	SIf of sexpr * sstmt * sstmt
	| 	SFor of sexpr * sexpr * sexpr * sstmt
	| 	SWhile of sexpr * sstmt
	|  	SBreak
	|   SContinue

type sfunc_decl = {
	sfname : fname;
	sreturnType : datatype;
	sformals : formal list;
	sbody : sstmt list;
}

type sclass_decl = {
	scname : string;
	sfields : field list;
	smethods : sfunc_decl list;
}

(* Class Declarations | All method declarations | Main entry method *)
type sprogram = SProgram of sclass_decl list * sfunc_decl list * sfunc_decl