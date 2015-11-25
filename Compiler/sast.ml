open Ast

type sstmt =
		SBlock of sstmt list
	| 	SExpr of expr * datatype
	| 	SReturn of expr
	| 	SIf of expr * sstmt * sstmt
	| 	SFor of expr * expr * expr * sstmt
	| 	SWhile of expr * sstmt
	|  	SBreak
	|   SContinue
	|   SLocal of datatype * string * expr

type sfunc_decl = {
	sfname : fname;
	sreturnType : datatype;
	sformals : formal list;
	sbody : sstmt list;
}

type sclass_decl = {
	scname : string;
	sfields : field list;
}

(* Class Declarations | All method declarations | Main entry method *)
type sprogram = SProgram of sclass_decl list * sfunc_decl list * sfunc_decl