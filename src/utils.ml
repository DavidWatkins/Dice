(* Pretty Printer *)
open Ast
open Sast
open Parser
open Processor
open Yojson

let save file string =
     let channel = open_out file in
     output_string channel string;
     close_out channel

let replace input output =
    Str.global_replace (Str.regexp_string input) output
    
(* Print data types *)

let string_of_scope = function 
		Public 	-> "public"
	| 	Private -> "private"

let string_of_primitive = function 
		Int_t 						-> "int"
	| 	Float_t 					-> "float"
	| 	Void_t						-> "void"
	| 	Bool_t 						-> "bool"
	| 	Char_t 						-> "char"
	| 	Objecttype(s)				-> "class " ^ s
	| 	ConstructorType				-> "constructor"
	|  	Null_t 						-> "null"

let string_of_object = function
		Datatype(Objecttype(s))	-> s
	| 	_ -> ""

let rec print_brackets = function
		1 -> "[]"
	| 	a -> "[]" ^ print_brackets (a - 1)
		
let string_of_datatype = function 
		Arraytype(p, i)	-> (string_of_primitive p) ^ (print_brackets i)
	| 	Datatype(p)		-> (string_of_primitive p)
	|  	Any 			-> "Any"

(* Print expressions *)

let string_of_op = function
	   	Add			-> "+"	
	 | 	Sub			-> "-"	
	 | 	Mult		-> "*"	
	 | 	Div			-> "/"	
	 | 	Equal		-> "=="		
	 | 	Neq			-> "!="	
	 | 	Less		-> "<"	
	 | 	Leq			-> "<="	
	 | 	Greater		-> ">"			
	 | 	Geq			-> ">="	
	 | 	And			-> "and"	
	 | 	Not			-> "not"	
	 | 	Or			-> "or"
	 | 	Mod 		-> "%"

let rec string_of_bracket_expr = function
		[] 				-> ""
	| 	head :: tail 	-> "[" ^ (string_of_expr head) ^ "]" ^ (string_of_bracket_expr tail)
and string_of_array_primitive = function
		[] 				-> ""
	|   [last]			-> (string_of_expr last)
	| 	head :: tail 	-> (string_of_expr head) ^ ", " ^ (string_of_array_primitive tail)
and string_of_expr = function 
		Int_Lit(i)				-> string_of_int i
	|	Boolean_Lit(b)			-> if b then "true" else "false"
	|	Float_Lit(f)			-> string_of_float f
	|	String_Lit(s)			-> "\"" ^ (String.escaped s) ^ "\""
	|	Char_Lit(c)				-> Char.escaped c
	|	This					-> "this"
	|	Id(s)					-> s
	|	Binop(e1, o, e2)		-> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
	|	Assign(e1, e2)			-> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
	|	Noexpr					-> ""
	|	ObjAccess(e1, e2)		-> (string_of_expr e1) ^ "." ^ (string_of_expr e2)
	|	Call(f, el)				-> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	|	ArrayPrimitive(el)		-> "|" ^ (string_of_array_primitive el) ^ "|"
	|  	Unop(op, e)				-> (string_of_op op) ^ "(" ^ string_of_expr e ^ ")"
	|	Null					-> "null"
	|   ArrayCreate(d, el)  	-> "new " ^ string_of_datatype d ^ string_of_bracket_expr el
  	|   ArrayAccess(e, el)  	-> (string_of_expr e) ^ (string_of_bracket_expr el)
  	|   ObjectCreate(s, el) 	-> "new " ^ s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  	| 	Delete(e) 				-> "delete (" ^ (string_of_expr e) ^ ")"
;;

let rec string_of_bracket_sexpr = function
		[] 				-> ""
	| 	head :: tail 	-> "[" ^ (string_of_sexpr head) ^ "]" ^ (string_of_bracket_sexpr tail)
and string_of_sarray_primitive = function
		[] 				-> ""
	|   [last]			-> (string_of_sexpr last)
	| 	head :: tail 	-> (string_of_sexpr head) ^ ", " ^ (string_of_sarray_primitive tail)
and string_of_sexpr = function 
		SInt_Lit(i, _)				-> string_of_int i
	|	SBoolean_Lit(b, _)			-> if b then "true" else "false"
	|	SFloat_Lit(f, _)			-> string_of_float f
	|	SString_Lit(s, _)			-> "\"" ^ (String.escaped s) ^ "\""
	|	SChar_Lit(c, _)				-> Char.escaped c
	|	SId(s, _)					-> s
	|	SBinop(e1, o, e2, _)		-> (string_of_sexpr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_sexpr e2)
	|	SAssign(e1, e2, _)			-> (string_of_sexpr e1) ^ " = " ^ (string_of_sexpr e2)
	|	SNoexpr	_					-> ""
	|	SObjAccess(e1, e2, _)		-> (string_of_sexpr e1) ^ "." ^ (string_of_sexpr e2)
	|	SCall(f, el, _)				-> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
	|	SArrayPrimitive(el, _)		-> "|" ^ (string_of_sarray_primitive el) ^ "|"
	|  	SUnop(op, e, _)				-> (string_of_op op) ^ "(" ^ string_of_sexpr e ^ ")"
	|	SNull _						-> "null"
	|   SArrayCreate(d, el, _)  	-> "new " ^ string_of_datatype d ^ string_of_bracket_sexpr el
  	|   SArrayAccess(e, el, _)  	-> (string_of_sexpr e) ^ (string_of_bracket_sexpr el)
  	|   SObjectCreate(s, el, _) 	-> "new " ^ s ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  	| 	SDelete(e) 					-> "delete (" ^ (string_of_sexpr e) ^ ")"
;;

let string_of_local_expr = function
		Noexpr -> ""
	|  	e 	   -> " = " ^ string_of_expr e

(* Print statements *)

let rec string_of_stmt indent =
	let indent_string = String.make indent '\t' in
 	let get_stmt_string = function 

			Block(stmts) 			-> 
				indent_string ^ "{\n" ^ 
					String.concat "" (List.map (string_of_stmt (indent+1)) stmts) ^ 
				indent_string ^ "}\n"

		| 	Expr(expr) 				-> 
				indent_string ^ string_of_expr expr ^ ";\n";

		| 	Return(expr) 			-> 
				indent_string ^ "return " ^ string_of_expr expr ^ ";\n";

		| 	If(e, s, Block([Expr(Noexpr)])) 	-> 
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^ 
					(string_of_stmt (indent+1) s)

		| 	If(e, s1, s2) 			-> 
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^ 
					string_of_stmt (indent+1) s1 ^ 
				indent_string ^ "else\n" ^ 
					string_of_stmt (indent+1) s2

		| 	For(e1, e2, e3, s) 		-> 
				indent_string ^ "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ")\n" ^ 
					string_of_stmt (indent) s

		| 	While(e, s) 			-> 
				indent_string ^ "while (" ^ string_of_expr e ^ ")\n" ^ 
					string_of_stmt (indent) s

		|  	Break					-> indent_string ^ "break;\n"
		|  	Continue				-> indent_string ^ "continue;\n"
		|   Local(d, s, e) 			-> indent_string ^ string_of_datatype d ^ " " ^ s ^ string_of_local_expr e ^ ";\n"
	in get_stmt_string

let string_of_local_sexpr = function
		SNoexpr _ -> ""
	|  	e 	   -> " = " ^ string_of_sexpr e

let rec string_of_sstmt indent =
	let indent_string = String.make indent '\t' in
 	let get_stmt_string = function 

			SBlock(stmts) 			-> 
				indent_string ^ "{\n" ^ 
					String.concat "" (List.map (string_of_sstmt (indent+1)) stmts) ^ 
				indent_string ^ "}\n"

		| 	SExpr(expr, _) 				-> 
				indent_string ^ string_of_sexpr expr ^ ";\n";

		| 	SReturn(expr, _) 			-> 
				indent_string ^ "return " ^ string_of_sexpr expr ^ ";\n";

		| 	SIf(e, s, SBlock([SExpr(SNoexpr(_), _)])) 	-> 
				indent_string ^ "if (" ^ string_of_sexpr e ^ ")\n" ^ 
					(string_of_sstmt (indent+1) s)

		| 	SIf(e, s1, s2) 			-> 
				indent_string ^ "if (" ^ string_of_sexpr e ^ ")\n" ^ 
					string_of_sstmt (indent+1) s1 ^ 
				indent_string ^ "else\n" ^ 
					string_of_sstmt (indent+1) s2

		| 	SFor(e1, e2, e3, s) 		-> 
				indent_string ^ "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ")\n" ^ 
					string_of_sstmt (indent) s

		| 	SWhile(e, s) 			-> 
				indent_string ^ "while (" ^ string_of_sexpr e ^ ")\n" ^ 
					string_of_sstmt (indent) s

		|  	SBreak					-> indent_string ^ "break;\n"
		|  	SContinue				-> indent_string ^ "continue;\n"
		|   SLocal(d, s, e) 			-> indent_string ^ string_of_datatype d ^ " " ^ s ^ string_of_local_sexpr e ^ ";\n"
	in get_stmt_string

(* Print Function *)

let string_of_fname = function 
		Constructor -> "constructor"
	|	FName(s)	-> s

let string_of_formal = function
		Formal(d, s) -> (string_of_datatype d) ^ " " ^ s
	|  	_ 			 -> ""

let string_of_formal_name = function
		Formal(_, s) -> s
	| 	_ -> ""

let string_of_func_decl fdecl =
	"" ^ (string_of_scope fdecl.scope) ^ " " ^ (string_of_datatype fdecl.returnType) ^ " " ^ (string_of_fname fdecl.fname) ^ " " ^ 
	(* Formals *)
	"(" ^ String.concat "," (List.map string_of_formal fdecl.formals) ^ ") {\n" ^
		(* body *)
		String.concat "" (List.map (string_of_stmt 2) fdecl.body) ^
	"\t}\n\n"

(* Class Printing *)

let string_of_extends = function 
		NoParent	-> ""
	| 	Parent(s)	-> "extends " ^ s ^ " " 
let string_of_field = function 
	Field(s, d, id) -> (string_of_scope s) ^ " " ^ (string_of_datatype d) ^ " " ^ id ^ ";\n"

let string_of_cbody cbody = 
	String.concat "" (List.map (fun s -> "\t" ^ s) (List.map string_of_field cbody.fields)) ^
	String.concat "" (List.map (fun s -> "\t" ^ s) (List.map string_of_func_decl cbody.constructors)) ^
	String.concat "" (List.map (fun s -> "\t" ^ s) (List.map string_of_func_decl cbody.methods))

let string_of_class_decl cdecl = 
	"class " ^ cdecl.cname ^ " " ^ (string_of_extends cdecl.extends) ^ "{\n" ^
	(string_of_cbody cdecl.cbody) ^
	"}\n"

(* Include Printing *)

let rec string_of_include = function 
	Include(s) -> "include(" ^ s ^ ");\n"

(* Print whole program *)

let string_of_program = function
	Program(includes, cdecls) -> 
		String.concat "" (List.map string_of_include includes) ^ "\n" ^
		String.concat "\n" (List.map string_of_class_decl cdecls)

(* Print AST tree representation *)

let includes_tree includes = 
	`List (List.map (function Include s -> `String s) includes)

let map_fields_to_json fields = 
	`List (List.map (function Field(scope, datatype, s) -> 
		`Assoc [
			("name", `String s);
			("scope", `String (string_of_scope scope));
			("datatype", `String (string_of_datatype datatype));
		]) fields)

let map_formals_to_json formals = 
	`List (List.map (function Formal(d, s) -> `Assoc [
													("name", `String s);
													("datatype", `String (string_of_datatype d));
												]
							  | Many d -> `Assoc [("Many", `String (string_of_datatype d));]
		) formals)

let rec map_expr_to_json = function 
		Int_Lit(i)				-> `Assoc [("int_lit", `Int i)]
	|	Boolean_Lit(b)			-> `Assoc [("bool_lit", `Bool b)]
	|	Float_Lit(f)			-> `Assoc [("float_lit", `Float f)]
	|	String_Lit(s)			-> `Assoc [("string_lit", `String s)]
	|	Char_Lit(c)				-> `Assoc [("char_lit", `String (Char.escaped c))]
	|	This					-> `String "this"
	|	Id(s)					-> `Assoc [("id", `String s)]
	|	Binop(e1, o, e2)		-> `Assoc [("binop", `Assoc [("lhs", map_expr_to_json e1); ("op", `String (string_of_op o)); ("rhs", map_expr_to_json e2)])]
	|	Assign(e1, e2)			-> `Assoc [("assign", `Assoc [("lhs", map_expr_to_json e1); ("op", `String "="); ("rhs", map_expr_to_json e2)])]
	|	Noexpr					-> `String "noexpr"
	|	ObjAccess(e1, e2)		-> `Assoc [("objaccess", `Assoc [("lhs", map_expr_to_json e1); ("op", `String "."); ("rhs", map_expr_to_json e2)])]
	|	Call(f, el)				-> `Assoc [("call", `Assoc ([("name", `String f); ("params", `List (List.map map_expr_to_json el)); ]) )]
	|	ArrayPrimitive(el)		-> `Assoc [("arrayprimitive", `List(List.map map_expr_to_json el))]
	|  	Unop(op, e)				-> `Assoc [("Unop", `Assoc [("op", `String (string_of_op op)); ("operand", map_expr_to_json e)])]
	|	Null					-> `String "null"
	|   ArrayCreate(d, el)  	-> `Assoc [("arraycreate", `Assoc [("datatype", `String (string_of_datatype d)); ("args", `List (List.map map_expr_to_json el))])]
  	|   ArrayAccess(e, el)  	-> `Assoc [("arrayaccess", `Assoc [("array", map_expr_to_json e); ("args", `List (List.map map_expr_to_json el))])]
  	|   ObjectCreate(s, el) 	-> `Assoc [("objectcreate", `Assoc [("type", `String s); ("args", `List (List.map map_expr_to_json el))])]
  	| 	Delete(e) 				-> `Assoc [("delete", `Assoc [("expr", map_expr_to_json e)])]

let rec map_stmt_to_json = function
		Block(stmts) 			-> `Assoc [("block", `List (List.map (map_stmt_to_json) stmts))]
	| 	Expr(expr) 				-> `Assoc [("expr", map_expr_to_json expr)]
	| 	Return(expr) 			-> `Assoc [("return", map_expr_to_json expr)]
	| 	If(e, s1, s2) 			-> `Assoc [("if", `Assoc [("cond", map_expr_to_json e); ("ifbody", map_stmt_to_json s1)]); ("else", map_stmt_to_json s2)]
	| 	For(e1, e2, e3, s) 		-> `Assoc [("for", `Assoc [("init", map_expr_to_json e1); ("cond", map_expr_to_json e2); ("inc", map_expr_to_json e3); ("body", map_stmt_to_json s)])]
	| 	While(e, s) 			-> `Assoc [("while", `Assoc [("cond", map_expr_to_json e); ("body", map_stmt_to_json s)])]
	|  	Break					-> `String "break"
	|  	Continue				-> `String "continue"
	|   Local(d, s, e) 			-> `Assoc [("local", `Assoc [("datatype", `String (string_of_datatype d)); ("name", `String s); ("val", map_expr_to_json e)])]

let map_methods_to_json methods = 
	`List (List.map (fun (fdecl:Ast.func_decl) -> 
		`Assoc [
			("name", `String (string_of_fname fdecl.fname));
			("scope", `String (string_of_scope fdecl.scope));
			("returnType", `String (string_of_datatype fdecl.returnType));
			("formals", map_formals_to_json fdecl.formals);
			("body", `List (List.map (map_stmt_to_json) fdecl.body));
		]) methods)


let cdecls_tree cdecls =
	let map_cdecl_to_json cdecl = 
		`Assoc [
			("cname", `String cdecl.cname);
			("extends", `String (string_of_extends cdecl.extends));
			("fields", map_fields_to_json cdecl.cbody.fields);
			("methods", map_methods_to_json cdecl.cbody.methods);
			("constructors", map_methods_to_json cdecl.cbody.constructors)
		]
	in
	`List (List.map (map_cdecl_to_json) cdecls)

let print_tree = function
	Program(includes, cdecls) -> 
		`Assoc [("program", 
			`Assoc([
				("includes", includes_tree includes);
				("classes", cdecls_tree cdecls)
			])
		)]

(* Print SAST tree representation *)

let rec map_sexpr_to_json = 
	let datatype d = [("datatype", `String (string_of_datatype d))] in
	function
		SInt_Lit(i, d)          -> `Assoc [("int_lit", `Assoc ([("val", `Int i)] @ (datatype d)))]
	|   SBoolean_Lit(b, d)      -> `Assoc [("bool_lit", `Assoc ([("val", `Bool b)] @ (datatype d)))]
	|   SFloat_Lit(f, d)        -> `Assoc [("float_lit", `Assoc ([("val", `Float f)]  @ (datatype d)))]
	|   SString_Lit(s, d)       -> `Assoc [("string_lit", `Assoc ([("val", `String s)] @ (datatype d)))]
	|   SChar_Lit(c, d)         -> `Assoc [("char_lit", `Assoc ([("val", `String (Char.escaped c))] @ (datatype d)))]
	|   SId(s, d)               -> `Assoc [("id", `Assoc ([("name", `String s)] @ (datatype d)))]
	|   SBinop(e1, o, e2, d)    -> `Assoc [("binop", `Assoc ([("lhs", map_sexpr_to_json e1); ("op", `String (string_of_op o)); ("rhs", map_sexpr_to_json e2)] @ (datatype d)))]
	|   SAssign(e1, e2, d)      -> `Assoc [("assign", `Assoc ([("lhs", map_sexpr_to_json e1); ("op", `String "="); ("rhs", map_sexpr_to_json e2)] @ (datatype d)))]
	|   SNoexpr d               -> `Assoc [("noexpr", `Assoc (datatype d))]
	|   SArrayCreate(t, el, d)  -> `Assoc [("arraycreate", `Assoc ([("datatype", `String (string_of_datatype d)); ("args", `List (List.map map_sexpr_to_json el))] @ (datatype d)))]
	|   SArrayAccess(e, el, d)  -> `Assoc [("arrayaccess", `Assoc ([("array", map_sexpr_to_json e); ("args", `List (List.map map_sexpr_to_json el))] @ (datatype d)))]
	|   SObjAccess(e1, e2, d)   -> `Assoc [("objaccess", `Assoc ([("lhs", map_sexpr_to_json e1); ("op", `String "."); ("rhs", map_sexpr_to_json e2)] @ (datatype d)))]
	|   SCall(fname, el, d)     -> `Assoc [("call", `Assoc ([("name", `String fname); ("params", `List (List.map map_sexpr_to_json el)); ] @ (datatype d)) )]
	|   SObjectCreate(s, el, d) -> `Assoc [("objectcreate", `Assoc ([("type", `String s); ("args", `List (List.map map_sexpr_to_json el))] @ (datatype d)))]
	|   SArrayPrimitive(el, d)  -> `Assoc [("arrayprimitive", `Assoc ([("expressions", `List(List.map map_sexpr_to_json el))] @ (datatype d)))]
	|   SUnop(op, e, d)         -> `Assoc [("Unop", `Assoc ([("op", `String (string_of_op op)); ("operand", map_sexpr_to_json e)] @ (datatype d)))]
	|   SNull d                 -> `Assoc [("null", `Assoc (datatype d))] 
	| 	SDelete(e) 				-> `Assoc [("delete", `Assoc [("expr", map_sexpr_to_json e); ("type", `String "void")])]

let rec map_sstmt_to_json = 
	let datatype d = [("datatype", `String (string_of_datatype d))] in
	function
		SBlock sl        			-> `Assoc [("sblock", `List (List.map (map_sstmt_to_json) sl))]
	|   SExpr(e, d)          		-> `Assoc [("sexpr", `Assoc ([("expr", map_sexpr_to_json e)] @ (datatype d)))]
	|   SReturn(e, d)    			-> `Assoc [("sreturn", `Assoc ([("return", map_sexpr_to_json e)] @ (datatype d)))]
	|   SIf (e, s1, s2)       		-> `Assoc [("sif", `Assoc [("cond", map_sexpr_to_json e); ("ifbody", map_sstmt_to_json s1)]); ("selse", map_sstmt_to_json s2)]
	|   SFor (e1, e2, e3, s)  		-> `Assoc [("sfor", `Assoc [("init", map_sexpr_to_json e1); ("cond", map_sexpr_to_json e2); ("inc", map_sexpr_to_json e3); ("body", map_sstmt_to_json s)])]
	|   SWhile (e, s)    			-> `Assoc [("swhile", `Assoc [("cond", map_sexpr_to_json e); ("body", map_sstmt_to_json s)])]
	|   SBreak           			-> `String "sbreak"
	|   SContinue        			-> `String "scontinue"
	|   SLocal(d, s, e)  			-> `Assoc [("slocal", `Assoc [("datatype", `String (string_of_datatype d)); ("name", `String s); ("val", map_sexpr_to_json e)])]

let string_of_func_type = function
	User -> "user" | Reserved -> "reserved"

let map_sfdecl_to_json sfdecl =
	`Assoc[("sfdecl", `Assoc[
		("sfname", `String (string_of_fname sfdecl.sfname));
		("sreturnType", `String (string_of_datatype sfdecl.sreturnType));
		("sformals", map_formals_to_json sfdecl.sformals);
		("sbody", `List (List.map (map_sstmt_to_json) sfdecl.sbody));
		("func_type", `String(string_of_func_type sfdecl.func_type));
	])] 

let map_sfdecls_to_json sfdecls =
	`List(List.map map_sfdecl_to_json sfdecls)

let map_scdecls_to_json scdecls = 		
	`List(List.map (fun scdecl -> 
						`Assoc [("scdecl", 
							`Assoc[
								("scname", `String scdecl.scname); 
								("sfields", map_fields_to_json scdecl.sfields);
							])
						]) 
		scdecls)

let map_sprogram_to_json sprogram = 
	`Assoc [("sprogram", `Assoc [
		("classes", map_scdecls_to_json sprogram.classes);
		("functions", map_sfdecls_to_json sprogram.functions);
		("main", map_sfdecl_to_json sprogram.main);
		("reserved", map_sfdecls_to_json sprogram.reserved);
	])]

(* Print tokens *)

let string_of_token = function
		LPAREN				-> "LPAREN"	
	| 	RPAREN				-> "RPAREN"	
	| 	LBRACE				-> "LBRACE"	
	| 	RBRACE				-> "RBRACE"	
	| 	SEMI				-> "SEMI"	
	| 	COMMA				-> "COMMA"	
	| 	PLUS				-> "PLUS"	
	| 	MINUS				-> "MINUS"	
	| 	TIMES				-> "TIMES"	
	| 	DIVIDE				-> "DIVIDE"	
	| 	ASSIGN				-> "ASSIGN"	
	| 	EQ					-> "EQ"
	| 	NEQ					-> "NEQ"
	| 	LT					-> "LT"
	| 	LEQ					-> "LEQ"
	| 	GT					-> "GT"
	| 	GEQ					-> "GEQ"
	| 	AND					-> "AND"
	| 	OR					-> "OR"
	| 	NOT					-> "NOT"
	| 	DOT					-> "DOT"
	| 	LBRACKET			-> "LBRACKET"		
	| 	RBRACKET			-> "RBRACKET"		
	| 	BAR					-> "BAR"
	| 	IF					-> "IF"
	| 	ELSE				-> "ELSE"	
	| 	FOR					-> "FOR"
	| 	WHILE				-> "WHILE"	
	| 	RETURN				-> "RETURN"	
	| 	INT					-> "INT"
	| 	FLOAT				-> "FLOAT"	
	| 	BOOL				-> "BOOL"	
	| 	CHAR				-> "CHAR"	
	| 	VOID				-> "VOID"	
	| 	NULL				-> "NULL"	
	| 	TRUE				-> "TRUE"	
	| 	FALSE				-> "FALSE"	
	| 	CLASS				-> "CLASS"	
	| 	CONSTRUCTOR			-> "CONSTRUCTOR"		
	| 	PUBLIC				-> "PUBLIC"	
	| 	PRIVATE				-> "PRIVATE"	
	| 	EXTENDS				-> "EXTENDS"	
	| 	INCLUDE				-> "INCLUDE"	
	| 	THIS				-> "THIS"	
	| 	BREAK				-> "BREAK"	
	| 	CONTINUE			-> "CONTINUE"	
	|   NEW 				-> "NEW"	
	| 	INT_LITERAL(i)		-> "INT_LITERAL(" ^ string_of_int i ^ ")"
	| 	FLOAT_LITERAL(f)	-> "FLOAT_LITERAL(" ^ string_of_float f ^ ")"
	| 	CHAR_LITERAL(c)		-> "CHAR_LITERAL(" ^ Char.escaped c ^ ")"
	| 	STRING_LITERAL(s)	-> "STRING_LITERAL(" ^ s ^ ")"
	| 	ID(s)				-> "ID(" ^ s ^ ")"
	| 	DELETE 				-> "DELETE"
	| 	MODULO 				-> "MODULO"
	|  	EOF					-> "EOF"

let string_of_token_no_id = function
		LPAREN				-> "LPAREN"	
	| 	RPAREN				-> "RPAREN"	
	| 	LBRACE				-> "LBRACE"	
	| 	RBRACE				-> "RBRACE"	
	| 	SEMI				-> "SEMI"	
	| 	COMMA				-> "COMMA"	
	| 	PLUS				-> "PLUS"	
	| 	MINUS				-> "MINUS"	
	| 	TIMES				-> "TIMES"	
	| 	DIVIDE				-> "DIVIDE"	
	| 	ASSIGN				-> "ASSIGN"	
	| 	EQ					-> "EQ"
	| 	NEQ					-> "NEQ"
	| 	LT					-> "LT"
	| 	LEQ					-> "LEQ"
	| 	GT					-> "GT"
	| 	GEQ					-> "GEQ"
	| 	AND					-> "AND"
	| 	OR					-> "OR"
	| 	NOT					-> "NOT"
	| 	DOT					-> "DOT"
	| 	LBRACKET			-> "LBRACKET"		
	| 	RBRACKET			-> "RBRACKET"		
	| 	BAR					-> "BAR"
	| 	IF					-> "IF"
	| 	ELSE				-> "ELSE"	
	| 	FOR					-> "FOR"
	| 	WHILE				-> "WHILE"	
	| 	RETURN				-> "RETURN"	
	| 	INT					-> "INT"
	| 	FLOAT				-> "FLOAT"	
	| 	BOOL				-> "BOOL"	
	| 	CHAR				-> "CHAR"	
	| 	VOID				-> "VOID"	
	| 	NULL				-> "NULL"	
	| 	TRUE				-> "TRUE"	
	| 	FALSE				-> "FALSE"	
	| 	CLASS				-> "CLASS"	
	| 	CONSTRUCTOR			-> "CONSTRUCTOR"		
	| 	PUBLIC				-> "PUBLIC"	
	| 	PRIVATE				-> "PRIVATE"	
	| 	EXTENDS				-> "EXTENDS"	
	| 	INCLUDE				-> "INCLUDE"	
	| 	THIS				-> "THIS"	
	| 	BREAK				-> "BREAK"	
	| 	CONTINUE			-> "CONTINUE"	
	|   NEW 				-> "NEW"		
	| 	INT_LITERAL(i)		-> "INT_LITERAL"
	| 	FLOAT_LITERAL(f)	-> "FLOAT_LITERAL"
	| 	CHAR_LITERAL(c)		-> "CHAR_LITERAL"
	| 	STRING_LITERAL(s)	-> "STRING_LITERAL"
	| 	ID(s)				-> "ID"
	| 	DELETE 				-> "DELETE"
	| 	MODULO 				-> "MODULO"
	|  	EOF					-> "EOF"

let token_list_to_string_endl token_list =
  let rec helper last_line_number = function
    	(token, curr)::tail ->
    	let line = curr.lineno in 
        (if line != last_line_number then "\n" ^ string_of_int line ^ ". " else " ") ^
        string_of_token token ^ helper line tail
    | 	[] -> "\n"
  in helper 0 token_list

let token_list_to_string token_list =
  let rec helper = function
    	(token, line)::tail ->
        string_of_token_no_id token ^ " " ^ helper tail
    | 	[] -> "\n"
  in helper token_list