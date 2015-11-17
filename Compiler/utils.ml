(* Pretty Printer *)
open Ast
open Parser
open Processor

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
	| 	ConstructorType				-> ""

let rec print_brackets = function
		1 -> "[]"
	| 	a -> "[]" ^ print_brackets (a - 1)
		
let string_of_datatype = function 
		Arraytype(p, i)	-> (string_of_primitive p) ^ (print_brackets i)
	| 	Datatype(p)		-> (string_of_primitive p)

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
	|	String_Lit(s)			-> "\"" ^ s ^ "\""
	|	Char_Lit(c)				-> Char.escaped c
	|	This					-> "this"
	|	Id(s)					-> s
	|	Binop(e1, o, e2)		-> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
	|	Assign(e1, e2)			-> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
	|	Noexpr					-> ""
	|	ObjAccess(e1, e2)		-> (string_of_expr e1) ^ "." ^ (string_of_expr e2)
	|	Call(f, el)				-> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	|	ArrayPrimitive(el)		-> "|" ^ (string_of_array_primitive el) ^ "|"
	|  	UMinus e1				-> "-" ^ string_of_expr e1
	|	Null					-> "null"
	|   ArrayCreate(d, el)  	-> "new " ^ string_of_datatype d ^ string_of_bracket_expr el
  	|   ArrayAccess(e, el)  	-> (string_of_expr e) ^ (string_of_bracket_expr el)
  	|   ObjectCreate(s, el) 	-> "new " ^ s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
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

		| 	If(e, s, Block([])) 	-> 
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^ 
					(string_of_stmt (indent+1) s)

		| 	If(e, s1, s2) 			-> 
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^ 
					string_of_stmt (indent+1) s1 ^ 
				indent_string ^ "else\n" ^ 
					string_of_stmt (indent+1) s2

		| 	For(e1, e2, e3, s) 		-> 
				indent_string ^ "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") " ^ 
					string_of_stmt (indent+1) s

		| 	While(e, s) 			-> 
				indent_string ^ "while (" ^ string_of_expr e ^ ") " ^ 
					string_of_stmt (indent+1) s

		|  	Break					-> indent_string ^ "break;\n"
		|  	Continue				-> indent_string ^ "continue;\n"
		|   Local(d, s, e) 			-> indent_string ^ string_of_datatype d ^ " " ^ s ^ string_of_local_expr e ^ ";\n"
	in get_stmt_string

(* Print Function *)

let string_of_fname = function 
		Constructor -> "constructor"
	|	FName(s)	-> s

let string_of_formal = function
	Formal(d, s) -> (string_of_datatype d) ^ " " ^ s

let string_of_func_decl fdecl =
	"\t" ^ (string_of_scope fdecl.scope) ^ " " ^ (string_of_datatype fdecl.returnType) ^ " " ^ (string_of_fname fdecl.fname) ^ " " ^ 
	(* Formals *)
	"(" ^ String.concat "," (List.map string_of_formal fdecl.formals) ^ ") {\n" ^
		(* body *)
		String.concat "" (List.map (string_of_stmt 2) fdecl.body) ^
	"\t}\n"

(* Class Printing *)

let string_of_extends = function 
		NoParent	-> ""
	| 	Parent(s)	-> "extends " ^ s ^ " " 
let string_of_field = function 
	Field(s, d, id) -> (string_of_scope s) ^ " " ^ (string_of_datatype d) ^ " " ^ id ^ ";\n"

let string_of_cbody cbody = 
	"\t" ^
	String.concat "\t" (List.map string_of_field cbody.fields) ^
	String.concat "\t" (List.map string_of_func_decl cbody.constructors) ^
	String.concat "\t" (List.map string_of_func_decl cbody.methods)

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


(* Print tree representation *)

let cbody_tree cbody = 
(* 	let indent_string = String.make 4 '\t' in
 *)	"\n" (* TODO: ^
	String.concat indent_string (List.map fields_tree cbody.fields) ^
	String.concat indent_string (List.map string_of_func_decl cbody.constructors) ^
	String.concat indent_string (List.map string_of_func_decl cbody.methods) *)

let rec includes_tree = function 
		[] -> ""
	|  	[Include s] -> "\ninclude(" ^ s ^ "),\n\t"
	|  	Include s :: tail -> "\ninclude(" ^ s ^ ")," ^ includes_tree tail

let rec cdecls_tree = function 
		[] -> ""
	|  	[cdecl] -> 	"\n\t\tclass " ^ cdecl.cname ^ "\n" ^ 
			"\t\t\textends: " ^ (string_of_extends cdecl.extends) ^ "\n" ^
			"\t\t\tcbody:" ^ (cbody_tree cdecl.cbody) ^
	"\n\t"
	|  	cdecl::tail -> 
		"\n\t\tclass " ^ cdecl.cname ^ "\n" ^ 
			"\t\t\textends: " ^ (string_of_extends cdecl.extends) ^ "\n" ^
			"\t\t\tcbody:" ^ (cbody_tree cdecl.cbody)



let print_tree = function
	Program(includes, cdecls) -> 
		"[program:\n" ^
			"\t[includes:" ^ includes_tree includes ^ "]\n" ^
			"\t[cdecls:" ^ cdecls_tree cdecls ^ "]\n" ^
		"]\n"

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