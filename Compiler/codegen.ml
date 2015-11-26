(* ===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm
(* Change to Sast *)
open Ast
open Sast

exception Error of string

let context = global_context ()
let the_module = create_module context "Dice Codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let i32_t = i32_type context;;
let i8_t = i8_type context;;
let f_t = float_type context;;

(* Need to add logic for fmul and fdiv *)

let rec handle_binop e1 op e2 index llbuilder = 
	let e1 = codegen_sexpr (index+1) llbuilder e1 in
	let e2 = codegen_sexpr (index+2) llbuilder e2 in
	match op with
			Add 		-> build_add e1 e2 (string_of_int index) llbuilder
	| 	Sub 		-> build_sub e1 e2 (string_of_int index) llbuilder
	| 	Mult 		-> build_mul e1 e2 (string_of_int index) llbuilder
	| 	Div 		-> build_fdiv e1 e2 (string_of_int index) llbuilder
	| 	Equal 	-> build_sub e1 e2 (string_of_int index) llbuilder
	| 	Neq 		-> build_global_stringptr "Hi" "" llbuilder
	| 	Less 		-> build_global_stringptr "Hi" "" llbuilder
	| 	Leq 		-> build_global_stringptr "Hi" "" llbuilder
	| 	Greater -> build_global_stringptr "Hi" "" llbuilder
	| 	Geq 		-> build_global_stringptr "Hi" "" llbuilder
	| 	And 		-> build_and e1 e2 (string_of_int index) llbuilder
	| 	Or 			-> build_or  e1 e2 (string_of_int index) llbuilder
	| 	_ 			-> build_global_stringptr "Hi" "" llbuilder (* Will not happen *)

and codegen_sexpr index llbuilder = function
			SInt_Lit(i, d)            -> const_int i32_t i
	|   SBoolean_Lit(b, d)        -> if b then const_int i32_t 1 else const_int i32_t 0
	|   SFloat_Lit(f, d)          -> const_float f_t f 
	|   SString_Lit(s, d)         -> build_global_stringptr s "" llbuilder
	|   SChar_Lit(c, d)           -> const_int i32_t (Char.code c)
	|   SId(id, d)                -> build_global_stringptr "Hi" "" llbuilder
	|   SBinop(e1, op, e2, d)     -> handle_binop e1 op e2 index llbuilder
	|   SAssign(e1, e2, d)        -> build_global_stringptr "Hi" "" llbuilder
	|   SNoexpr d                 -> build_global_stringptr "Hi" "" llbuilder
	|   SArrayCreate(t, el, d)    -> build_global_stringptr "Hi" "" llbuilder
	|   SArrayAccess(e, el, d)    -> build_global_stringptr "Hi" "" llbuilder
	|   SObjAccess(e1, e2, d)     -> build_global_stringptr "Hi" "" llbuilder
	|   SCall(fname, el, d)       ->  (function
																				"print" -> 
																					let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
																					let printf = declare_function "printf" printf_ty the_module in
																					let s = codegen_sexpr (index+1) llbuilder (List.hd el) in
																					let zero = const_int i32_t 0 in
																					let s = build_in_bounds_gep s [| zero |] "" llbuilder in
																					build_call printf [| s |] "" llbuilder
																			| _ -> build_global_stringptr "Hi" "" llbuilder) fname
	|   SObjectCreate(id, el, d)  -> build_global_stringptr "Hi" "" llbuilder
	|   SArrayPrimitive(el, d)    -> build_global_stringptr "Hi" "" llbuilder
	|   SUnop(op, e, d)           -> build_global_stringptr "Hi" "" llbuilder
	|   SNull d                   -> build_global_stringptr "Hi" "" llbuilder

let rec codegen_stmt index llbuilder = function
			SBlock sl        			-> List.fold_left (fun i s -> ignore(codegen_stmt (i+1) llbuilder s); (i+1)) index sl
	|   SExpr(e, d)          	-> ignore(codegen_sexpr index llbuilder e); index+1
	|   SReturn(e, d)    			-> ignore(build_ret (codegen_sexpr index llbuilder e) llbuilder ); index+1
	|   SIf (e, s1, s2)       -> index+1
	|   SFor (e1, e2, e3 ,s)  -> index+1
	|   SWhile (e, s)    			-> index+1
	|   SBreak           			-> index+1    
	|   SContinue        			-> index+1
	|   SLocal(d, s, e)  			-> index+1

let codegen_func fdecl = 
		let handle_func = function
			_ -> build_global_stringptr "Hi" "" builder 
		in 
		handle_func fdecl.sfname

let codegen_library_functions = ()

let codegen_struct s =
	named_struct_type context s.scname

let codegen_main main = 
	let fty = function_type i32_t [| |] in
	let f = define_function "main" fty the_module in
	let llbuilder = builder_at_end context (entry_block f) in
	let _ = codegen_stmt 0 llbuilder (List.hd main.sbody) in
	build_ret (const_int i32_t 0) llbuilder 

let codegen_sprogram sprogram = match sprogram with 
	SProgram(structs, functions, main) -> 
	let _ = codegen_library_functions in
	(* let _ = List.map (fun f -> codegen_func f) functions in *)
	let _ = List.map (fun s -> codegen_struct s) structs in
	let _ = codegen_main main in
		the_module