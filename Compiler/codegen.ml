(* ===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm
open Ast
open Sast
open Analyzer
open Exceptions
open Batteries
open Hashtbl

exception Error of string

let context = global_context ()
let the_module = create_module context "Dice Codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 50

let i32_t = i32_type context;;
let i8_t = i8_type context;;
let f_t = double_type context;;
let i1_t = i1_type context;;
let str_t = pointer_type i8_t;;

let str_type = Arraytype(Char_t, 1)
let noop = SNoexpr(Datatype(Int_t))

let get_type datatype = match datatype with 
		Datatype(Int_t) -> i32_t
	| 	Datatype(Float_t) -> f_t
	| 	Datatype(Bool_t) -> i1_t
	| 	Datatype(Char_t) -> i8_t
	|  	Arraytype(Char_t, 1) -> str_t
	| 	_ -> i32_t (* WRONG *)



(* cast will return an llvalue of the desired type *)
(* The commented out casts are unsupported actions in Dice *)
let cast lhs rhs lhsType rhsType = match (lhsType, rhsType) with
		(* int to,__ ) ( using const_sitofp for signed ints *)
		(Datatype(Int_t), Datatype(Int_t))				-> (lhs, rhs), Datatype(Int_t)
	| 	(Datatype(Int_t), Datatype(Char_t))				-> (const_uitofp lhs i8_t, rhs), Datatype(Char_t)
	(* |   	(Datatype(Int_t), Datatype(Bool_t))				-> (lhs, const_zext rhs i32_t) *)
	|   (Datatype(Int_t), Datatype(Float_t)) 			-> (const_sitofp lhs f_t, rhs), Datatype(Float_t)

		(* char to,__)  ( using uitofp since char isn't signed *)
	|   (Datatype(Char_t), Datatype(Int_t)) 			-> (lhs, const_uitofp rhs i8_t), Datatype(Char_t)
	|   (Datatype(Char_t), Datatype(Char_t)) 			-> (lhs, rhs), Datatype(Char_t)
	(* | 	(Datatype(Char_t), Datatype(Bool_t))			-> (lhs, const_zext rhs i8_t) *)
	(* | 	(Datatype(Char_t), Datatype(Float_t))			-> (const_uitofp lhs f_t, rhs) *)

		(* bool to,__)  ( zext fills the empty bits with zeros, zero extension *)
	(* |   	(Datatype(Bool_t), Datatype(Int_t)) 			-> (const_zext lhs i32_t, rhs) *)
	(* | 	(Datatype(Bool_t), Datatype(Char_t))			-> (const_zext lhs i8_t, rhs) *)
	(* |   	(Datatype(Bool_t), Datatype(Bool_t))			-> (lhs, rhs) *)
	(* |   	(Datatype(Bool_t), Datatype(Float_t))			-> (const_uitofp lhs f_t, rhs) *)

		(* float to,__) ( using fptosi for signed ints *)
	|   (Datatype(Float_t), Datatype(Int_t)) 			-> (lhs, const_sitofp rhs f_t), Datatype(Float_t)
	(* | 	(Datatype(Float_t), Datatype(Char_t))			-> (lhs, const_uitofp rhs f_t) *)
	(* |   	(Datatype(Float_t), Datatype(Bool_t))			-> (lhs, const_uitofp rhs f_t) *)
	|   (Datatype(Float_t), Datatype(Float_t)) 			-> (lhs, rhs), Datatype(Float_t)

	| 	_ 												-> raise Exceptions.CannotCastTypeException

let rec handle_binop e1 op e2 d llbuilder =
	(* Get the types of e1 and e2 *) 
	let type1 = Analyzer.get_type_from_sexpr e1 in
	let type2 = Analyzer.get_type_from_sexpr e2 in

	(* Generate llvalues from e1 and e2 *)

	let e1 = codegen_sexpr llbuilder e1 in
	let e2 = codegen_sexpr llbuilder e2 in

	let float_ops op e1 e2 =
	match op with
		Add 		-> build_fadd e1 e2 "flt_addtmp" llbuilder
	| 	Sub 		-> build_fsub e1 e2 "flt_subtmp" llbuilder
	| 	Mult 		-> build_fmul e1 e2 "flt_multmp" llbuilder
	| 	Div 		-> build_fdiv e1 e2 "flt_divtmp" llbuilder
	| 	Equal 		-> build_fcmp Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
	| 	Neq 		-> build_fcmp Fcmp.One e1 e2 "flt_neqtmp" llbuilder
	| 	Less 		-> build_fcmp Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
	| 	Leq 		-> build_fcmp Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
	| 	Greater		-> build_fcmp Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
	| 	Geq 		-> build_fcmp Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
	| 	_ 			-> raise Exceptions.FloatOpNotSupported 

	in 

	(* chars are considered ints, so they will use int_ops as well*)
	let int_ops op e1 e2 = 
	match op with
		Add 		-> build_add e1 e2 "addtmp" llbuilder
	| 	Sub 		-> build_sub e1 e2 "subtmp" llbuilder
	| 	Mult 		-> build_mul e1 e2 "multmp" llbuilder
	| 	Div 		-> build_sdiv e1 e2 "divtmp" llbuilder
	| 	Equal 		-> build_icmp Icmp.Eq e1 e2 "eqtmp" llbuilder
	| 	Neq 		-> build_icmp Icmp.Ne e1 e2 "neqtmp" llbuilder
	| 	Less 		-> build_icmp Icmp.Slt e1 e2 "lesstmp" llbuilder
	| 	Leq 		-> build_icmp Icmp.Sle e1 e2 "leqtmp" llbuilder
	| 	Greater		-> build_icmp Icmp.Sgt e1 e2 "sgttmp" llbuilder
	| 	Geq 		-> build_icmp Icmp.Sge e1 e2 "sgetmp" llbuilder
	| 	And 		-> build_and e1 e2 "andtmp" llbuilder
	| 	Or 			-> build_or  e1 e2 "ortmp" llbuilder
	| 	_ 			-> raise Exceptions.IntOpNotSupported 
	in 
	
	let (e1, e2), d = cast e1 e2 type1 type2 in

	let type_handler d = match d with
			Datatype(Float_t)   -> float_ops op e1 e2
		|	Datatype(Int_t)	
		|   Datatype(Bool_t)
		| 	Datatype(Char_t) 	-> int_ops op e1 e2
		|   _ -> raise Exceptions.InvalidBinopEvaluationType
	in

	type_handler d
 
and handle_unop op e d llbuilder =
	(* Get the type of e *) 
	let eType = Analyzer.get_type_from_sexpr e in
	(* Get llvalue  *)
	let e = codegen_sexpr llbuilder e in

	let unops op eType e = match (op, eType) with
		(Sub, Datatype(Int_t)) 		->  build_neg e "int_unoptmp" llbuilder
	|   (Sub, Datatype(Float_t)) 	-> 	build_fneg e "flt_unoptmp" llbuilder
	|   (Not, Datatype(Bool_t)) 	->  build_not e "bool_unoptmp" llbuilder
	|    _ 	 -> raise Exceptions.UnopNotSupported	in

	let unop_type_handler d = match d with
				Datatype(Float_t)   -> unops op eType e
			|	Datatype(Int_t)		-> unops op eType e
			|   Datatype(Bool_t)	-> unops op eType e
			|   _ -> raise Exceptions.InvalidUnopEvaluationType
		in
		
		unop_type_handler d
 
and codegen_print llbuilder el = 
	let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
	let printf = declare_function "printf" printf_ty the_module in
	let tmp_count = ref 0 in
	let incr_tmp = fun x -> incr tmp_count in

	let map_expr_to_printfexpr expr = 
		let exprType = Analyzer.get_type_from_sexpr expr in
		match exprType with 
		Datatype(Bool_t) ->
			incr_tmp ();
			let tmp_var = "tmp" ^ (string_of_int !tmp_count) in
			let trueStr = SString_Lit("true", str_type) in
			let falseStr = SString_Lit("false", str_type) in
			let id = SId(tmp_var, str_type) in 
			ignore(codegen_stmt llbuilder (SLocal(str_type, tmp_var, noop)));
			ignore(codegen_stmt llbuilder (SIf(expr, 
											SExpr(SAssign(id, trueStr, str_type), str_type), 
											SExpr(SAssign(id, falseStr, str_type), str_type)
										)));
			codegen_sexpr llbuilder id
		| _ -> codegen_sexpr llbuilder expr
	in

	let params = List.map map_expr_to_printfexpr el in
	let param_types = List.map (Analyzer.get_type_from_sexpr) el in 

	let map_param_to_string = function 
			Arraytype(Char_t, 1) 	-> "%s"
		| 	Datatype(Int_t) 		-> "%d"
		| 	Datatype(Float_t) 		-> "%f"
		| 	Datatype(Bool_t) 		-> "%s"
		| 	Datatype(Char_t) 		-> "%c"
		| 	_ 						-> raise (Exceptions.InvalidTypePassedToPrintf)
	in 
	let const_str = List.fold_left (fun s t -> s ^ map_param_to_string t) "" param_types in
	let s = codegen_sexpr llbuilder (SString_Lit(const_str, Arraytype(Char_t, 1))) in
	let zero = const_int i32_t 0 in 
	let s = build_in_bounds_gep s [| zero |] "" llbuilder in
	build_call printf (Array.of_list (s :: params)) "" llbuilder

and codegen_func_call llbuilder el = function
		"print" -> codegen_print llbuilder el
	| 	_ -> build_global_stringptr "Hi" "" llbuilder

and codegen_id id llbuilder = 
	let v = try Hashtbl.find named_values id with
		| Not_found -> raise Exceptions.UnknownVariable
	in
	(* Load the value. *)
	build_load v id llbuilder

and codegen_assign lhs rhs llbuilder = 
	(* Special case '=' because we don't want to emit the LHS as an
	* expression. *)
	let name =
		match lhs with
		| Sast.SId(id, d) -> id
		| _ -> raise Exceptions.AssignLHSMustBeAssignable
	in

	(* Codegen the rhs. *)
	let val_ = codegen_sexpr llbuilder rhs in

	(* Lookup the name. *)
	let variable = try Hashtbl.find named_values name with
	| Not_found -> raise (Error "unknown variable name")
	in
	ignore(build_store val_ variable llbuilder);
	val_

and codegen_sexpr llbuilder = function
		SInt_Lit(i, d)            -> const_int i32_t i
	|   SBoolean_Lit(b, d)        -> if b then const_int i1_t 1 else const_int i1_t 0
	|   SFloat_Lit(f, d)          -> const_float f_t f 
	|   SString_Lit(s, d)         -> build_global_stringptr s "" llbuilder
	|   SChar_Lit(c, d)           -> const_int i32_t (Char.code c)
	|   SId(id, d)                -> codegen_id id llbuilder
	|   SBinop(e1, op, e2, d)     -> handle_binop e1 op e2 d llbuilder
	|   SAssign(e1, e2, d)        -> codegen_assign e1 e2 llbuilder
	|   SNoexpr d                 -> build_add (const_int i32_t 0) (const_int i32_t 0) "nop" llbuilder
	|   SArrayCreate(t, el, d)    -> build_global_stringptr "Hi" "" llbuilder
	|   SArrayAccess(e, el, d)    -> build_global_stringptr "Hi" "" llbuilder
	|   SObjAccess(e1, e2, d)     -> build_global_stringptr "Hi" "" llbuilder
	|   SCall(fname, el, d)       -> codegen_func_call llbuilder el fname		
	|   SObjectCreate(id, el, d)  -> build_global_stringptr "Hi" "" llbuilder
	|   SArrayPrimitive(el, d)    -> build_global_stringptr "Hi" "" llbuilder
	|   SUnop(op, e, d)           -> handle_unop op e d llbuilder
	|   SNull d                   -> build_global_stringptr "Hi" "" llbuilder

and codegen_if_stmt exp then_ (else_:Sast.sstmt) llbuilder =
	let cond_val = codegen_sexpr llbuilder exp in

	(* Grab the first block so that we might later add the conditional branch
	 * to it at the end of the function. *)
	let start_bb = insertion_block llbuilder in
	let the_function = block_parent start_bb in

	let then_bb = append_block context "then" the_function in

	(* Emit 'then' value. *)
	position_at_end then_bb llbuilder;
	let then_val = codegen_stmt llbuilder then_ in

	(* Codegen of 'then' can change the current block, update then_bb for the
	 * phi. We create a new name because one is used for the phi node, and the
	 * other is used for the conditional branch. *)
	let new_then_bb = insertion_block llbuilder in

	(* Emit 'else' value. *)
	let else_bb = append_block context "else" the_function in
	position_at_end else_bb llbuilder;
	let else_val = codegen_stmt llbuilder else_ in

	(* Codegen of 'else' can change the current block, update else_bb for the
	 * phi. *)
	let new_else_bb = insertion_block llbuilder in

	(* Emit merge block. *)
	let merge_bb = append_block context "ifcont" the_function in
	position_at_end merge_bb llbuilder;
	let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
	let phi = build_phi incoming "iftmp" llbuilder in

	(* Return to the start block to add the conditional branch. *)
	position_at_end start_bb llbuilder;
	ignore (build_cond_br cond_val then_bb else_bb llbuilder);

	(* Set a unconditional branch at the end of the 'then' block and the
	 * 'else' block to the 'merge' block. *)
	position_at_end new_then_bb llbuilder; ignore (build_br merge_bb llbuilder);
	position_at_end new_else_bb llbuilder; ignore (build_br merge_bb llbuilder);

	(* Finally, set the builder to the end of the merge block. *)
	position_at_end merge_bb llbuilder;

	phi

and codegen_for init_ cond_ inc_ body_ llbuilder = 

	let the_function = block_parent (insertion_block llbuilder) in

	(* Emit the start code first, without 'variable' in scope. *)
	let _ = codegen_sexpr llbuilder init_ in

	(* Make the new basic block for the loop header, inserting after current
	* block. *)
	let loop_bb = append_block context "loop" the_function in

	(* Insert an explicit fall through from the current block to the
	* loop_bb. *)
	ignore (build_br loop_bb llbuilder);

	(* Start insertion in loop_bb. *)
	position_at_end loop_bb llbuilder;

	(* Emit the body of the loop.  This, like any other expr, can change the
	* current BB.  Note that we ignore the value computed by the body, but
	* don't allow an error *)
	ignore (codegen_stmt llbuilder body_);

	(* Emit the step value. *)
	let _ = codegen_sexpr llbuilder inc_ in

	(* Compute the end condition. *)
	let cond_val = codegen_sexpr llbuilder cond_ in

	(* Create the "after loop" block and insert it. *)
	let after_bb = append_block context "afterloop" the_function in

	(* Insert the conditional branch into the end of loop_end_bb. *)
	ignore (build_cond_br cond_val loop_bb after_bb llbuilder);

	(* Any new code will be inserted in after_bb. *)
	position_at_end after_bb llbuilder;

	(* for expr always returns 0.0. *)
	const_null f_t

and codegen_alloca datatype var_name expr llbuilder = 
	match expr with 
	SNoexpr(_) -> 
		let alloca = build_alloca (get_type datatype) var_name llbuilder in
		Hashtbl.add named_values var_name alloca;
		alloca
	| _ -> 
		let init_val = codegen_sexpr llbuilder expr in
		let alloca = build_alloca (get_type datatype) var_name llbuilder in
		ignore(build_store init_val alloca llbuilder);
		Hashtbl.add named_values var_name alloca;
		init_val

and codegen_stmt llbuilder = function
		SBlock sl        			-> List.hd(List.map (codegen_stmt llbuilder) sl)	
	|   SExpr(e, d)          		-> codegen_sexpr llbuilder e
	|   SReturn(e, d)    			-> build_ret (codegen_sexpr llbuilder e) llbuilder
	|   SIf (e, s1, s2)       		-> codegen_if_stmt e s1 s2 llbuilder
	|   SFor (e1, e2, e3, s)  		-> codegen_for e1 e2 e3 s llbuilder
	|   SWhile (e, s)    			-> build_global_stringptr "Hi" "" llbuilder
	|   SBreak           			-> build_global_stringptr "Hi" "" llbuilder   
	|   SContinue        			-> build_global_stringptr "Hi" "" llbuilder
	|   SLocal(d, s, e)  			-> codegen_alloca d s e llbuilder

let codegen_func sfdecl = 
	Hashtbl.clear named_values;
	let fty = function_type (get_type sfdecl.sreturnType) [| |] in
	let f = define_function (Utils.string_of_fname sfdecl.sfname) fty the_module in
	let llbuilder = builder_at_end context (entry_block f) in
	let _ = codegen_stmt llbuilder (SBlock (sfdecl.sbody)) in
	build_ret_void llbuilder 

let codegen_library_functions = ()

let codegen_struct s =
	named_struct_type context s.scname

let codegen_main main = 
	Hashtbl.clear named_values;
	let fty = function_type i32_t [| |] in
	let f = define_function "main" fty the_module in
	let llbuilder = builder_at_end context (entry_block f) in
	let _ = codegen_stmt llbuilder (SBlock (main.sbody)) in
	build_ret (const_int i32_t 0) llbuilder 

let codegen_sprogram sprogram = 
	let _ = codegen_library_functions in
	let _ = List.map (fun f -> codegen_func f) sprogram.functions in
	let _ = List.map (fun s -> codegen_struct s) sprogram.classes in
	let _ = codegen_main sprogram.main in
	the_module