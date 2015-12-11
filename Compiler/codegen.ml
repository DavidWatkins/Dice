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

let context = global_context ()
let the_module = create_module context "Dice Codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 50
let named_params:(string, llvalue) Hashtbl.t = Hashtbl.create 50
let struct_types:(string, lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create 50

let i32_t = i32_type context;;
let i8_t = i8_type context;;
let f_t = double_type context;;
let i1_t = i1_type context;;
let str_t = pointer_type i8_t;;
let void_t = void_type context;;

let str_type = Arraytype(Char_t, 1)
let noop = SNoexpr(Datatype(Int_t))

let gimmi_array_type = function
    x -> array_type x
    | _ -> array_type i32_t

let debug = fun s ->  
	print_endline ("`````````````````````````````````````"^s);
	dump_module the_module;
	print_endline ("`````````````````````````````````````"^s);
	()

let rec get_ptr_type datatype = match datatype with
		Arraytype(t, 0) -> get_type (Datatype(t))
	|	Arraytype(t, 1) -> pointer_type (get_type (Datatype(t)))
	|	Arraytype(t, i) -> pointer_type (get_ptr_type (Arraytype(t, (i-1))))
	| 	_ -> raise(Exceptions.InvalidStructType "Array Pointer Type")

and find_struct name = 
	try Hashtbl.find struct_types name
	with | Not_found -> raise(Exceptions.InvalidStructType name)

and get_type (datatype:Ast.datatype) = match datatype with 
		Datatype(Int_t) -> i32_t
	| 	Datatype(Float_t) -> f_t
	| 	Datatype(Bool_t) -> i1_t
	| 	Datatype(Char_t) -> i8_t
	| 	Datatype(Void_t) -> void_t
	| 	Datatype(Objecttype(name)) -> pointer_type(find_struct name)
	| 	Arraytype(t, i) -> get_ptr_type (Arraytype(t, (i)))
	| 	_ -> raise(Exceptions.InvalidStructType "Low level type") 



(* cast will return an llvalue of the desired type *)
(* The commented out casts are unsupported actions in Dice *)
let cast lhs rhs lhsType rhsType llbuilder = 
	match (lhsType, rhsType) with
		(* int to,__ ) ( using const_sitofp for signed ints *)
		(Datatype(Int_t), Datatype(Int_t))				-> (lhs, rhs), Datatype(Int_t)
	| 	(Datatype(Int_t), Datatype(Char_t))				-> (build_uitofp lhs i8_t "" llbuilder, rhs), Datatype(Char_t)
	(* |   	(Datatype(Int_t), Datatype(Bool_t))				-> (lhs, const_zext rhs i32_t) *)
	|   (Datatype(Int_t), Datatype(Float_t)) 			-> (build_sitofp lhs f_t "" llbuilder, rhs), Datatype(Float_t)

		(* char to,__)  ( using uitofp since char isn't signed *)
	|   (Datatype(Char_t), Datatype(Int_t)) 			-> (lhs, build_uitofp rhs i8_t "" llbuilder), Datatype(Char_t)
	|   (Datatype(Char_t), Datatype(Char_t)) 			-> (lhs, rhs), Datatype(Char_t)
	(* | 	(Datatype(Char_t), Datatype(Bool_t))			-> (lhs, const_zext rhs i8_t) *)
	(* | 	(Datatype(Char_t), Datatype(Float_t))			-> (const_uitofp lhs f_t, rhs) *)

		(* bool to,__)  ( zext fills the empty bits with zeros, zero extension *)
	(* |   	(Datatype(Bool_t), Datatype(Int_t)) 			-> (const_zext lhs i32_t, rhs) *)
	(* | 	(Datatype(Bool_t), Datatype(Char_t))			-> (const_zext lhs i8_t, rhs) *)
	|   	(Datatype(Bool_t), Datatype(Bool_t))			-> (lhs, rhs), Datatype(Bool_t)
	(* |   	(Datatype(Bool_t), Datatype(Float_t))			-> (const_uitofp lhs f_t, rhs) *)

		(* float to,__) ( using fptosi for signed ints *)
	|   (Datatype(Float_t), Datatype(Int_t)) 			-> (lhs, build_sitofp rhs f_t "" llbuilder), Datatype(Float_t)
	(* | 	(Datatype(Float_t), Datatype(Char_t))			-> (lhs, const_uitofp rhs f_t) *)
	(* |   	(Datatype(Float_t), Datatype(Bool_t))			-> (lhs, const_uitofp rhs f_t) *)
	|   (Datatype(Float_t), Datatype(Float_t)) 			-> (lhs, rhs), Datatype(Float_t)

	| 	_ 												-> raise (Exceptions.CannotCastTypeException(Utils.string_of_datatype lhsType, Utils.string_of_datatype rhsType))

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
	
	let (e1, e2), d = cast e1 e2 type1 type2 llbuilder in

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
				Datatype(Float_t)   
			|	Datatype(Int_t)		
			|   Datatype(Bool_t)	-> unops op eType e
			|   _ -> raise Exceptions.InvalidUnopEvaluationType
		in
		
		unop_type_handler d
 
and func_lookup fname = 
	match (lookup_function fname the_module) with
			None 	-> raise (Exceptions.LLVMFunctionNotFound fname)
		|  	Some f 	-> f

and codegen_print el llbuilder = 
	let printf = func_lookup "printf" in
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

and codegen_func_call fname el llbuilder = 
	let f = func_lookup fname in
	let params = List.map (codegen_sexpr llbuilder) el in
	build_call f (Array.of_list params) "" llbuilder

and codegen_sizeof el llbuilder =
	let type_of = Analyzer.get_type_from_sexpr (List.hd el) in
	let type_of = get_type type_of in
	let size_of = size_of type_of in
	build_bitcast size_of i32_t "" llbuilder

and codegen_cast el d llbuilder =
	let cast_malloc_to_objtype lhs currType newType llbuilder = match newType with
		Datatype(Objecttype(x)) -> 
			let obj_type = get_type (Datatype(Objecttype(x))) in 
			build_pointercast lhs obj_type "" llbuilder
		| 	_ as t -> raise (Exceptions.CannotCastTypeException(Utils.string_of_datatype currType, Utils.string_of_datatype t))
	in
	let expr = List.hd el in
	let t = Analyzer.get_type_from_sexpr expr in
	let expr = codegen_sexpr llbuilder expr in
	cast_malloc_to_objtype expr t d llbuilder


and codegen_call llbuilder d el = function
		"print" 	-> codegen_print el llbuilder
	(* |  	"malloc" 	-> codegen_malloc el llbuilder *)
	| 	"sizeof"	-> codegen_sizeof el llbuilder
	| 	"cast" 		-> codegen_cast el d llbuilder
	| 	"malloc" 	-> codegen_func_call "malloc" el llbuilder
	| 	_ as fname 	-> raise (Exceptions.UnableToCallFunctionWithoutParent fname)(* codegen_func_call fname el llbuilder *)

and codegen_id isDeref checkParam id d llbuilder = 
	if isDeref then
		try Hashtbl.find named_params id
		with | Not_found ->
		try let _val = Hashtbl.find named_values id in
			build_load _val id llbuilder
		with | Not_found -> raise (Exceptions.UnknownVariable id)
	else 
		try Hashtbl.find named_values id
		with | Not_found ->
			try 
				let _val = Hashtbl.find named_params id in
				if checkParam then raise (Exceptions.CannotAssignParam id)
				else _val
		with | Not_found -> raise (Exceptions.UnknownVariable id)

and codegen_assign lhs rhs d llbuilder = 
	(* Special case '=' because we don't want to emit the LHS as an
	* expression. *)
	let lhs = match lhs with
	| 	Sast.SId(id, d) -> codegen_id false false id d llbuilder
	|  	SObjAccess(e1, e2, d) -> codegen_obj_access false e1 e2 d llbuilder
	| _ -> raise Exceptions.AssignLHSMustBeAssignable
	in
	(* Codegen the rhs. *)
	let rhs = codegen_sexpr llbuilder rhs in
	let rhs = match d with 
			Datatype(Objecttype(_))	-> build_load rhs "" llbuilder
		| _ -> rhs 
	 in
	(* Lookup the name. *)
	ignore(build_store rhs lhs llbuilder);
	rhs

and deref ptr t llbuilder = 
	build_gep ptr (Array.of_list [ptr]) "" llbuilder

and codegen_obj_access isAssign lhs rhs d llbuilder = 
	let codegen_func_call fname parent_expr el llbuilder = 
		let f = func_lookup fname in
		let params = List.map (codegen_sexpr llbuilder) el in
		build_call f (Array.of_list (parent_expr :: params)) "" llbuilder
	in
	let check_lhs = function
		SId(s, d)			-> codegen_id false false s d llbuilder
	| 	_  	-> raise (Exceptions.LHSofRootAccessMustBeIDorFunc ("Need to print sexpr"))
	in
	(* Needs to be changed *)
	let rec check_rhs parent_expr parent_type = 
		let parent_str = Utils.string_of_object parent_type in
		function
			(* Check fields in parent *)
			SId(field, d) -> 
				let field_index = Hashtbl.find struct_field_indexes (parent_str ^ "." ^ field) in
				let _val = build_struct_gep parent_expr field_index "" llbuilder in
				if isAssign then
					build_load _val field llbuilder
				else 
					_val
			(* Check functions in parent *)
		| 	SCall(fname, el, d) 	-> codegen_func_call fname parent_expr el llbuilder
			(* Set parent, check if base is field *)
		| 	SObjAccess(e1, e2, d) 	-> 
				let e1_type = Analyzer.get_type_from_sexpr e1 in
				let e1 = check_rhs parent_expr parent_type e1 in
				let e2 = check_rhs e1 e1_type e2 in
				e2
		| 	_ -> raise (Exceptions.InvalidAccessLHS ("Need to print sexpr"))
	in 
	let lhs_type = Analyzer.get_type_from_sexpr lhs in 
	let lhs = check_lhs lhs in
	let rhs = check_rhs lhs lhs_type rhs in
	rhs

and codegen_obj_create fname el d llbuilder = 
	let f = func_lookup fname in
	let params = List.map (codegen_sexpr llbuilder) el in
	let obj = build_call f (Array.of_list params) "" llbuilder in
	obj

and codegen_string_lit s llbuilder = 
	if s = "true" then build_global_stringptr "true" "" llbuilder
	else if s = "false" then build_global_stringptr "false" "" llbuilder
	else build_global_stringptr s "" llbuilder


and codegen_array_create t = function
    [] -> [] 
    | head::(_::_) -> array_type ((codegen_array_create t head)) (List.length head)

(*and get_freaking_type = function
    [] -> []
    | head::tail -> *) 

and codegen_sexpr llbuilder = function
		SInt_Lit(i, d)            -> const_int i32_t i
	|   SBoolean_Lit(b, d)        -> if b then const_int i1_t 1 else const_int i1_t 0
	|   SFloat_Lit(f, d)          -> const_float f_t f 
	|   SString_Lit(s, d)         -> codegen_string_lit s llbuilder
	|   SChar_Lit(c, d)           -> const_int i32_t (Char.code c)
	|   SId(id, d)                -> codegen_id true false id d llbuilder
	|   SBinop(e1, op, e2, d)     -> handle_binop e1 op e2 d llbuilder
	|   SAssign(e1, e2, d)        -> codegen_assign e1 e2 d llbuilder
	|   SNoexpr d                 -> build_add (const_int i32_t 0) (const_int i32_t 0) "nop" llbuilder
(*	|   SArrayCreate(t, el, d)    -> let x = (array_type (get_type t) (List.length el)) in let y = (array_type x (List.length el)) in build_alloca y "" llbuilder*)
    |   SArrayCreate(t, el, d)    -> build_alloca (codegen_array_create t el) "" builder 
	|   SArrayAccess(e, el, d)    -> build_global_stringptr "Hi" "" llbuilder
	|   SObjAccess(e1, e2, d)     -> codegen_obj_access true e1 e2 d llbuilder
	|   SCall(fname, el, d)       -> codegen_call llbuilder d el fname		
	|   SObjectCreate(id, el, d)  -> codegen_obj_create id el d llbuilder

    |   SArrayPrimitive(el, d)    -> build_global_stringptr "Hi" "" llbuilder 
(*    |   SArrayPrimitive(el, d)    -> const_array (get_type d) (Array.of_list (List.fold_left (fun s t -> codegen_sexpr s t) llbuilder el)) *)
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
	let _(* then_val *) = codegen_stmt llbuilder then_ in

	(* Codegen of 'then' can change the current block, update then_bb for the
	 * phi. We create a new name because one is used for the phi node, and the
	 * other is used for the conditional branch. *)
	let new_then_bb = insertion_block llbuilder in

	(* Emit 'else' value. *)
	let else_bb = append_block context "else" the_function in
	position_at_end else_bb llbuilder;
	let _ (* else_val *) = codegen_stmt llbuilder else_ in

	(* Codegen of 'else' can change the current block, update else_bb for the
	 * phi. *)
	let new_else_bb = insertion_block llbuilder in

	(* Emit merge block. *)
	let merge_bb = append_block context "ifcont" the_function in
	position_at_end merge_bb llbuilder;
	(* let then_bb_val = value_of_block new_then_bb in *)
	let else_bb_val = value_of_block new_else_bb in
	(* let incoming = [(then_bb_val, new_then_bb); (else_bb_val, new_else_bb)] in *)
	(* let phi = build_phi incoming "iftmp" llbuilder in *)

	(* Return to the start block to add the conditional branch. *)
	position_at_end start_bb llbuilder;
	ignore (build_cond_br cond_val then_bb else_bb llbuilder);

	(* Set a unconditional branch at the end of the 'then' block and the
	 * 'else' block to the 'merge' block. *)
	position_at_end new_then_bb llbuilder; ignore (build_br merge_bb llbuilder);
	position_at_end new_else_bb llbuilder; ignore (build_br merge_bb llbuilder);

	(* Finally, set the builder to the end of the merge block. *)
	position_at_end merge_bb llbuilder;

	else_bb_val (* phi *)

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
	let t = match datatype with 
			Datatype(Objecttype(name)) -> find_struct name
		|  	_ -> get_type datatype
	in
	let alloca = build_alloca t var_name llbuilder in
	Hashtbl.add named_values var_name alloca;
	let lhs = SId(var_name, datatype) in
	match expr with 
		SNoexpr(_) -> alloca
	|  	_ -> codegen_assign lhs expr datatype llbuilder

and codegen_ret d expr llbuilder =  
	let e = match expr with
		SId(name, d) ->
			(match d with 
			| Datatype(Objecttype(_)) -> codegen_id false false name d llbuilder
			| _ -> codegen_id true true name d llbuilder)
		| SObjAccess(e1, e2, d) -> codegen_obj_access true e1 e2 d llbuilder
		| _ -> codegen_sexpr llbuilder expr 
	in
	build_ret e llbuilder

and codegen_stmt llbuilder = function
		SBlock sl        			-> List.hd(List.map (codegen_stmt llbuilder) sl)	
	|   SExpr(e, d)          		-> codegen_sexpr llbuilder e
	|   SReturn(e, d)    			-> codegen_ret d e llbuilder
	|   SIf (e, s1, s2)       		-> codegen_if_stmt e s1 s2 llbuilder
	|   SFor (e1, e2, e3, s)  		-> codegen_for e1 e2 e3 s llbuilder
	|   SWhile (e, s)    			-> build_global_stringptr "Hi" "" llbuilder
	|   SBreak           			-> build_global_stringptr "Hi" "" llbuilder   
	|   SContinue        			-> build_global_stringptr "Hi" "" llbuilder
	|   SLocal(d, s, e)  			-> codegen_alloca d s e llbuilder

let codegen_funcstub sfdecl = 
	let fname = (Utils.string_of_fname sfdecl.sfname) in
	let is_var_arg = ref false in
	let params = List.rev (List.fold_left (fun l -> (function Formal(t, _) -> get_type t :: l | _ -> is_var_arg := true; l )) [] sfdecl.sformals) in
	let fty = if !is_var_arg 
			then var_arg_function_type (get_type sfdecl.sreturnType) (Array.of_list params)
			else function_type (get_type sfdecl.sreturnType) (Array.of_list params) 
	in
	define_function fname fty the_module

let init_params f formals =
	let formals = Array.of_list (formals) in
	Array.iteri (fun i a ->
        let n = formals.(i) in
        let n = Utils.string_of_formal_name n in
        set_value_name n a;
        Hashtbl.add named_params n a;
    ) (params f)

let codegen_func sfdecl = 
	Hashtbl.clear named_values;
	Hashtbl.clear named_params;
	let fname = (Utils.string_of_fname sfdecl.sfname) in
	let f = func_lookup fname in
	let llbuilder = builder_at_end context (entry_block f) in
	let _ = init_params f sfdecl.sformals in 
	let _ = codegen_stmt llbuilder (SBlock (sfdecl.sbody)) in
	if sfdecl.sreturnType = Datatype(Void_t) 
		then ignore(build_ret_void llbuilder);
	()
	
let codegen_library_functions () = 
	let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
	let _ = declare_function "printf" printf_ty the_module in
	let malloc_ty = function_type (str_t) [| i32_t |] in
	let _ = declare_function "malloc" malloc_ty the_module in
	()

let codegen_struct_stub s =
	let struct_t = named_struct_type context s.scname in
	Hashtbl.add struct_types s.scname struct_t

let codegen_struct s = 
	let struct_t = Hashtbl.find struct_types s.scname in
	let type_list = List.map (function Field(_, d, _) -> get_type d) s.sfields in
	let name_list = List.map (function Field(_, _, s) -> s) s.sfields in
	let type_array = (Array.of_list type_list) in
	List.iteri (fun i f ->
        let n = s.scname ^ "." ^ f in
        Hashtbl.add struct_field_indexes n i;
    ) name_list;
	struct_set_body struct_t type_array true

let codegen_main main = 
	Hashtbl.clear named_values;
	Hashtbl.clear named_params;
	let fty = function_type i32_t [| |] in
	let f = define_function "main" fty the_module in
	let llbuilder = builder_at_end context (entry_block f) in
	let _ = codegen_stmt llbuilder (SBlock (main.sbody)) in
	build_ret (const_int i32_t 0) llbuilder 

let codegen_sprogram sprogram = 
	let _ = codegen_library_functions () in
	let _ = List.map (fun s -> codegen_struct_stub s) sprogram.classes in
	let _ = List.map (fun s -> codegen_struct s) sprogram.classes in
	let _ = List.map (fun f -> codegen_funcstub f) sprogram.functions in
	let _ = List.map (fun f -> codegen_func f) sprogram.functions in
	let _ = codegen_main sprogram.main in
	the_module
