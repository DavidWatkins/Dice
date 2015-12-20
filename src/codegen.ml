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
open Conf

open Llvm.MemoryBuffer
open Llvm_bitreader

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
let i64_t = i64_type context;;
let void_t = void_type context;;

let str_type = Arraytype(Char_t, 1)

let (br_block) = ref (block_of_value (const_int i32_t 0))
let (cont_block) = ref (block_of_value (const_int i32_t 0))
let is_loop = ref false

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
	| 	Datatype(Null_t) -> i32_t
	| 	Datatype(Objecttype(name)) -> pointer_type(find_struct name)
	| 	Arraytype(t, i) -> get_ptr_type (Arraytype(t, (i)))
	| 	d -> raise(Exceptions.InvalidStructType (Utils.string_of_datatype d)) 

(* cast will return an llvalue of the desired type *)
(* The commented out casts are unsupported actions in Dice *)
let cast lhs rhs lhsType rhsType llbuilder = 
	match (lhsType, rhsType) with
		(* int to,__ ) ( using const_sitofp for signed ints *)
		(Datatype(Int_t), Datatype(Int_t))				-> (lhs, rhs), Datatype(Int_t)
	| 	(Datatype(Int_t), Datatype(Char_t))				-> (build_uitofp lhs i8_t "tmp" llbuilder, rhs), Datatype(Char_t)
	(* |   	(Datatype(Int_t), Datatype(Bool_t))				-> (lhs, const_zext rhs i32_t) *)
	|   (Datatype(Int_t), Datatype(Float_t)) 			-> (build_sitofp lhs f_t "tmp" llbuilder, rhs), Datatype(Float_t)

		(* char to,__)  ( using uitofp since char isn't signed *)
	|   (Datatype(Char_t), Datatype(Int_t)) 			-> (lhs, build_uitofp rhs i8_t "tmp" llbuilder), Datatype(Char_t)
	|   (Datatype(Char_t), Datatype(Char_t)) 			-> (lhs, rhs), Datatype(Char_t)
	(* | 	(Datatype(Char_t), Datatype(Bool_t))			-> (lhs, const_zext rhs i8_t) *)
	(* | 	(Datatype(Char_t), Datatype(Float_t))			-> (const_uitofp lhs f_t, rhs) *)

		(* bool to,__)  ( zext fills the empty bits with zeros, zero extension *)
	(* |   	(Datatype(Bool_t), Datatype(Int_t)) 			-> (const_zext lhs i32_t, rhs) *)
	(* | 	(Datatype(Bool_t), Datatype(Char_t))			-> (const_zext lhs i8_t, rhs) *)
	|   	(Datatype(Bool_t), Datatype(Bool_t))			-> (lhs, rhs), Datatype(Bool_t)
	(* |   	(Datatype(Bool_t), Datatype(Float_t))			-> (const_uitofp lhs f_t, rhs) *)

		(* float to,__) ( using fptosi for signed ints *)
	|   (Datatype(Float_t), Datatype(Int_t)) 			-> (lhs, build_sitofp rhs f_t "tmp" llbuilder), Datatype(Float_t)
	(* | 	(Datatype(Float_t), Datatype(Char_t))			-> (lhs, const_uitofp rhs f_t) *)
	(* |   	(Datatype(Float_t), Datatype(Bool_t))			-> (lhs, const_uitofp rhs f_t) *)
	|   (Datatype(Float_t), Datatype(Float_t)) 			-> (lhs, rhs), Datatype(Float_t)

	| Datatype(Objecttype(d)), Datatype(Null_t)			-> (lhs, rhs), lhsType
	| Datatype(Null_t), Datatype(Objecttype(d)) 		-> (rhs, lhs), rhsType
	| Datatype(Objecttype(d)), t 						-> raise(Exceptions.CanOnlyCompareObjectsWithNull(d, (Utils.string_of_datatype t)))

	| Arraytype(d, s), Datatype(Null_t)					-> (lhs, rhs), lhsType
	| Datatype(Null_t), Arraytype(d, s) 				-> (rhs, lhs), rhsType
	| Arraytype(d, _), t 								-> raise(Exceptions.CanOnlyCompareArraysWithNull(Utils.string_of_primitive d, (Utils.string_of_datatype t)))

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
	| 	Mod 		-> build_frem e1 e2 "flt_sremtmp" llbuilder
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
	| 	Mod 		-> build_srem e1 e2 "sremtmp" llbuilder
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

	let obj_ops op e1 e2 = 
		match op with
			Equal -> build_is_null e1 "tmp" llbuilder 
		| 	Neq -> build_is_not_null e1 "tmp" llbuilder 
		| 	_ 	-> raise (Exceptions.ObjOpNotSupported(Utils.string_of_op op))
	in
	
	let (e1, e2), d = cast e1 e2 type1 type2 llbuilder in

	let type_handler d = match d with
			Datatype(Float_t)   -> float_ops op e1 e2
		|	Datatype(Int_t)	
		|   Datatype(Bool_t)
		| 	Datatype(Char_t) 	-> int_ops op e1 e2
		| 	Datatype(Objecttype(_))
		| 	Arraytype(_, _) -> obj_ops op e1 e2
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
			let trueStr = SString_Lit("true") in
			let falseStr = SString_Lit("false") in
			let id = SId(tmp_var, str_type) in 
			ignore(codegen_stmt llbuilder (SLocal(str_type, tmp_var, SNoexpr)));
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
	let s = codegen_sexpr llbuilder (SString_Lit(const_str)) in
	let zero = const_int i32_t 0 in 
	let s = build_in_bounds_gep s [| zero |] "tmp" llbuilder in
	build_call printf (Array.of_list (s :: params)) "tmp" llbuilder

and codegen_func_call fname el d llbuilder = 
	let f = func_lookup fname in
	let params = List.map (codegen_sexpr llbuilder) el in
	match d with
		Datatype(Void_t) -> build_call f (Array.of_list params) "" llbuilder
	| 	_ -> 				build_call f (Array.of_list params) "tmp" llbuilder

and codegen_sizeof el llbuilder =
	let type_of = Analyzer.get_type_from_sexpr (List.hd el) in
	let type_of = get_type type_of in
	let size_of = size_of type_of in
	build_bitcast size_of i32_t "tmp" llbuilder

and codegen_cast el d llbuilder =
	let cast_malloc_to_objtype lhs currType newType llbuilder = match newType with
		Datatype(Objecttype(x)) -> 
			let obj_type = get_type (Datatype(Objecttype(x))) in 
			build_pointercast lhs obj_type "tmp" llbuilder
		| 	_ as t -> raise (Exceptions.CannotCastTypeException(Utils.string_of_datatype currType, Utils.string_of_datatype t))
	in
	let expr = List.hd el in
	let t = Analyzer.get_type_from_sexpr expr in
	let expr = codegen_sexpr llbuilder expr in
	cast_malloc_to_objtype expr t d llbuilder

and codegen_input el d llbuilder =
	let malloc = func_lookup "malloc" in
	let realloc = func_lookup "realloc" in

	let SIZE = const_int i32_t 100 in
	let init_size = build_alloca i32_t "init_size" llbuilder in
	ignore(build_store SIZE init_size llbuilder);


		let new_block label =
		let f = block_parent (insertion_block llbuilder) in
		append_block (global_context ()) label f
	in
	let bbcurr = insertion_block llbuilder in
	let bbcond = new_block "read.cond" in
	let bbbody = new_block "read.init" in
	let bbdone = new_block "read.done" in
	ignore (build_br bbcond llbuilder);
	position_at_end bbcond llbuilder;

	(* Counter into the length of the array *)
	let counter = build_phi [const_int i32_t start_pos, bbcurr] "counter" llbuilder in
	add_incoming ((build_add counter (const_int i32_t 1) "tmp" llbuilder), bbbody) counter;
	let cmp = build_icmp Icmp.Slt counter arr_len "tmp" llbuilder in
	ignore (build_cond_br cmp bbbody bbdone llbuilder);
	position_at_end bbbody llbuilder;

	(* Assign array position to init_val *)
	let arr_ptr = build_gep arr [| counter |] "tmp" llbuilder in
	ignore (build_store init_val arr_ptr llbuilder);
	ignore (build_br bbcond llbuilder);
	position_at_end bbdone llbuilder

and codegen_call llbuilder d el = function
		"print" 	-> codegen_print el llbuilder
	| 	"sizeof"	-> codegen_sizeof el llbuilder
	| 	"cast" 		-> codegen_cast el d llbuilder
	| 	"malloc" 	-> codegen_func_call "malloc" el d llbuilder
	| 	"open" 		-> codegen_func_call "open" el d llbuilder
	| 	"write"		-> codegen_func_call "write" el d llbuilder
	| 	"close"		-> codegen_func_call "close" el d llbuilder
	| 	"read" 		-> codegen_func_call "read" el d llbuilder
	| 	"lseek" 	-> codegen_func_call "lseek" el d llbuilder
	| 	"exit" 		-> codegen_func_call "exit" el d llbuilder
	| 	"input" 	-> codegen_input el d llbuilder
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
	let rhsType = Analyzer.get_type_from_sexpr rhs in
	(* Special case '=' because we don't want to emit the LHS as an
	* expression. *)
	let lhs, isObjAccess = match lhs with
	| 	Sast.SId(id, d) -> codegen_id false false id d llbuilder, false
	|  	SObjAccess(e1, e2, d) -> codegen_obj_access false e1 e2 d llbuilder, true
	| 	SArrayAccess(se, sel, d) -> codegen_array_access true se sel d llbuilder, false
	| _ -> raise Exceptions.AssignLHSMustBeAssignable
	in
	(* Codegen the rhs. *)
	let rhs = codegen_sexpr llbuilder rhs in
	let rhs = match d with 
			Datatype(Objecttype(_))	-> 
				if isObjAccess then rhs
				else build_load rhs "tmp" llbuilder
		| 	Datatype(Null_t) -> const_null (get_type d)
		| _ -> rhs 
	in
	let rhs = match d, rhsType with
		Datatype(Char_t), Datatype(Int_t) -> build_uitofp rhs i8_t "tmp" llbuilder
	| 	Datatype(Int_t), Datatype(Char_t) -> build_uitofp rhs i32_t "tmp" llbuilder
	| 	_ -> rhs
	in 
	(* Lookup the name. *)
	ignore(build_store rhs lhs llbuilder);
	rhs

and deref ptr t llbuilder = 
	build_gep ptr (Array.of_list [ptr]) "tmp" llbuilder

and codegen_obj_access isAssign lhs rhs d llbuilder = 
	let codegen_func_call fptr parent_expr el d llbuilder = 
		let match_sexpr se = match se with
			SId(id, d) -> let isDeref = match d with
				Datatype(Objecttype(_)) -> false
			| 	_ -> true 
			in codegen_id isDeref false id d llbuilder
		| 	se -> codegen_sexpr llbuilder se
		in
		let params = List.map match_sexpr el in
		match d with
			Datatype(Void_t) -> build_call fptr (Array.of_list (parent_expr :: params)) "" llbuilder
		| 	_ -> build_call fptr (Array.of_list (parent_expr :: params)) "tmp" llbuilder
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
				let search_term = (parent_str ^ "." ^ field) in
				let field_index = Hashtbl.find struct_field_indexes search_term in
				let _val = build_struct_gep parent_expr field_index "tmp" llbuilder in
				if isAssign then
					build_load _val field llbuilder
				else 
					_val
			(* Check functions in parent *)
		| 	SCall(fname, el, d, index) 	-> 
				let index = const_int i32_t index in
				let c_index = build_struct_gep parent_expr 0 "cindex" llbuilder in
				let c_index = build_load c_index "cindex" llbuilder in
				let lookup = func_lookup "lookup" in
				let fptr = build_call lookup [| c_index; index |] "fptr" llbuilder in
				let f_ty = type_of (func_lookup fname) in
				let fptr = build_pointercast fptr f_ty "fptr" llbuilder in
				codegen_func_call fptr parent_expr el d llbuilder
			(* Set parent, check if base is field *)
		| 	SObjAccess(e1, e2, d) 	-> 
				let e1_type = Analyzer.get_type_from_sexpr e1 in
				let e1 = check_rhs parent_expr parent_type e1 in
				let e2 = check_rhs e1 e1_type e2 in
				e2
		| 	_ -> raise (Exceptions.InvalidAccessLHS ("Need to print sexpr"))
	in 
	let lhs_type = Analyzer.get_type_from_sexpr lhs in 
	match lhs_type with
		Arraytype(_, _) -> 
			let lhs = codegen_sexpr llbuilder lhs in
			let _ = match rhs with
				SId("length", _) -> "length"
			| 	_ -> raise(Exceptions.CanOnlyAccessLengthOfArray)
			in
			let _val = build_gep lhs [| (const_int i32_t 0) |] "tmp" llbuilder in
			build_load _val "tmp" llbuilder 
	| 	_ -> 
		let lhs = check_lhs lhs in
		let rhs = check_rhs lhs lhs_type rhs in
		rhs

and codegen_obj_create fname el d llbuilder = 
	let f = func_lookup fname in
	let params = List.map (codegen_sexpr llbuilder) el in
	let obj = build_call f (Array.of_list params) "tmp" llbuilder in
	obj

and codegen_string_lit s llbuilder = 
	if s = "true" then build_global_stringptr "true" "tmp" llbuilder
	else if s = "false" then build_global_stringptr "false" "tmp" llbuilder
	else build_global_stringptr s "tmp" llbuilder

and codegen_array_access isAssign e el d llbuilder =
	let index = codegen_sexpr llbuilder (List.hd el) in
	let index = match d with
		Datatype(Char_t) -> index
	| 	_ -> build_add index (const_int i32_t 1) "tmp" llbuilder
	in
    let arr = codegen_sexpr llbuilder e in
    let _val = build_gep arr [| index |] "tmp" llbuilder in
    if isAssign
    	then _val
    	else build_load _val "tmp" llbuilder 

and initialise_array arr arr_len init_val start_pos llbuilder =
	let new_block label =
		let f = block_parent (insertion_block llbuilder) in
		append_block (global_context ()) label f
	in
  let bbcurr = insertion_block llbuilder in
  let bbcond = new_block "array.cond" in
  let bbbody = new_block "array.init" in
  let bbdone = new_block "array.done" in
  ignore (build_br bbcond llbuilder);
  position_at_end bbcond llbuilder;

  (* Counter into the length of the array *)
  let counter = build_phi [const_int i32_t start_pos, bbcurr] "counter" llbuilder in
  add_incoming ((build_add counter (const_int i32_t 1) "tmp" llbuilder), bbbody) counter;
  let cmp = build_icmp Icmp.Slt counter arr_len "tmp" llbuilder in
  ignore (build_cond_br cmp bbbody bbdone llbuilder);
  position_at_end bbbody llbuilder;

  (* Assign array position to init_val *)
  let arr_ptr = build_gep arr [| counter |] "tmp" llbuilder in
  ignore (build_store init_val arr_ptr llbuilder);
  ignore (build_br bbcond llbuilder);
  position_at_end bbdone llbuilder

and codegen_array_create llbuilder t expr_type el = 
	if(List.length el > 1) then raise(Exceptions.ArrayLargerThan1Unsupported)
	else
	match expr_type with 
		Arraytype(Char_t, 1) -> 
		let e = List.hd el in
		let size = (codegen_sexpr llbuilder e) in
		let t = get_type t in
		let arr = build_array_malloc t size "tmp" llbuilder in
		let arr = build_pointercast arr (pointer_type t) "tmp" llbuilder in
		(* initialise_array arr size (const_int i32_t 0) 0 llbuilder; *)
		arr
	| 	_ -> 
		let e = List.hd el in
		let t = get_type t in

		(* This will not work for arrays of objects *)
		let size = (codegen_sexpr llbuilder e) in
		let size_t = build_intcast (size_of t) i32_t "tmp" llbuilder in
		let size = build_mul size_t size "tmp" llbuilder in
		let size_real = build_add size (const_int i32_t 1) "arr_size" llbuilder in
		
	    let arr = build_array_malloc t size_real "tmp" llbuilder in
		let arr = build_pointercast arr (pointer_type t) "tmp" llbuilder in
		let arr_len_ptr = build_pointercast arr (pointer_type i32_t) "tmp" llbuilder in

		(* Store length at this position *)
		ignore(build_store size_real arr_len_ptr llbuilder); 
		initialise_array arr size_real (const_int i32_t 0) 1 llbuilder;
		arr

and codegen_array_prim d el llbuilder =
    let t = d in
    let size = (const_int i32_t ((List.length el))) in
    let size_real = (const_int i32_t ((List.length el) + 1)) in
	let t = get_type t in
    let arr = build_array_malloc t size_real "tmp" llbuilder in
	let arr = build_pointercast arr t "tmp" llbuilder in
	let size_casted = build_bitcast size t "tmp" llbuilder in
	ignore(if d = Arraytype(Char_t, 1) then ignore(build_store size_casted arr llbuilder);); (* Store length at this position *)
	initialise_array arr size_real (const_int i32_t 0) 1 llbuilder;

    let llvalues = List.map (codegen_sexpr llbuilder) el in
    List.iteri (fun i llval -> 
    			let arr_ptr = build_gep arr [| (const_int i32_t (i+1)) |] "tmp" llbuilder in
    			ignore(build_store llval arr_ptr llbuilder);  ) llvalues;
    arr

and codegen_delete e llbuilder =
	let ce = codegen_sexpr llbuilder e in
	build_free ce llbuilder

and codegen_sexpr llbuilder = function
		SInt_Lit(i)            		-> const_int i32_t i
	|   SBoolean_Lit(b)        		-> if b then const_int i1_t 1 else const_int i1_t 0
	|   SFloat_Lit(f)          		-> const_float f_t f 
	|   SString_Lit(s)         		-> codegen_string_lit s llbuilder
	|   SChar_Lit(c)           		-> const_int i8_t (Char.code c)
	|   SId(id, d)                	-> codegen_id true false id d llbuilder
	|   SBinop(e1, op, e2, d)     	-> handle_binop e1 op e2 d llbuilder
	|   SAssign(e1, e2, d)        	-> codegen_assign e1 e2 d llbuilder
	|   SNoexpr                 	-> build_add (const_int i32_t 0) (const_int i32_t 0) "nop" llbuilder
	|   SArrayCreate(t, el, d)    	-> codegen_array_create llbuilder t d el
	|   SArrayAccess(e, el, d)    	-> codegen_array_access false e el d llbuilder
	|   SObjAccess(e1, e2, d)     	-> codegen_obj_access true e1 e2 d llbuilder
	|   SCall(fname, el, d, _)       	-> codegen_call llbuilder d el fname		
	|   SObjectCreate(id, el, d)  	-> codegen_obj_create id el d llbuilder
	|   SArrayPrimitive(el, d)    	-> codegen_array_prim d el llbuilder 
	|   SUnop(op, e, d)           	-> handle_unop op e d llbuilder
	|   SNull          	        	-> const_null i32_t
	| 	SDelete e 				 	-> codegen_delete e llbuilder

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
	let old_val = !is_loop in
	is_loop := true;

	let the_function = block_parent (insertion_block llbuilder) in

	(* Emit the start code first, without 'variable' in scope. *)
	let _ = codegen_sexpr llbuilder init_ in

	(* Make the new basic block for the loop header, inserting after current
	* block. *)
	let loop_bb = append_block context "loop" the_function in
	(* Insert maintenance block *)
	let inc_bb = append_block context "inc" the_function in
	(* Insert condition block *)
	let cond_bb = append_block context "cond" the_function in
	(* Create the "after loop" block and insert it. *)
	let after_bb = append_block context "afterloop" the_function in

	let _ = if not old_val then
		cont_block := inc_bb;
		br_block := after_bb;
	in

	(* Insert an explicit fall through from the current block to the
	* loop_bb. *)
	ignore (build_br cond_bb llbuilder);

	(* Start insertion in loop_bb. *)
	position_at_end loop_bb llbuilder;

	(* Emit the body of the loop.  This, like any other expr, can change the
	* current BB.  Note that we ignore the value computed by the body, but
	* don't allow an error *)
	ignore (codegen_stmt llbuilder body_);

	let bb = insertion_block llbuilder in
	move_block_after bb inc_bb;
	move_block_after inc_bb cond_bb;
	move_block_after cond_bb after_bb;
	ignore(build_br inc_bb llbuilder);

	(* Start insertion in loop_bb. *)
	position_at_end inc_bb llbuilder;
	(* Emit the step value. *)	
	let _ = codegen_sexpr llbuilder inc_ in
	ignore(build_br cond_bb llbuilder);

	position_at_end cond_bb llbuilder;

	let cond_val = codegen_sexpr llbuilder cond_ in
	ignore (build_cond_br cond_val loop_bb after_bb llbuilder);

	(* Any new code will be inserted in after_bb. *)
	position_at_end after_bb llbuilder;

	is_loop := old_val;

	(* for expr always returns 0.0. *)
	const_null f_t

and codegen_while cond_ body_ llbuilder = 
	let null_sexpr = SInt_Lit(0) in
	codegen_for null_sexpr cond_ null_sexpr body_ llbuilder

and codegen_alloca datatype var_name expr llbuilder = 
	let t = match datatype with 
			Datatype(Objecttype(name)) -> find_struct name
		|  	_ -> get_type datatype
	in
	let alloca = build_alloca t var_name llbuilder in
	Hashtbl.add named_values var_name alloca;
	let lhs = SId(var_name, datatype) in
	match expr with 
		SNoexpr -> alloca
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

and codegen_break llbuilder = 
	let block = fun () -> !br_block in
	build_br (block ()) llbuilder

and codegen_continue llbuilder = 
	let block = fun () -> !cont_block in
	build_br (block ()) llbuilder

and codegen_stmt llbuilder = function
		SBlock sl        			-> List.hd(List.map (codegen_stmt llbuilder) sl)
	|   SExpr(e, d)          		-> codegen_sexpr llbuilder e
	|   SReturn(e, d)    			-> codegen_ret d e llbuilder
	|   SIf (e, s1, s2)       		-> codegen_if_stmt e s1 s2 llbuilder
	|   SFor (e1, e2, e3, s)  		-> codegen_for e1 e2 e3 s llbuilder
	|   SWhile (e, s)    			-> codegen_while e s llbuilder
	|   SBreak           			-> codegen_break llbuilder   
	|   SContinue        			-> codegen_continue llbuilder
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
	let _ = if sfdecl.overrides then
		let this_param = Hashtbl.find named_params "this" in
		let source = Datatype(Objecttype(sfdecl.source)) in
		let casted_param = build_pointercast this_param (get_type source) "casted" llbuilder in
		Hashtbl.replace named_params "this" casted_param;
	in
	let _ = codegen_stmt llbuilder (SBlock (sfdecl.sbody)) in
	if sfdecl.sreturnType = Datatype(Void_t) 
		then ignore(build_ret_void llbuilder);
	()

let codegen_vtbl scdecls = 
	let rt = pointer_type i64_t in
	let void_pt = pointer_type i64_t in
	let void_ppt = pointer_type void_pt in

	let f = func_lookup "lookup" in
	let llbuilder = builder_at_end context (entry_block f) in

	let len = List.length scdecls in
	let total_len = ref 0 in
	let scdecl_llvm_arr = build_array_alloca void_ppt (const_int i32_t len) "tmp" llbuilder in

	let handle_scdecl scdecl = 
		let index = Hashtbl.find Analyzer.struct_indexes scdecl.scname in
		let len = List.length scdecl.sfuncs in
		let sfdecl_llvm_arr = build_array_alloca void_pt (const_int i32_t len) "tmp" llbuilder in

		let handle_fdecl i sfdecl = 
			let fptr = func_lookup (Utils.string_of_fname sfdecl.sfname) in
			let fptr = build_pointercast fptr void_pt "tmp" llbuilder in

			let ep = build_gep sfdecl_llvm_arr [| (const_int i32_t i) |] "tmp" llbuilder in
			ignore(build_store fptr ep llbuilder);
		in 
		List.iteri handle_fdecl scdecl.sfuncs;
		total_len := !total_len + len;

		let ep = build_gep scdecl_llvm_arr [| (const_int i32_t index) |] "tmp" llbuilder in
		ignore(build_store sfdecl_llvm_arr ep llbuilder);
	in
	List.iter handle_scdecl scdecls;

	let c_index = param f 0 in
	let f_index = param f 1 in
	set_value_name "c_index" c_index;
	set_value_name "f_index" f_index;

	if !total_len == 0 then
		build_ret (const_null rt) llbuilder
	else
		let vtbl = build_gep scdecl_llvm_arr [| c_index |] "tmp" llbuilder in
		let vtbl = build_load vtbl "tmp" llbuilder in
		let fptr = build_gep vtbl [| f_index |] "tmp" llbuilder in
		let fptr = build_load fptr "tmp" llbuilder in

		build_ret fptr llbuilder 
	
let codegen_library_functions () = 
	let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
	let _ = declare_function "printf" printf_ty the_module in
	let malloc_ty = function_type (str_t) [| i32_t |] in
	let _ = declare_function "malloc" malloc_ty the_module in
	(* Not sure we need rec_init *)
    let rec_init_ty = function_type void_t [| (pointer_type i64_t); i32_t; (pointer_type i32_t); (pointer_type i32_t); (pointer_type i32_t); i32_t; i32_t |] in
    let _ = declare_function "rec_init" rec_init_ty the_module in
    let init_arr_ty = function_type (pointer_type i64_t) [| (pointer_type i32_t); i32_t |] in
    let _ = declare_function "init_arr" init_arr_ty the_module in 
    let open_ty = function_type i32_t [| (pointer_type i8_t); i32_t |] in 
    let _ = declare_function "open" open_ty the_module in
    let close_ty = function_type i32_t [| i32_t |] in
    let _ = declare_function "close" close_ty the_module in
    let read_ty = function_type i32_t [| i32_t; pointer_type i8_t; i32_t |] in
    let _ = declare_function "read" read_ty the_module in
    let write_ty = function_type i32_t [| i32_t; pointer_type i8_t; i32_t |] in
    let _ = declare_function "write" write_ty the_module in 
    let lseek_ty = function_type i32_t [| i32_t; i32_t; i32_t |] in
    let _ = declare_function "lseek" lseek_ty the_module in
    let exit_ty = function_type void_t [| i32_t |] in
    let _ = declare_function "exit" exit_ty the_module in
    let fty = function_type (pointer_type i64_t) [| i32_t; i32_t |] in
	let _ = define_function "lookup" fty the_module in
	let realloc_ty = function_type str_t [| str_t; i32_t |] in
	let _ = declare_function "realloc" realloc_ty the_module in
    ()

let codegen_struct_stub s =
	let struct_t = named_struct_type context s.scname in
	Hashtbl.add struct_types s.scname struct_t

let codegen_struct s = 
		let struct_t = Hashtbl.find struct_types s.scname in
	let type_list = List.map (function Field(_, d, _) -> get_type d) s.sfields in
	let name_list = List.map (function Field(_, _, s) -> s) s.sfields in

	(* Add key field to all structs *)
	let type_list = i32_t :: type_list in
	let name_list = ".key" :: name_list in

	let type_array = (Array.of_list type_list) in
	List.iteri (fun i f ->
        let n = s.scname ^ "." ^ f in
        Hashtbl.add struct_field_indexes n i;
    ) name_list;
	struct_set_body struct_t type_array true

let init_args argv args argc llbuilder =
	let new_block label =
		let f = block_parent (insertion_block llbuilder) in
		append_block (global_context ()) label f
	in
	let bbcurr = insertion_block llbuilder in
	let bbcond = new_block "args.cond" in
	let bbbody = new_block "args.init" in
	let bbdone = new_block "args.done" in
	ignore (build_br bbcond llbuilder);
	position_at_end bbcond llbuilder;

	(* Counter into the length of the array *)
	let counter = build_phi [const_int i32_t 0, bbcurr] "counter" llbuilder in
	add_incoming ((build_add counter (const_int i32_t 1) "tmp" llbuilder), bbbody) counter;
	let cmp = build_icmp Icmp.Slt counter argc "tmp" llbuilder in
	ignore (build_cond_br cmp bbbody bbdone llbuilder);
	position_at_end bbbody llbuilder;

	(* Assign array position to init_val *)
	let arr_ptr = build_gep args [| counter |] "tmp" llbuilder in
	let argv_val = build_gep argv [| counter |] "tmp" llbuilder in
	let argv_val = build_load argv_val "tmp" llbuilder in
	ignore (build_store argv_val arr_ptr llbuilder);
	ignore (build_br bbcond llbuilder);
	position_at_end bbdone llbuilder

let construct_args argc argv llbuilder = 
	let str_pt = pointer_type str_t in
	let size_real = build_add argc (const_int i32_t 1) "arr_size" llbuilder in

	let arr = build_array_malloc str_pt size_real "args" llbuilder in
	let arr = build_pointercast arr str_pt "args" llbuilder in
	let arr_len_ptr = build_pointercast arr (pointer_type i32_t) "argc_len" llbuilder in
	let arr_1 = build_gep arr [| const_int i32_t 1 |] "arr_1" llbuilder in

	(* Store length at this position *)
	ignore(build_store argc arr_len_ptr llbuilder); 
	ignore(init_args argv arr_1 argc llbuilder);
	arr

let codegen_main main = 
	Hashtbl.clear named_values;
	Hashtbl.clear named_params;
	let fty = function_type i32_t [| i32_t; pointer_type str_t |] in
	let f = define_function "main" fty the_module in
	let llbuilder = builder_at_end context (entry_block f) in

	let argc = param f 0 in
	let argv = param f 1 in
	set_value_name "argc" argc;
	set_value_name "argv" argv;
	let args = construct_args argc argv llbuilder in
	Hashtbl.add named_params "args" args;

	let _ = codegen_stmt llbuilder (SBlock (main.sbody)) in
	build_ret (const_int i32_t 0) llbuilder 

let linker filename = 
	let llctx = Llvm.global_context () in
	let llmem = Llvm.MemoryBuffer.of_file filename in
	let llm = Llvm_bitreader.parse_bitcode llctx llmem in
	ignore(Llvm_linker.link_modules the_module llm)

let codegen_sprogram sprogram = 
	let _ = codegen_library_functions () in
	let _ = List.map (fun s -> codegen_struct_stub s) sprogram.classes in
	let _ = List.map (fun s -> codegen_struct s) sprogram.classes in
	let _ = List.map (fun f -> codegen_funcstub f) sprogram.functions in
	let _ = List.map (fun f -> codegen_func f) sprogram.functions in
	let _ = codegen_main sprogram.main in
	let _ = codegen_vtbl sprogram.classes in
	let _ = linker Conf.bindings_path in
	the_module

(* Need to handle assignment of two different types *)
(* Need to handle private/public access *)