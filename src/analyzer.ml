open Sast
open Ast
open Processor
open Utils
open Filepath
open Conf

module StringMap = Map.Make (String)

module StringSet = Set.Make (String)

let struct_indexes:(string, int) Hashtbl.t = Hashtbl.create 10
let predecessors:(string, string list) Hashtbl.t = Hashtbl.create 10

module SS = Set.Make(
	struct
		let compare = Pervasives.compare
		type t = datatype
	end )

type class_map = {
	field_map       : Ast.field StringMap.t;
	func_map        : Ast.func_decl StringMap.t;
	constructor_map : Ast.func_decl StringMap.t;
	reserved_map 	: sfunc_decl StringMap.t;
	cdecl 			: Ast.class_decl;
}

type env = {
	env_class_maps: class_map StringMap.t;
	env_name      : string;
	env_cmap 	  : class_map;
	env_locals    : datatype StringMap.t;
	env_parameters: Ast.formal StringMap.t;
	env_returnType: datatype;
	env_in_for    : bool;
	env_in_while  : bool;
	env_reserved  : sfunc_decl list;
}

let update_env_name env env_name = 
{
	env_class_maps = env.env_class_maps;
	env_name       = env_name;
	env_cmap 	   = env.env_cmap;
	env_locals     = env.env_locals;
	env_parameters = env.env_parameters;
	env_returnType = env.env_returnType;
	env_in_for     = env.env_in_for;
	env_in_while   = env.env_in_while;
	env_reserved   = env.env_reserved;
}

let update_call_stack env in_for in_while = 
{
	env_class_maps = env.env_class_maps;
	env_name       = env.env_name;
	env_cmap 	   = env.env_cmap;
	env_locals     = env.env_locals;
	env_parameters = env.env_parameters;
	env_returnType = env.env_returnType;
	env_in_for     = in_for;
	env_in_while   = in_while;
	env_reserved   = env.env_reserved;
}

let append_code_to_constructor fbody cname ret_type =
	let key = Hashtbl.find struct_indexes cname in 
	let init_this = [SLocal(
		ret_type,
		"this",
		SCall(	"cast", 
				[SCall("malloc", 
					[	
						SCall("sizeof", [SId("ignore", ret_type)], Datatype(Int_t), 0)
					], 
					Arraytype(Char_t, 1), 0)
				],
				ret_type,
				0
			)
		);
		SExpr(
			SAssign(
				SObjAccess(
					SId("this", ret_type),
					SId(".key", Datatype(Int_t)),
					Datatype(Int_t)
				),
				SInt_Lit(key),
				Datatype(Int_t)
			),
			Datatype(Int_t)
		)
	]
	in
	let ret_this = 
		[
			SReturn(
				SId("this", ret_type),
				ret_type
			)
		]
	in
	(* Need to check for duplicate default constructs *)
	(* Also need to add malloc around other constructors *)
	init_this @ fbody @ ret_this

let default_constructor_body cname = 
	let ret_type = Datatype(Objecttype(cname)) in
	let fbody = [] in
	append_code_to_constructor fbody cname ret_type

let default_sc cname = 
{
	sfname 			= Ast.FName (cname ^ "." ^ "constructor");
	sreturnType 	= Datatype(Objecttype(cname));
	sformals 		= [];
	sbody 			= default_constructor_body cname;
	func_type		= Sast.User;
	overrides       = false;
	source 			= "NA";
}

let default_c cname = 
{
	scope			= Ast.Public;
	fname 			= Ast.Constructor;
	returnType 		= Datatype(ConstructorType);
	formals 		= [];
	body 			= [];
	overrides 		= false;
	root_cname 		= None;
}

let process_includes filename includes classes =
	(* Bring in each include  *)
	let processInclude include_statement = 
		let file_in = open_in include_statement in
		let lexbuf = Lexing.from_channel file_in in
		let token_list = Processor.build_token_list lexbuf in
		let program = Processor.parser include_statement token_list in
		ignore(close_in file_in);
		program
	in
	let rec iterate_includes classes m = function
			[] -> classes
		| (Include h) :: t -> 
			let h = if h = "stdlib" then Conf.stdlib_path else h in
			(* Check each include against the map *)
			let realpath = Filepath.realpath h in
			if StringMap.mem realpath m then 
				iterate_includes (classes) (m) (t)
			else 
				let result = processInclude realpath in 
				match result with Program(i,c) ->
				List.iter (fun x -> print_string(Utils.string_of_include x)) i;
				iterate_includes (classes @ c) (StringMap.add realpath 1 m) (i @ t)
	in
	iterate_includes classes (StringMap.add (Filepath.realpath filename) 1 StringMap.empty) includes

let get_name cname fdecl = 
	(* We use '.' to separate types so llvm will recognize the function name and it won't conflict *)
	let params = List.fold_left (fun s -> (function Formal(t, _) -> s ^ "." ^ Utils.string_of_datatype t | _ -> "" )) "" fdecl.formals in
	let name = Utils.string_of_fname fdecl.fname in
	if name = "main" 
		then "main"
		else cname ^ "." ^ name ^ params

let get_name_without_class fdecl = 
	(* We use '.' to separate types so llvm will recognize the function name and it won't conflict *)
	let params = List.fold_left (fun s -> (function Formal(t, _) -> s ^ "." ^ Utils.string_of_datatype t | _ -> "" )) "" fdecl.formals in
	let name = Utils.string_of_fname fdecl.fname in
    name ^ params

(* Generate list of all classes to be used for semantic checking *)
let build_class_maps reserved cdecls =
	let reserved_map = List.fold_left (fun m f -> StringMap.add (Utils.string_of_fname f.sfname) f m) StringMap.empty reserved in
	let helper m (cdecl:Ast.class_decl) =  
		let fieldfun = (fun m -> (function Field(s, d, n) -> if (StringMap.mem (n) m) then raise(Exceptions.DuplicateField) else (StringMap.add n (Field(s, d, n)) m))) in
		let funcname = get_name cdecl.cname in
		let funcfun m fdecl = 
			if (StringMap.mem (funcname fdecl) m) 
				then raise(Exceptions.DuplicateFunction(funcname fdecl)) 
			else if (StringMap.mem (Utils.string_of_fname fdecl.fname) reserved_map)
				then raise(Exceptions.CannotUseReservedFuncName(Utils.string_of_fname fdecl.fname))
			else (StringMap.add (funcname fdecl) fdecl m)	
		in
		let constructor_name = get_name cdecl.cname in
		let constructorfun m fdecl = 
			if fdecl.formals = [] then m
			else if StringMap.mem (constructor_name fdecl) m 
				then raise(Exceptions.DuplicateConstructor) 
				else (StringMap.add (constructor_name fdecl) fdecl m)
		in
		let default_c = default_c cdecl.cname in
		let constructor_map = StringMap.add (get_name cdecl.cname default_c) default_c StringMap.empty in
		(if (StringMap.mem cdecl.cname m) then raise (Exceptions.DuplicateClassName(cdecl.cname)) else
			StringMap.add cdecl.cname 
			{ 	field_map = List.fold_left fieldfun StringMap.empty cdecl.cbody.fields; 
				func_map = List.fold_left funcfun StringMap.empty cdecl.cbody.methods;
				constructor_map = List.fold_left constructorfun constructor_map cdecl.cbody.constructors; 
				reserved_map = reserved_map; 
				cdecl = cdecl } 
										 m) in
	List.fold_left helper StringMap.empty cdecls

let get_equality_binop_type type1 type2 se1 se2 op = 
		(* Equality op not supported for float operands. The correct way to test floats 
		   for equality is to check the difference between the operands in question *)
		if (type1 = Datatype(Float_t) || type2 = Datatype(Float_t)) then raise (Exceptions.InvalidBinopExpression "Equality operation is not supported for Float types")
		else 
		match type1, type2 with
			Datatype(Char_t), Datatype(Int_t) 
		| 	Datatype(Int_t), Datatype(Char_t)
		| 	Datatype(Objecttype(_)), Datatype(Null_t)
		| 	Datatype(Null_t), Datatype(Objecttype(_))
		| 	Datatype(Null_t), Arraytype(_, _)
		| 	Arraytype(_, _), Datatype(Null_t) -> SBinop(se1, op, se2, Datatype(Bool_t))
		| _ ->
			if type1 = type2 then SBinop(se1, op, se2, Datatype(Bool_t))
			else raise (Exceptions.InvalidBinopExpression "Equality operator can't operate on different types, with the exception of Int_t and Char_t")


let get_logical_binop_type se1 se2 op = function 
		(Datatype(Bool_t), Datatype(Bool_t)) -> SBinop(se1, op, se2, Datatype(Bool_t))
		| _ -> raise (Exceptions.InvalidBinopExpression "Logical operators only operate on Bool_t types")


let get_comparison_binop_type type1 type2 se1 se2 op =  
	let numerics = SS.of_list [Datatype(Int_t); Datatype(Char_t); Datatype(Float_t)]
	in
		if SS.mem type1 numerics && SS.mem type2 numerics
			then SBinop(se1, op, se2, Datatype(Bool_t))
		else raise (Exceptions.InvalidBinopExpression "Comparison operators operate on numeric types only")


let get_arithmetic_binop_type se1 se2 op = function 
			(Datatype(Int_t), Datatype(Float_t)) 
		| 	(Datatype(Float_t), Datatype(Int_t)) 
		| 	(Datatype(Float_t), Datatype(Float_t)) 	-> SBinop(se1, op, se2, Datatype(Float_t))

		| 	(Datatype(Int_t), Datatype(Char_t)) 
		| 	(Datatype(Char_t), Datatype(Int_t)) 
		| 	(Datatype(Char_t), Datatype(Char_t)) 	-> SBinop(se1, op, se2, Datatype(Char_t))

		| 	(Datatype(Int_t), Datatype(Int_t)) 		-> SBinop(se1, op, se2, Datatype(Int_t))

		| _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators don't support these types")

let rec get_ID_type env s = 
	try StringMap.find s env.env_locals
	with | Not_found -> 
	try let formal = StringMap.find s env.env_parameters in
		(function Formal(t, _) -> t | Many t -> t ) formal
	with | Not_found -> raise (Exceptions.UndefinedID s)

and check_array_primitive env el = 
	let rec iter t sel = function
		[] -> sel, t
	| 	e :: el -> 
		let se, _ = expr_to_sexpr env e in
		let se_t = get_type_from_sexpr se in
		if t = se_t 
			then iter t (se :: sel) el 
			else
				let t1 = Utils.string_of_datatype t in
				let t2 = Utils.string_of_datatype se_t in 
				raise(Exceptions.InvalidArrayPrimitiveConsecutiveTypes(t1, t2))
	in
	let se, _ = expr_to_sexpr env (List.hd el) in
	let el = List.tl el in
	let se_t = get_type_from_sexpr se in
	let sel, t = iter se_t ([se]) el in
	let se_t = match t with
					Datatype(x) -> Arraytype(x, 1)
				| 	Arraytype(x, n) -> Arraytype(x, n+1)
				| 	_ as t -> raise(Exceptions.InvalidArrayPrimitiveType(Utils.string_of_datatype t))
	in
	SArrayPrimitive(sel, se_t)

and check_array_init env d el = 
	(* Get dimension size for the array being created *)
	let array_complexity = List.length el in
	let check_elem_type e = 
		let sexpr, _ = expr_to_sexpr env e in
		let sexpr_type = get_type_from_sexpr sexpr in
		if sexpr_type = Datatype(Int_t) 
			then sexpr
			else raise(Exceptions.MustPassIntegerTypeToArrayCreate)
	in
	let convert_d_to_arraytype = function
		Datatype(x) -> Arraytype(x, array_complexity)
	| 	_ as t -> 
		let error_msg = Utils.string_of_datatype t in
		raise (Exceptions.ArrayInitTypeInvalid(error_msg))
	in
	let sexpr_type = convert_d_to_arraytype d in
	let sel = List.map check_elem_type el in
	SArrayCreate(d, sel, sexpr_type)

and check_array_access env e el = 
	(* Get dimensions of array, ex: foo[10][4][2] is dimen=3 *)
	let array_dimensions = List.length el in
	(* Check every e in el is of type Datatype(Int_t). Ensure all indices are ints *)
	let check_elem_type arg = 
		let sexpr, _ = expr_to_sexpr env arg in
		let sexpr_type = get_type_from_sexpr sexpr in
		if sexpr_type = Datatype(Int_t) 
			then sexpr
			else raise(Exceptions.MustPassIntegerTypeToArrayAccess)
	in
	(* converting e to se also checks if the array id has been declared  *)
	let se, _ = expr_to_sexpr env e in 
	let se_type = get_type_from_sexpr se in

	(* Check that e has enough dimens as e's in el. Return overall datatype of access*)
	let check_array_dim_vs_params num_params = function
		Arraytype(t, n) -> 
			if num_params < n then
				Arraytype(t, (n-num_params))
			else if num_params = n then
				Datatype(t)
			else
				raise (Exceptions.ArrayAccessInvalidParamLength(string_of_int num_params, string_of_int n))
	| 	_ as t -> 
		let error_msg = Utils.string_of_datatype t in
		raise (Exceptions.ArrayAccessExpressionNotArray(error_msg))
	in
	let sexpr_type = check_array_dim_vs_params array_dimensions se_type in
	let sel = List.map check_elem_type el in

	SArrayAccess(se, sel, sexpr_type)

and check_obj_access env lhs rhs = 
	let check_lhs = function
		This 			-> SId("this", Datatype(Objecttype(env.env_name)))
	|	Id s 			-> SId(s, get_ID_type env s)
	| 	_ as e 	-> raise (Exceptions.LHSofRootAccessMustBeIDorFunc (Utils.string_of_expr e))
	in
	let ptype_name parent_type = match parent_type with
			Datatype(Objecttype(name)) 	-> name
		| 	_ as d						-> raise (Exceptions.ObjAccessMustHaveObjectType (Utils.string_of_datatype d))
	in
	let rec check_rhs (env) parent_type (top_level_env) = 
		let pt_name = ptype_name parent_type in
		let get_id_type_from_object env (id) cname tlenv = 
			let cmap = StringMap.find cname env.env_class_maps in
			let match_field f = match f with
				Field(scope, d, n) -> 
					(* Have to update this with all parent classes checks *)
					if scope = Ast.Private && tlenv.env_name <> env.env_name then
						raise(Exceptions.CannotAccessPrivateFieldInNonProperScope(n, env.env_name, tlenv.env_name))
					else d
			in	
			try match_field (StringMap.find id cmap.field_map)
			with | Not_found -> raise (Exceptions.UnknownIdentifierForClass(id, cname))
		in
		function
			(* Check fields in parent *)
			Id s 				-> SId(s, (get_id_type_from_object env s pt_name top_level_env)), env
			(* Check functions in parent *)
		| 	Call(fname, el) 	-> 
				let env = update_env_name env pt_name in
				check_call_type top_level_env true env fname el, env
			(* Set parent, check if base is field *)
		| 	ObjAccess(e1, e2) 	-> 
				let old_env = env in
				let lhs, env = check_rhs env parent_type top_level_env e1 in
				let lhs_type = get_type_from_sexpr lhs in

				let pt_name = ptype_name lhs_type in
				let lhs_env = update_env_name env pt_name in

				let rhs, env = check_rhs lhs_env lhs_type top_level_env e2 in
				let rhs_type = get_type_from_sexpr rhs in
				SObjAccess(lhs, rhs, rhs_type), old_env
		| 	_ as e				-> raise (Exceptions.InvalidAccessLHS (Utils.string_of_expr e))
	in 
	let arr_lhs, _ = expr_to_sexpr env lhs in
	let arr_lhs_type = get_type_from_sexpr arr_lhs in
	match arr_lhs_type with
		Arraytype(Char_t, 1) -> raise(Exceptions.CannotAccessLengthOfCharArray)
	|	Arraytype(_, _) -> 
			let rhs = match rhs with
				Id("length") -> SId("length", Datatype(Int_t))
			| 	_ -> raise(Exceptions.CanOnlyAccessLengthOfArray)
			in
			SObjAccess(arr_lhs, rhs, Datatype(Int_t))
	| _ ->
		let lhs = check_lhs lhs in
		let lhs_type = get_type_from_sexpr lhs in 

		let ptype_name = ptype_name lhs_type in
		let lhs_env = update_env_name env ptype_name in

		let rhs, _ = check_rhs lhs_env lhs_type env rhs in
		let rhs_type = get_type_from_sexpr rhs in
		SObjAccess(lhs, rhs, rhs_type)

and check_call_type top_level_env isObjAccess env fname el = 
	let sel, env = exprl_to_sexprl env el in
	(* check that 'env.env_name' is in the list of defined classes *)
	let cmap = 
		try StringMap.find env.env_name env.env_class_maps
		with | Not_found -> raise (Exceptions.UndefinedClass env.env_name)
	in

	(* Add a reference to the class in front of the function call *)
	(* Must properly handle the case where this is a reserved function *)

	(* get a list of the types of the actuals to match against defined function formals *)
	let params = List.fold_left (fun s e -> s ^ "." ^ (Utils.string_of_datatype (get_type_from_sexpr e))) "" sel in
	let sfname = env.env_name ^ "." ^ fname ^ params in

	let index fdecl =
		let cdecl = cmap.cdecl in
		(* Have to update this with all parent classes checks *)
		let _ = 
			if fdecl.scope = Ast.Private && top_level_env.env_name <> env.env_name then
			raise(Exceptions.CannotAccessPrivateFunctionInNonProperScope(get_name env.env_name fdecl, env.env_name, top_level_env.env_name))
		in
		(* Not exactly sure why there needs to be a list.rev *)
		let fns = List.rev cdecl.cbody.methods in
		let rec find x lst =
		    match lst with
		    | [] -> raise (Failure ("Could not find " ^ sfname))
		    | fdecl :: t -> 
		    	let search_name = (get_name env.env_name fdecl) in
		    	if x = search_name then 0 
		    	else if search_name = "main" then find x t 
		    	else 1 + find x t
		in
		find sfname fns
	in

	try let fdecl = (StringMap.find sfname cmap.func_map) in
		let index = index fdecl in
		SCall(sfname, sel, fdecl.returnType, index)
	with | Not_found -> 
	if isObjAccess then raise (Exceptions.FunctionNotFound fname)
	else 
	try 
		let func = (StringMap.find fname cmap.reserved_map) in
		SCall(fname, sel, func.sreturnType, 0)
	with | Not_found -> raise (Exceptions.FunctionNotFound fname)	

and check_object_constructor env s el = 
	let sel, env = exprl_to_sexprl env el in
	(* check that 'env.env_name' is in the list of defined classes *)
	let cmap = 
		try StringMap.find s env.env_class_maps
		with | Not_found -> raise (Exceptions.UndefinedClass s)
	in
	(* get a list of the types of the actuals to match against defined function formals *)
	let params = List.fold_left (fun s e -> s ^ "." ^ (Utils.string_of_datatype (get_type_from_sexpr e))) "" sel in
	let constructor_name = s ^ "." ^ "constructor" ^ params in
	let _ = 
		try StringMap.find constructor_name cmap.constructor_map
		with | Not_found -> raise (Exceptions.ConstructorNotFound constructor_name)
	in
	let ftype = Datatype(Objecttype(s)) in
	(* Add a reference to the class in front of the function call *)
	(* Must properly handle the case where this is a reserved function *)
	SObjectCreate(constructor_name, sel, ftype)

and check_assign env e1 e2 = 
	let se1, env = expr_to_sexpr env e1 in
	let se2, env = expr_to_sexpr env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in 
	match (type1, se2) with
		Datatype(Objecttype(_)), SNull 
	| 	Arraytype(_, _), SNull -> SAssign(se1, se2, type1)
	|   _ -> 
	match type1, type2 with
		Datatype(Char_t), Datatype(Int_t)
	| 	Datatype(Int_t), Datatype(Char_t) -> SAssign(se1, se2, type1)
	| _ -> 
	if type1 = type2 
		then SAssign(se1, se2, type1)
		else raise (Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))

and check_unop env op e = 
	let check_num_unop t = function
			Sub 	-> t
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in 
	let check_bool_unop = function
			Not 	-> Datatype(Bool_t)
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let se, env = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	match t with 
		Datatype(Int_t) 	
	|	Datatype(Float_t) 	-> SUnop(op, se, check_num_unop t op)
	|  	Datatype(Bool_t) 	-> SUnop(op, se, check_bool_unop op)
	| 	_ -> raise(Exceptions.InvalidUnaryOperation)

and check_binop env e1 op e2 =
	let se1, env = expr_to_sexpr env e1 in
	let se2, env = expr_to_sexpr env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq -> get_comparison_binop_type type1 type2 se1 se2 op
	| Add | Mult | Sub | Div | Mod -> get_arithmetic_binop_type se1 se2 op (type1, type2) 
	| _ -> raise (Exceptions.InvalidBinopExpression ((Utils.string_of_op op) ^ " is not a supported binary op"))

and check_delete env e = 
	let se, _ = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	match t with
		Arraytype(_, _) | Datatype(Objecttype(_)) -> SDelete(se)
	| 	_ -> raise(Exceptions.CanOnlyDeleteObjectsOrArrays)

and expr_to_sexpr env = function
		Int_Lit i           -> SInt_Lit(i), env
	|   Boolean_Lit b       -> SBoolean_Lit(b), env
	|   Float_Lit f         -> SFloat_Lit(f), env
	|   String_Lit s        -> SString_Lit(s), env
	|   Char_Lit c          -> SChar_Lit(c), env
	|   This                -> SId("this", Datatype(Objecttype(env.env_name))), env
	|   Id s                -> SId(s, get_ID_type env s), env
	|   Null                -> SNull, env
	|   Noexpr              -> SNoexpr, env

	|   ObjAccess(e1, e2)   -> check_obj_access env e1 e2, env
	|   ObjectCreate(s, el) -> check_object_constructor env s el, env
	|   Call(s, el)         -> check_call_type env false env s el, env

	|   ArrayCreate(d, el)  -> check_array_init env d el, env
	|   ArrayAccess(e, el)  -> check_array_access env e el, env
	|   ArrayPrimitive el   -> check_array_primitive env el, env

	|   Assign(e1, e2)      -> check_assign env e1 e2, env
	|   Unop(op, e)         -> check_unop env op e, env
	|   Binop(e1, op, e2)   -> check_binop env e1 op e2, env
	| 	Delete(e) 			-> check_delete env e, env


and get_type_from_sexpr = function
		SInt_Lit(_)				-> Datatype(Int_t)
	| 	SBoolean_Lit(_)			-> Datatype(Bool_t)
	| 	SFloat_Lit(_)			-> Datatype(Float_t)
	| 	SString_Lit(_) 			-> Arraytype(Char_t, 1)
	| 	SChar_Lit(_) 			-> Datatype(Char_t)
	| 	SId(_, d) 				-> d
	| 	SBinop(_, _, _, d) 		-> d
	| 	SAssign(_, _, d) 		-> d
	| 	SNoexpr 				-> Datatype(Void_t)
	| 	SArrayCreate(_, _, d)	-> d
	| 	SArrayAccess(_, _, d) 	-> d
	| 	SObjAccess(_, _, d)		-> d
	| 	SCall(_, _, d, _)		-> d
	|   SObjectCreate(_, _, d) 	-> d
	| 	SArrayPrimitive(_, d)	-> d
	|  	SUnop(_, _, d) 			-> d
	| 	SNull					-> Datatype(Null_t)
	| 	SDelete _ 				-> Datatype(Void_t)

and exprl_to_sexprl env el =
  let env_ref = ref(env) in
  let rec helper = function
	  head::tail ->
		let a_head, env = expr_to_sexpr !env_ref head in
		env_ref := env;
		a_head::(helper tail)
	| [] -> []
  in (helper el), !env_ref

let rec get_all_descendants cname accum = 
    if Hashtbl.mem predecessors cname then
        let direct_descendants = Hashtbl.find predecessors cname in
        let add_childs_descendants desc_set direct_descendant = get_all_descendants direct_descendant (StringSet.add direct_descendant desc_set)
    in
    List.fold_left add_childs_descendants accum direct_descendants
    else accum
    
let inherited potential_predec potential_child = 
    match potential_predec, potential_child with 
    Datatype(Objecttype(predec_cname)), Datatype(Objecttype(child_cname)) -> 
        let descendants = get_all_descendants predec_cname StringSet.empty in
        if (predec_cname = child_cname) || (StringSet.mem child_cname descendants) then true else raise (Exceptions.LocalAssignTypeMismatch(predec_cname, child_cname))
    | _ , _ -> false


let rec local_handler d s e env = 
	if StringMap.mem s env.env_locals 
		then raise (Exceptions.DuplicateLocal s)
		else
			let se, env = expr_to_sexpr env e in
			let t = get_type_from_sexpr se in
			if t = Datatype(Void_t) || t = Datatype(Null_t) || t = d || (inherited d t) 
				then
				let new_env = {
					env_class_maps = env.env_class_maps;
					env_name = env.env_name;
					env_cmap = env.env_cmap;
					env_locals = StringMap.add s d env.env_locals;
					env_parameters = env.env_parameters;
					env_returnType = env.env_returnType;
					env_in_for = env.env_in_for;
					env_in_while = env.env_in_while;
					env_reserved = env.env_reserved;
				} in 
(* if the user-defined type being declared is not in global classes map, it is an undefined class *)
				(match d with
					Datatype(Objecttype(x)) -> 
						(if not (StringMap.mem (Utils.string_of_object d) env.env_class_maps) 
							then raise (Exceptions.UndefinedClass (Utils.string_of_object d)) 
							else
                                let local = if inherited d t then SLocal(t, s, se) else SLocal(d, s, se)
                                in local, new_env)
				| _ -> SLocal(d, s, se), new_env)
			else 
				(let type1 = (Utils.string_of_datatype t) in
				let type2 = (Utils.string_of_datatype d) in
				let ex = Exceptions.LocalAssignTypeMismatch(type1, type2) in
				raise ex)

let rec check_sblock sl env = match sl with
		[] -> SBlock([SExpr(SNoexpr, Datatype(Void_t))]), env
	| 	_  -> 
		let sl, _ = convert_stmt_list_to_sstmt_list env sl in
		SBlock(sl), env

and check_expr_stmt e env = 
	let se, env = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in 
	SExpr(se, t), env

and check_return e env = 
	let se, _ = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	if t = env.env_returnType 
		then SReturn(se, t), env
		else raise Exceptions.ReturnTypeMismatch

and check_if e s1 s2 env = 
	let se, _ = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	let ifbody, _ = parse_stmt env s1 in
	let elsebody, _ = parse_stmt env s2 in
	if t = Datatype(Bool_t) 
		then SIf(se, ifbody, elsebody), env
		else raise Exceptions.InvalidIfStatementType

and check_for e1 e2 e3 s env = 
	let old_val = env.env_in_for in
	let env = update_call_stack env true env.env_in_while in

	let se1, _ = expr_to_sexpr env e1 in
	let se2, _ = expr_to_sexpr env e2 in
	let se3, _ = expr_to_sexpr env e3 in
	let forbody, _ = parse_stmt env s in
	let conditional = get_type_from_sexpr se2 in
	let sfor = 
		if (conditional = Datatype(Bool_t) || conditional = Datatype(Void_t))
			then SFor(se1, se2, se3, forbody)
			else raise Exceptions.InvalidForStatementType
	in

	let env = update_call_stack env old_val env.env_in_while in
	sfor, env

and check_while e s env = 
	let old_val = env.env_in_while in
	let env = update_call_stack env env.env_in_for true in

	let se, _ = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	let sstmt, _ = parse_stmt env s in 
	let swhile = 
		if (t = Datatype(Bool_t) || t = Datatype(Void_t)) 
			then SWhile(se, sstmt)
			else raise Exceptions.InvalidWhileStatementType
	in

	let env = update_call_stack env env.env_in_for old_val in
	swhile, env

and check_break env = 
	if env.env_in_for || env.env_in_while then
		SBreak, env
	else
		raise Exceptions.CannotCallBreakOutsideOfLoop

and check_continue env = 
	if env.env_in_for || env.env_in_while then
		SContinue, env
	else
		raise Exceptions.CannotCallContinueOutsideOfLoop

and parse_stmt env = function
		Block sl 				-> check_sblock sl env
	| 	Expr e 					-> check_expr_stmt e env
	| 	Return e 				-> check_return e env
	| 	If(e, s1, s2) 			-> check_if e s1 s2	env
	| 	For(e1, e2, e3, e4) 	-> check_for e1 e2 e3 e4 env	
	| 	While(e, s)				-> check_while e s env
	|  	Break 					-> check_break env (* Need to check if in right context *)
	|   Continue 				-> check_continue env (* Need to check if in right context *)
	|   Local(d, s, e) 			-> local_handler d s e env

(* Update this function to return an env object *)
and convert_stmt_list_to_sstmt_list env stmt_list = 
	let env_ref = ref(env) in
	let rec iter = function
	  head::tail ->
		let a_head, env = parse_stmt !env_ref head in
		env_ref := env;
		a_head::(iter tail)
	| [] -> []
	in 
	let sstmt_list = (iter stmt_list), !env_ref in
	sstmt_list

let append_code_to_main fbody cname ret_type = 
	let key = Hashtbl.find struct_indexes cname in 
	let init_this = [SLocal(
		ret_type,
		"this",
		SCall(	"cast", 
				[SCall("malloc", 
					[	
						SCall("sizeof", [SId("ignore", ret_type)], Datatype(Int_t), 0)
					], 
					Arraytype(Char_t, 1), 0)
				],
				ret_type, 0
			)
		);
		SExpr(
			SAssign(
				SObjAccess(
					SId("this", ret_type),
					SId(".key", Datatype(Int_t)),
					Datatype(Int_t)
				),
				SInt_Lit(key),
				Datatype(Int_t)
			),
			Datatype(Int_t)
		)
	]
	in 
	init_this @ fbody

let convert_constructor_to_sfdecl class_maps reserved class_map cname constructor = 
	let env = {
		env_class_maps 	= class_maps;
		env_name     	= cname;
		env_cmap 		= class_map;
		env_locals    	= StringMap.empty;
		env_parameters	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m) | _ -> m) StringMap.empty constructor.formals;
		env_returnType	= Datatype(Objecttype(cname));
		env_in_for 		= false;
		env_in_while 	= false;
		env_reserved 	= reserved;
	} in 
	let fbody = fst (convert_stmt_list_to_sstmt_list env constructor.body) in
	{
		sfname 		= Ast.FName (get_name cname constructor);
		sreturnType = Datatype(Objecttype(cname));
		sformals 	= constructor.formals;
		sbody 		= append_code_to_constructor fbody cname (Datatype(Objecttype(cname)));
		func_type	= Sast.User;
		overrides 	= false;
		source 		= "NA";
	}

let check_fbody fname fbody returnType =
	let final_stmt = List.hd (List.rev fbody) in
	match returnType, final_stmt with
		Datatype(Void_t), _ -> ()
	| 	_, SReturn(_, _) -> ()
	| 	_ -> raise(Exceptions.AllNonVoidFunctionsMustEndWithReturn(fname))

let convert_fdecl_to_sfdecl class_maps reserved class_map cname fdecl = 
	let root_cname = match fdecl.root_cname with 
        Some(x) -> x
        | None -> cname
    in
    let class_formal = 
    	if fdecl.overrides then 
    		Ast.Formal(Datatype(Objecttype(root_cname)), "this")
    	else 
    		Ast.Formal(Datatype(Objecttype(cname)), "this")
    in
	let env_param_helper m fname = match fname with 
			Formal(d, s) -> (StringMap.add s fname m) 
		| 	_ -> m
	in
	let env_params = List.fold_left env_param_helper StringMap.empty (class_formal :: fdecl.formals) in
	let env = {
		env_class_maps 	= class_maps;
		env_name     	= cname;
		env_cmap 		= class_map;
		env_locals    	= StringMap.empty;
		env_parameters	= env_params;
		env_returnType	= fdecl.returnType;
		env_in_for 		= false;
		env_in_while 	= false;
		env_reserved 	= reserved;
	} 
	in
	let fbody = fst (convert_stmt_list_to_sstmt_list env fdecl.body) in
	let fname = (get_name cname fdecl) in
	ignore(check_fbody fname fbody fdecl.returnType);
	let fbody = if fname = "main" 
		then (append_code_to_main fbody cname (Datatype(Objecttype(cname)))) 
		else fbody 
	in
	(* We add the class as the first parameter to the function for codegen *)
	{
		sfname 			= Ast.FName (get_name cname fdecl);
		sreturnType 	= fdecl.returnType;
		sformals 		= class_formal :: fdecl.formals;
		sbody 			= fbody;
		func_type		= Sast.User;
		overrides       = fdecl.overrides;
		source 			= cname;
	}

let convert_cdecl_to_sast sfdecls (cdecl:Ast.class_decl) = 
	{
		scname = cdecl.cname;
		sfields = cdecl.cbody.fields;
		sfuncs = sfdecls;
	}

(* 
 * Given a list of func_decls for the base class and a single func_decl
 * for the child class, replaces func_decls for the base class if any of them 
 * have the same method signature 
 *)
let replace_fdecl_in_base_methods base_cname base_methods child_fdecl = 
	let replace base_fdecl accum = 
		let get_root_cname = function
			None -> Some(base_cname)
			| Some(x) -> Some(x)
		in
		let modify_child_fdecl = 
			{
				scope = child_fdecl.scope;
				fname = child_fdecl.fname;
				returnType = child_fdecl.returnType;
				formals = child_fdecl.formals;
				body = child_fdecl.body;
				overrides = true;
				root_cname = get_root_cname base_fdecl.root_cname;
			} 
		in
		if (get_name_without_class base_fdecl) = (get_name_without_class child_fdecl) 
			then modify_child_fdecl::accum 
			else base_fdecl::accum
	in
	List.fold_right replace base_methods []

let merge_methods base_cname base_methods child_methods =
	let check_overrides child_fdecl accum = 
		let base_checked_for_overrides = 
			replace_fdecl_in_base_methods base_cname (fst accum) child_fdecl 
		in
		if (fst accum) = base_checked_for_overrides
			then ((fst accum), child_fdecl::(snd accum)) 
			else (base_checked_for_overrides, (snd accum))
	in
	let updated_base_and_child_fdecls = 
		List.fold_right check_overrides child_methods (base_methods, [])
	in
	(fst updated_base_and_child_fdecls) @ (snd updated_base_and_child_fdecls)

let merge_cdecls base_cdecl child_cdecl = 
(* return a cdecl in which cdecl.cbody.fields contains the fields of 
the extended class, concatenated by the fields of the child class *)
	let child_cbody = 
		{
			fields = base_cdecl.cbody.fields @ child_cdecl.cbody.fields;
			 constructors = child_cdecl.cbody.constructors;
			 methods = merge_methods base_cdecl.cname base_cdecl.cbody.methods child_cdecl.cbody.methods
		}
		in
		{
			cname = child_cdecl.cname;
			extends = child_cdecl.extends;
			cbody = child_cbody
		}

(* returns a list of cdecls that contains inherited fields *)
let inherit_fields_cdecls cdecls inheritance_forest = 
	(* iterate through cdecls to make a map for lookup *)
	let cdecl_lookup = List.fold_left (fun a litem -> StringMap.add litem.cname litem a) StringMap.empty cdecls in
	let add_key key pred maps = 
		let elem1 = StringSet.add key (fst maps) in
		let accum acc child = StringSet.add child acc in
		let elem2 = List.fold_left (accum) (snd maps) pred in
		(elem1, elem2)
	in
	let empty_s = StringSet.empty in
	let res = StringMap.fold add_key inheritance_forest (empty_s, empty_s) in
	let roots = StringSet.diff (fst res) (snd res) in
	let rec add_inherited_fields predec desc map_to_update = 
		let merge_fields accum descendant = 
			let updated_predec_cdecl = StringMap.find predec accum in 
			let descendant_cdecl_to_update = StringMap.find descendant cdecl_lookup in
			let merged = merge_cdecls updated_predec_cdecl descendant_cdecl_to_update in 
			let updated = (StringMap.add descendant merged accum) in 
			if (StringMap.mem descendant inheritance_forest) then 
				let descendants_of_descendant = StringMap.find descendant inheritance_forest in
				add_inherited_fields descendant descendants_of_descendant updated
			else updated
		in
		List.fold_left merge_fields map_to_update desc
	in
	(* map class name of every class_decl in `cdecls` to its inherited cdecl *)
	let inherited_cdecls = 
		let traverse_tree tree_root accum = 
			let tree_root_descendant = StringMap.find tree_root inheritance_forest in 
			let accum_with_tree_root_mapping = StringMap.add tree_root (StringMap.find tree_root cdecl_lookup) accum in
			add_inherited_fields tree_root tree_root_descendant accum_with_tree_root_mapping
		in
		StringSet.fold traverse_tree roots StringMap.empty 
	in
	(* build a list of updated cdecls corresponding to the sequence of cdecls in `cdecls` *)
	let add_inherited_cdecl cdecl accum = 
		let inherited_cdecl = 
			try StringMap.find cdecl.cname inherited_cdecls 
			with | Not_found -> cdecl
		in
		inherited_cdecl::accum
	in
	let result = List.fold_right add_inherited_cdecl cdecls [] in
	result

let convert_cdecls_to_sast class_maps reserved (cdecls:Ast.class_decl list) = 
	let find_main = (fun f -> match f.sfname with FName n -> n = "main" | _ -> false) in
	let get_main func_list = 
		let mains = (List.find_all find_main func_list) in
		if List.length mains < 1 then 
			raise Exceptions.MainNotDefined 
		else if List.length mains > 1 then 
			raise Exceptions.MultipleMainsDefined 
		else List.hd mains 
	in
	let remove_main func_list = 
		List.filter (fun f -> not (find_main f)) func_list 
	in
	let find_default_constructor cdecl clist = 
		let default_cname = cdecl.cname ^ "." ^ "constructor" in
		let find_default_c f =
			match f.sfname with FName n -> n = default_cname | _ -> false
		in
		try let _ = List.find find_default_c clist in
			clist
		with | Not_found -> 
			let default_c = default_sc cdecl.cname in
			default_c :: clist
	in
	let handle_cdecl cdecl = 
		let class_map = StringMap.find cdecl.cname class_maps in 
		let sconstructor_list = List.fold_left (fun l c -> (convert_constructor_to_sfdecl class_maps reserved class_map cdecl.cname c) :: l) [] cdecl.cbody.constructors in
		let sconstructor_list = find_default_constructor cdecl sconstructor_list in
		let func_list = List.fold_left (fun l f -> (convert_fdecl_to_sfdecl class_maps reserved class_map cdecl.cname f) :: l) [] cdecl.cbody.methods in
		let sfunc_list = remove_main func_list in
		let scdecl = convert_cdecl_to_sast sfunc_list cdecl in
		(scdecl, func_list @ sconstructor_list)
	in 
	let iter_cdecls t c = 
		let scdecl = handle_cdecl c in 
		(fst scdecl :: fst t, snd scdecl @ snd t)
	in
	let scdecl_list, func_list = List.fold_left iter_cdecls ([], []) cdecls in
	let main = get_main func_list in
	let funcs = remove_main func_list in
	(* let funcs = (add_default_constructors cdecls class_maps) @ funcs in *)
	{
		classes 		= scdecl_list;
		functions 		= funcs;
		main 			= main;
		reserved 		= reserved;
	}

let add_reserved_functions = 
	let reserved_stub name return_type formals = 
		{
			sfname 			= FName(name);
			sreturnType 	= return_type;
			sformals 		= formals;
			sbody 			= [];
			func_type		= Sast.Reserved;
			overrides 		= false;
			source 			= "NA";
		}
	in
	let i32_t = Datatype(Int_t) in
	let void_t = Datatype(Void_t) in
	let str_t = Arraytype(Char_t, 1) in
	let mf t n = Formal(t, n) in (* Make formal *)
	let reserved = [
		reserved_stub "print" 	(void_t) 	([Many(Any)]);
		reserved_stub "malloc" 	(str_t) 	([mf i32_t "size"]);
		reserved_stub "cast" 	(Any) 		([mf Any "in"]);
		reserved_stub "sizeof" 	(i32_t) 	([mf Any "in"]);
		reserved_stub "open" 	(i32_t) 	([mf str_t "path"; mf i32_t "flags"]);
		reserved_stub "close" 	(i32_t) 	([mf i32_t "fd"]);
		reserved_stub "read" 	(i32_t) 	([mf i32_t "fd"; mf str_t "buf"; mf i32_t "nbyte"; mf i32_t "offset"]);
		reserved_stub "write" 	(i32_t) 	([mf i32_t "fd"; mf str_t "buf"; mf i32_t "nbyte"]);
		reserved_stub "lseek" 	(i32_t) 	([mf i32_t "fd"; mf i32_t "offset"; mf i32_t "whence"]);
		reserved_stub "exit" 	(void_t) 	([mf i32_t "status"]);
        reserved_stub "getchar" (void_t)    ([ ]);
	] in
	reserved

let build_inheritance_forest cdecls cmap = 
	let handler a cdecl =
		match cdecl.extends with 
			Parent(s) 	-> 
				let new_list = if (StringMap.mem s a) then
					cdecl.cname::(StringMap.find s a)
				else
					[cdecl.cname]
				in
				Hashtbl.add predecessors s new_list; 
				(StringMap.add s new_list a) 
		| 	NoParent 	-> a
	in
	let forest = List.fold_left handler StringMap.empty cdecls in

	let handler key value = 
		if not (StringMap.mem key cmap) then 
			raise (Exceptions.UndefinedClass key)
	in
	ignore(StringMap.iter handler forest);
	forest

let merge_maps m1 m2 = 
	StringMap.fold (fun k v a -> StringMap.add k v a) m1 m2

let update_class_maps map_type cmap_val cname cmap_to_update = 
	let update m map_type = 
		if map_type = "field_map" then
			{
				field_map = cmap_val;
				func_map = m.func_map;
				constructor_map = m.constructor_map;
				reserved_map = m.reserved_map;
				cdecl = m.cdecl;
			}
		else m
	in
	let updated = StringMap.find cname cmap_to_update in
	let updated = update updated map_type in
	let updated = StringMap.add cname updated cmap_to_update in
	updated

let inherit_fields class_maps predecessors =
	(* Get basic inheritance map *)
	let add_key key pred map = StringMap.add key pred map in
	let cmaps_inherit = StringMap.fold add_key class_maps StringMap.empty in
	(* Perform accumulation of child classes *)
	let add_key key pred maps = 
		let elem1 = StringSet.add key (fst maps) in
		let accum acc child = StringSet.add child acc in
		let elem2 = List.fold_left (accum) (snd maps) pred in
		(elem1, elem2)
	in
	let empty_s = StringSet.empty in
	let res = StringMap.fold add_key predecessors (empty_s, empty_s) in
	let roots = StringSet.diff (fst res) (snd res) in
	(*in let _ = print_set_members roots*)
	let rec add_inherited_fields predec desc cmap_to_update = 
		let cmap_inherit accum descendant = 
			let predec_field_map = (StringMap.find predec accum).field_map in
			let desc_field_map = (StringMap.find descendant accum).field_map in 
			let merged = merge_maps predec_field_map desc_field_map in 
			let updated = update_class_maps "field_map" merged descendant accum in
			if (StringMap.mem descendant predecessors) then 
				let descendants_of_descendant = StringMap.find descendant predecessors in
				add_inherited_fields descendant descendants_of_descendant updated 
			else updated
		in
		List.fold_left cmap_inherit cmap_to_update desc
		(* end of add_inherited_fields *)
	in 
	let result = StringSet.fold (fun x a -> add_inherited_fields x (StringMap.find x predecessors) a) roots cmaps_inherit
	(*in let _ = print_map result*)
	in result

(* TODO Check that this actually works *)
let check_cyclical_inheritance cdecls predecessors = 
	let handle_predecessor cdecl parent predecessor =
		if cdecl.cname = predecessor then
			raise(Exceptions.CyclicalDependencyBetween(cdecl.cname, parent))
	in
	let handle_cdecl cdecl = 
		if StringMap.mem cdecl.cname predecessors
			then 
				let pred_list = StringMap.find cdecl.cname predecessors in
				List.iter (handle_predecessor cdecl (List.hd pred_list)) pred_list
			else ()
	in
	List.iter handle_cdecl cdecls

let build_func_map_inherited_lookup cdecls_inherited = 
	let build_func_map cdecl =
		let add_func m fdecl = StringMap.add (get_name cdecl.cname fdecl) fdecl m in
		List.fold_left add_func StringMap.empty cdecl.cbody.methods
	in
	let add_class_func_map m cdecl = StringMap.add cdecl.cname (build_func_map cdecl) m in
	List.fold_left add_class_func_map StringMap.empty cdecls_inherited

let add_inherited_methods cmaps func_maps_inherited = 
	let update_with_inherited_methods cname cmap = 
		let fmap = StringMap.find cname func_maps_inherited in
		{
			field_map = cmap.field_map;
			func_map = fmap;
			constructor_map = cmap.constructor_map;
			reserved_map = cmap.reserved_map;
			cdecl = cmap.cdecl;
		}
	in
	let add_updated_cmap cname cmap accum = StringMap.add cname (update_with_inherited_methods cname cmap) accum in
	StringMap.fold add_updated_cmap cmaps StringMap.empty

let handle_inheritance cdecls class_maps = 
	let predecessors = build_inheritance_forest cdecls class_maps in
	let cdecls_inherited = inherit_fields_cdecls cdecls predecessors in
	let func_maps_inherited = build_func_map_inherited_lookup cdecls_inherited in
	ignore(check_cyclical_inheritance cdecls predecessors);
	let cmaps_with_inherited_fields = inherit_fields class_maps predecessors in
	let cmaps_inherited = add_inherited_methods cmaps_with_inherited_fields func_maps_inherited in
	cmaps_inherited, cdecls_inherited

let generate_struct_indexes cdecls = 
	let cdecl_handler index cdecl = 
		Hashtbl.add struct_indexes cdecl.cname index
	in
	List.iteri cdecl_handler cdecls

(* Main method for analyzer *)
let analyze filename program = match program with
	Program(includes, classes) ->
	(* Include code from external files *)
	let cdecls = process_includes filename includes classes in
	ignore(generate_struct_indexes cdecls);

	(* Add built-in functions *)
	let reserved = add_reserved_functions in
	(* Generate the class_maps for look up in checking functions *)
	let class_maps = build_class_maps reserved cdecls in
	let class_maps, cdecls = handle_inheritance cdecls class_maps in
	let sast = convert_cdecls_to_sast class_maps reserved cdecls in
	sast
