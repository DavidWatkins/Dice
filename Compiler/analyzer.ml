open Sast
open Ast
open Processor
open Utils
open Filepath

module StringMap = Map.Make (String)

module StringSet = Set.Make (String)

module SS = Set.Make(
    struct
        let compare = Pervasives.compare
        type t = datatype
    end )

module TM = Map.Make(
    struct
        let compare = Pervasives.compare
        type t = op
    end )

module Type_to_String = Map.Make(
    struct
        let compare = Pervasives.compare
        type t = datatype
    end )

type class_map = {
		field_map       : Ast.field StringMap.t;
		func_map        : Ast.func_decl StringMap.t;
		constructor_map : Ast.func_decl StringMap.t;
		reserved_map 	: sfunc_decl StringMap.t;
}

type env = {
		env_class_maps: class_map StringMap.t;
		env_name      : string;
		env_cmap 	  : class_map;
		env_locals    : datatype StringMap.t;
		env_parameters: Ast.formal StringMap.t;
		env_returnType: datatype;
		env_callStack : stmt list;
		env_reserved  : sfunc_decl list;
}

let print_keys m = 
StringMap.iter (fun key value -> print_string (key ^ "\n")) m


let print_map m = StringMap.iter (fun k v -> print_string ("\n" ^ k ^ "\n"); print_keys v.field_map) m

let construct_env cmaps cname cmap locals parameters returnType callStack reserved = 
{
	env_class_maps = cmaps;
	env_name       = cname;
	env_cmap 	   = cmap;
	env_locals     = locals;
	env_parameters = parameters;
	env_returnType = returnType;
	env_callStack  = callStack;
	env_reserved   = reserved;
}

let process_includes filename includes classes =
	(* Bring in each include  *)
	let processInclude include_statement = 
		let file_in = open_in filename in
		let lexbuf = Lexing.from_channel file_in in
		let token_list = Processor.build_token_list lexbuf in
		let program = Processor.parser include_statement token_list in
		program
	in
	let rec iterate_includes classes m = function
			[] -> classes
		| (Include h) :: t -> 
			(* Check each include against the map *)
			let realpath = Filepath.realpath h in
			let result = processInclude realpath in 
			if StringMap.mem h m then 
				iterate_includes (classes) (m) (t)
			else 
				(function Program(i, c) -> iterate_includes (classes @ c) (StringMap.add realpath 1 m) (i @ t) ) result
	in
	iterate_includes classes (StringMap.add (Filepath.realpath filename) 1 StringMap.empty) includes

let get_name cname fdecl = 
	(* We use '.' to separate types so llvm will recognize the function name and it won't conflict *)
	let params = List.fold_left (fun s -> (function Formal(t, _) -> s ^ "." ^ Utils.string_of_datatype t | _ -> "" )) "" fdecl.formals in
	let name = Utils.string_of_fname fdecl.fname in
	if name = "main" 
		then "main"
		else cname ^ "." ^ name ^ params

(* Generate list of all classes to be used for semantic checking *)
let build_class_maps reserved cdecls =
		let reserved_map = List.fold_left (fun m f -> StringMap.add (Utils.string_of_fname f.sfname) f m) StringMap.empty reserved in
		(* helper global_obj cdecls *)
		let helper m (cdecl:Ast.class_decl) =  
			let fieldfun = (fun m -> (function Field(s, d, n) -> if (StringMap.mem (n) m) then raise(Exceptions.DuplicateField) else (StringMap.add n (Field(s, d, n)) m))) in
			let funcname = get_name cdecl.cname in
			let funcfun = 
				(fun m fdecl -> 
					if (StringMap.mem (funcname fdecl) m) 
						then raise(Exceptions.DuplicateFunction(funcname fdecl)) 
					else if (StringMap.mem (Utils.string_of_fname fdecl.fname) reserved_map)
						then raise(Exceptions.CannotUseReservedFuncName(Utils.string_of_fname fdecl.fname))
					else (StringMap.add (funcname fdecl) fdecl m)) 
			in
			let constructor_name = get_name cdecl.cname in
			let constructorfun = (fun m fdecl -> 
									if StringMap.mem (constructor_name fdecl) m 
										then raise(Exceptions.DuplicateConstructor) 
										else (StringMap.add (constructor_name fdecl) fdecl m)) 
			in
			(if (StringMap.mem cdecl.cname m) then raise (Exceptions.DuplicateClassName) else
				StringMap.add cdecl.cname 
						{ field_map = List.fold_left fieldfun StringMap.empty cdecl.cbody.fields; 
							func_map = List.fold_left funcfun StringMap.empty cdecl.cbody.methods;
							constructor_map = List.fold_left constructorfun StringMap.empty cdecl.cbody.constructors; 
							reserved_map = reserved_map;} 
											 m) in
		List.fold_left helper StringMap.empty cdecls

let get_equality_binop_type type1 type2 se1 se2 op = 
        (* Equality op not supported for float operands. The correct way to test floats 
           for equality is to check the difference between the operands in question *)
	    if (type1 = Datatype(Float_t) || type2 = Datatype(Float_t)) then raise (Exceptions.InvalidBinopExpression "Equality operation is not supported for Float types")
        else if (type1 = Datatype(Char_t) && type2 = Datatype(Int_t) || 
                type1 = Datatype(Int_t) && type2 = Datatype(Char_t) || 
                type1 = type2) then SBinop(se1, op, se2, Datatype(Bool_t))
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

and check_array_primitive env el = SInt_Lit(0, Datatype(Int_t))

and check_array_init env d el = SInt_Lit(0, Datatype(Int_t))

and check_array_access e el = SInt_Lit(0, Datatype(Int_t))

and check_obj_access env lhs rhs = 
let _ = print_map env.env_class_maps in
	let check_lhs = function
		This 			-> SId("this", Datatype(Objecttype(env.env_name)))
	|	Id s 			-> SId(s, get_ID_type env s)
	(* | 	Call(fname, el) -> check_call_type env fname el *)
	| 	_ as e 	-> raise (Exceptions.LHSofRootAccessMustBeIDorFunc (Utils.string_of_expr e))
	in
	let rec check_rhs env parent_type = 
		let ptype_name = match parent_type with
			Datatype(Objecttype(name)) 	-> name
		| 	_ as d						-> raise (Exceptions.ObjAccessMustHaveObjectType (Utils.string_of_datatype d))
		in 
		let get_id_type_from_object env id cname = 
            let _ = print_string cname in
            let _ = print_map env.env_class_maps in
			let cmap = StringMap.find cname env.env_class_maps in
			try (function Field(_, d, _) -> d) (StringMap.find id cmap.field_map)
			with | Not_found -> raise (Exceptions.UnknownIdentifierForClass(id, cname))
		in
		function
			(* Check fields in parent *)
			Id s 				-> SId(s, get_id_type_from_object env s ptype_name), env
			(* Check functions in parent *)
		| 	Call(fname, el) 	-> 
				let env = construct_env env.env_class_maps ptype_name env.env_cmap env.env_locals env.env_parameters env.env_returnType env.env_callStack env.env_reserved in
				check_call_type env fname el, env
			(* Set parent, check if base is field *)
		| 	ObjAccess(e1, e2) 	-> 
				let old_env = env in
				let lhs, env = check_rhs env parent_type e1 in
				let lhs_type = get_type_from_sexpr lhs in
				let rhs, env = check_rhs env lhs_type e2 in
				let rhs_type = get_type_from_sexpr rhs in
				SObjAccess(lhs, rhs, rhs_type), old_env
		| 	_ as e				-> raise (Exceptions.InvalidAccessLHS (Utils.string_of_expr e))
	in 
	let lhs = check_lhs lhs in
	let lhs_type = get_type_from_sexpr lhs in 
	let rhs, _ = check_rhs env lhs_type rhs in
	let rhs_type = get_type_from_sexpr rhs in
	SObjAccess(lhs, rhs, rhs_type)

and check_call_type env fname el = 
	let sel, env = exprl_to_sexprl env el in
	(* check that 'env.env_name' is in the list of defined classes *)
	let cmap = 
		try StringMap.find env.env_name env.env_class_maps
		with | Not_found -> raise (Exceptions.UndefinedClass env.env_name)
	in
	(* get a list of the types of the actuals to match against defined function formals *)
	let params = List.fold_left (fun s e -> s ^ "." ^ (Utils.string_of_datatype (get_type_from_sexpr e))) "" sel in
	let sfname = env.env_name ^ "." ^ fname ^ params in
	let (fname, ftype, func_type) = 
		try (sfname, (StringMap.find sfname cmap.func_map).returnType, User)
		with | Not_found -> 
		try (fname, (StringMap.find fname cmap.reserved_map).sreturnType, Reserved)
		with | Not_found -> raise (Exceptions.FunctionNotFound fname)
	in
	(* Add a reference to the class in front of the function call *)
	(* Must properly handle the case where this is a reserved function *)
	let sel = if func_type = Sast.User then sel else sel in
	SCall(fname, sel, ftype)

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
	if type1 = type2 
		then SAssign(se1, se2, type1)
		else raise (Exceptions.AssignmentTypeMismatch)

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
    let ts = List.fold_left (fun map (key, value) -> TM.add key value map) TM.empty [(Equal, "Equal"); (Add, "Add"); (Sub, "Sub"); (Mult, "Mult"); (Div, "Div"); (And, "And"); (Or, "Or")] in   
	let se1, env = expr_to_sexpr env e1 in
	let se2, env = expr_to_sexpr env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
    match op with
    Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
    | And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
    | Less | Leq | Greater | Geq -> get_comparison_binop_type type1 type2 se1 se2 op
    | Add | Mult | Sub | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2) 
    | _ -> raise (Exceptions.InvalidBinopExpression ((TM.find op ts) ^ " is not a supported binary op"))

and expr_to_sexpr env = function
		Int_Lit i           -> SInt_Lit(i, Datatype(Int_t)), env
	|   Boolean_Lit b       -> SBoolean_Lit(b, Datatype(Bool_t)), env
	|   Float_Lit f         -> SFloat_Lit(f, Datatype(Float_t)), env
	|   String_Lit s        -> SString_Lit(s, Arraytype(Char_t, 1)), env
	|   Char_Lit c          -> SChar_Lit(c, Datatype(Char_t)), env
	|   This                -> SId("this", Datatype(Objecttype(env.env_name))), env
	|   Id s                -> SId(s, get_ID_type env s), env
	|   Null                -> SNull(Datatype(Null_t)), env
	|   Noexpr              -> SNoexpr(Datatype(Void_t)), env

	|   ObjAccess(e1, e2)   -> check_obj_access env e1 e2, env
	|   ObjectCreate(s, el) -> check_object_constructor env s el, env
	|   Call(s, el)         -> check_call_type env s el, env

	|   ArrayCreate(d, el)  -> check_array_init env d el, env
	|   ArrayAccess(e, el)  -> check_array_access e el, env
	|   ArrayPrimitive el   -> check_array_primitive env el, env

	|   Assign(e1, e2)      -> check_assign env e1 e2, env
	|   Unop(op, e)         -> check_unop env op e, env
	|   Binop(e1, op, e2)   -> check_binop env e1 op e2, env


and get_type_from_sexpr = function
		SInt_Lit(_, d)			-> d
	| 	SBoolean_Lit(_, d)		-> d
	| 	SFloat_Lit(_, d)		-> d
	| 	SString_Lit(_, d) 		-> d
	| 	SChar_Lit(_, d) 		-> d
	| 	SId(_, d) 				-> d
	| 	SBinop(_, _, _, d) 		-> d
	| 	SAssign(_, _, d) 		-> d
	| 	SNoexpr d 				-> d
	| 	SArrayCreate(_, _, d)	-> d
	| 	SArrayAccess(_, _, d) 	-> d
	| 	SObjAccess(_, _, d)		-> d
	| 	SCall(_, _, d) 			-> d
	|   SObjectCreate(_, _, d) 	-> d
	| 	SArrayPrimitive(_, d)	-> d
	|  	SUnop(_, _, d) 			-> d
	| 	SNull d 				-> d

and exprl_to_sexprl env el =
  let env_ref = ref(env) in
  let rec helper = function
      head::tail ->
        let a_head, env = expr_to_sexpr !env_ref head in
        env_ref := env;
        a_head::(helper tail)
    | [] -> []
  in (helper el), !env_ref

let rec local_handler d s e env = 
	if StringMap.mem s env.env_locals 
		then raise (Exceptions.DuplicateLocal s)
		else
			let se, env = expr_to_sexpr env e in
			let t = get_type_from_sexpr se in
(* TODO allow class Foo someObj = new Goo() if class Goo extends Foo *)
			if t = Datatype(Void_t) || t = d 
				then
				let new_env = {
					env_class_maps = env.env_class_maps;
					env_name = env.env_name;
					env_cmap = env.env_cmap;
					env_locals = StringMap.add s d env.env_locals;
					env_parameters = env.env_parameters;
					env_returnType = env.env_returnType;
					env_callStack = env.env_callStack;
					env_reserved = env.env_reserved;
				} in 
(* if the user-defined type being declared is not in global classes map, it is an undefined class *)
				(match d with
					Datatype(Objecttype(x)) -> 
						(if not (StringMap.mem (Utils.string_of_object d) env.env_class_maps) 
							then raise (Exceptions.UndefinedClass (Utils.string_of_object d)) 
							else SLocal(d, s, se), new_env)
				| 	_ -> SLocal(d, s, se), new_env) 
			else 
				let type1 = (Utils.string_of_datatype t) in
				let type2 = (Utils.string_of_datatype d) in
				print_string (type1 ^ type2);
				raise (Exceptions.LocalTypeMismatch)

(* Update this function to return an env object *)
let rec convert_stmt_list_to_sstmt_list env stmt_list = 
	let rec helper env = function 
			Block sl 				-> 	let sl, _ = convert_stmt_list_to_sstmt_list env sl in
										SBlock(sl), env

		| 	Expr e 					-> 	let se, env = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in 
									   	SExpr(se, t), env

		| 	Return e 				-> 	let se, _ = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in
										if t = env.env_returnType 
											then SReturn(se, t), env
											else raise Exceptions.ReturnTypeMismatch

		| 	If(e, s1, s2) 			-> 	let se, _ = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in
										let ifbody, _ = helper env s1 in
										let elsebody, _ = helper env s2 in
										if t = Datatype(Bool_t) 
											then SIf(se, ifbody, elsebody), env
											else raise Exceptions.InvalidIfStatementType

		| 	For(e1, e2, e3, s)		-> 	let se1, _ = expr_to_sexpr env e1 in
										let se2, _ = expr_to_sexpr env e2 in
										let se3, _ = expr_to_sexpr env e3 in
										let forbody, _ = helper env s in
										let conditional = get_type_from_sexpr se2 in
										if (conditional = Datatype(Bool_t) || conditional = Datatype(Void_t))
											then SFor(se1, se2, se3, forbody), env
											else raise Exceptions.InvalidForStatementType

		| 	While(e, s)				->	let se, _ = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in
										let sstmt, _ = helper env s in 
										if (t = Datatype(Bool_t) || t = Datatype(Void_t)) 
											then SWhile(se, sstmt), env
											else raise Exceptions.InvalidWhileStatementType

		|  	Break 					-> SBreak, env (* Need to check if in right context *)
		|   Continue 				-> SContinue, env (* Need to check if in right context *)
		|   Local(d, s, e) 			-> local_handler d s e env
	in
	let env_ref = ref(env) in
	let rec iter = function
	  head::tail ->
	    let a_head, env = helper !env_ref head in
	    env_ref := env;
	    a_head::(iter tail)
	| [] -> []
	in (iter stmt_list), !env_ref

let append_code_to_constructor fbody cname ret_type = 
	let init_this = [SLocal(
		ret_type,
		"this",
		SCall(	"cast", 
				[SCall("malloc", 
					[	
						SCall("sizeof", [SNoexpr(ret_type)], Datatype(Int_t))
					], 
					Arraytype(Char_t, 1))
				],
				ret_type
			)
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

let append_code_to_main fbody cname ret_type = 
	let init_this = [SLocal(
		ret_type,
		"this",
		SCall(	"cast", 
				[SCall("malloc", 
					[	
						SCall("sizeof", [SNoexpr(ret_type)], Datatype(Int_t))
					], 
					Arraytype(Char_t, 1))
				],
				ret_type
			)
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
		env_callStack 	= [];
		env_reserved 	= reserved;
	} in 
	let fbody = fst (convert_stmt_list_to_sstmt_list env constructor.body) in
	{
		sfname 			= Ast.FName (get_name cname constructor);
		sreturnType 	= Datatype(Objecttype(cname));
		sformals 		= constructor.formals;
		sbody 			= append_code_to_constructor fbody cname (Datatype(Objecttype(cname)));
		func_type		= Sast.User;
	}

let convert_fdecl_to_sfdecl class_maps reserved class_map cname fdecl = 
	let class_formal = Ast.Formal(Datatype(Objecttype(cname)), "this") in
	let env = {
		env_class_maps 	= class_maps;
		env_name     	= cname;
		env_cmap 		= class_map;
		env_locals    	= StringMap.empty;
		env_parameters	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m) | _ -> m) StringMap.empty (class_formal :: fdecl.formals);
		env_returnType	= fdecl.returnType;
		env_callStack 	= [];
		env_reserved 	= reserved;
	} in
	let fbody = fst (convert_stmt_list_to_sstmt_list env fdecl.body) in
	let fbody = if (get_name cname fdecl) = "main" then (append_code_to_main fbody cname (Datatype(Objecttype(cname)))) else fbody in
	(* We add the class as the first parameter to the function for codegen *)
	{
		sfname 			= Ast.FName (get_name cname fdecl);
		sreturnType 	= fdecl.returnType;
		sformals 		= class_formal :: fdecl.formals;
		sbody 			= fbody;
		func_type		= Sast.User;
	}

let convert_cdecl_to_sast (cdecl:Ast.class_decl) = 
	{
		scname = cdecl.cname;
		sfields = cdecl.cbody.fields;
	}

let print_fields cdecl = List.iter (fun x -> print_string (Utils.string_of_field x)) cdecl.cbody.fields; print_string "\n\n\n"

let inherit_fields_cdecls cdecls inheritance_forest = 
(* iterate through cdecls to make a map for lookup *)
let cdecl_lookup = List.fold_left (fun a litem -> StringMap.add litem.cname litem a) StringMap.empty cdecls
in
(* print the cdecl lookup map for debugging *)
let _ = StringMap.iter (fun k v -> print_string(k ^ "\n"); print_fields v) cdecl_lookup in
(*
(* returns a list of cdecls that contains inherited fields *)
let cmaps_inherit = StringMap.fold (fun k v a -> StringMap.add k v a) class_maps StringMap.empty
(*in let _ = print_keys cmaps_inherit*)
in let res = StringMap.fold (fun k v a -> (StringSet.add k (fst a), 
(List.fold_left (fun acc child -> StringSet.add child acc) (snd a) v)
)) predecessors (StringSet.empty, StringSet.empty)
in
let roots = StringSet.diff (fst res) (snd res)
(*in let _ = print_set_members roots*)
in let rec add_inherited_fields predec desc cmap_to_update = 
    List.fold_left (fun a x -> let merged = merge_cdecls (StringMap.find predec a).field_map (StringMap.find x a).field_map in let updated = (update_class_maps "field_map" merged x a) in if (StringMap.mem x predecessors) then (add_inherited_fields x (StringMap.find x predecessors) updated) else updated) cmap_to_update desc
    (* end of add_inherited_fields *)
in let result = StringSet.fold (fun x a -> add_inherited_fields x (StringMap.find x predecessors) a) roots cmaps_inherit
(*in let _ = print_map result*)
in result
*)
cdecls


let default_value t = match t with 
		Datatype(Int_t) 		-> SInt_Lit(0, Datatype(Int_t))
	| 	Datatype(Float_t) 		-> SFloat_Lit(0.0, Datatype(Float_t))
	| 	Datatype(Bool_t) 		-> SBoolean_Lit(false, Datatype(Bool_t))
	| 	Datatype(Char_t) 		-> SChar_Lit(Char.chr 0, Datatype(Char_t))
	|  	Arraytype(Char_t, 1) 	-> SString_Lit("", Arraytype(Char_t, 1))
	| 	_ 						-> SNull(Datatype(Null_t))

let convert_cdecls_to_sast class_maps reserved (cdecls:Ast.class_decl list) inheritance_forest = 
	let handle_cdecl cdecl = 
		let class_map = StringMap.find cdecl.cname class_maps in 
		let scdecl = convert_cdecl_to_sast cdecl in
		let sconstructor_list = List.fold_left (fun l c -> (convert_constructor_to_sfdecl class_maps reserved class_map cdecl.cname c) :: l) [] cdecl.cbody.constructors in
		let func_list = List.fold_left (fun l f -> (convert_fdecl_to_sfdecl class_maps reserved class_map cdecl.cname f) :: l) [] cdecl.cbody.methods in
		(scdecl, func_list @ sconstructor_list)
	in 
        let cdecls_inherited = inherit_fields_cdecls cdecls inheritance_forest in
		let overall_list = List.fold_left (fun t c -> let scdecl = handle_cdecl c in (fst scdecl :: fst t, snd scdecl @ snd t)) ([], []) cdecls_inherited in
        let find_main = (fun f -> match f.sfname with FName n -> n = "main" | _ -> false) in
		let mains = (List.find_all find_main (snd overall_list)) in
		let main = if List.length mains < 1 then raise Exceptions.MainNotDefined else if List.length mains > 1 then raise Exceptions.MultipleMainsDefined else List.hd mains in
		let funcs = (List.filter (fun f -> not (find_main f)) (snd overall_list)) in
		(* let funcs = (add_default_constructors cdecls class_maps) @ funcs in *)
		{
			classes 		= fst overall_list;
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
		}
	in
	let reserved = [] in
	let reserved = (reserved_stub "print" (Datatype(Void_t)) ([ Many(Any) ])) :: reserved in
	let reserved = (reserved_stub "malloc" (Arraytype(Char_t, 1)) ([ Formal(Datatype(Int_t), "size")])) :: reserved in
	let reserved = (reserved_stub "cast" (Any) ([ Formal(Any, "in")])) :: reserved in
	let reserved = (reserved_stub "sizeof" (Datatype(Int_t)) ([ Formal(Any, "in")])) :: reserved in	
	reserved

let print_inheritance_tree m = 
StringMap.iter (fun key value -> print_string ("\n" ^ key ^ "\n");
List.iter (fun x -> print_string x) value) m

let print_set_members s = 
StringSet.iter (fun el -> print_string el) s

let build_inheritance_forest cdecls cmap = 
    let forest = List.fold_left (fun a cdecl -> match cdecl.extends with Parent(s) -> if (StringMap.mem s a) then (StringMap.add s (cdecl.cname::(StringMap.find s a)) a) else StringMap.add s [cdecl.cname] a | NoParent -> a) StringMap.empty cdecls
    in let _ = StringMap.iter (fun key value -> if not (StringMap.mem key cmap) then raise (Exceptions.UndefinedClass key)) forest
    in forest

let merge_maps m1 m2 = 
let merged = StringMap.fold (fun k v a -> StringMap.add k v a) m1 m2
in merged

let update_class_maps cmap_attr cmap_val cname cmap_to_update = 
    let update m = function
    "field_map" -> {field_map = cmap_val;
                    func_map = m.func_map;
                    constructor_map = m.constructor_map;
                    reserved_map = m.reserved_map}
     | _ -> m
in let updated = StringMap.add cname (update (StringMap.find cname cmap_to_update) cmap_attr) cmap_to_update
in updated


let inherit_fields class_maps predecessors =
let cmaps_inherit = StringMap.fold (fun k v a -> StringMap.add k v a) class_maps StringMap.empty
(*in let _ = print_keys cmaps_inherit*)
in let res = StringMap.fold (fun k v a -> (StringSet.add k (fst a), 
(List.fold_left (fun acc child -> StringSet.add child acc) (snd a) v)
)) predecessors (StringSet.empty, StringSet.empty)
in
let roots = StringSet.diff (fst res) (snd res)
(*in let _ = print_set_members roots*)
in let rec add_inherited_fields predec desc cmap_to_update = 
    List.fold_left (fun a x -> let merged = merge_maps (StringMap.find predec a).field_map (StringMap.find x a).field_map in let updated = (update_class_maps "field_map" merged x a) in if (StringMap.mem x predecessors) then (add_inherited_fields x (StringMap.find x predecessors) updated) else updated) cmap_to_update desc
    (* end of add_inherited_fields *)
in let result = StringSet.fold (fun x a -> add_inherited_fields x (StringMap.find x predecessors) a) roots cmaps_inherit
(*in let _ = print_map result*)
in result


let check_cyclical_inheritance predecessors = print_string "checking for cycles"

(* Main method for analyzer *)
let analyze filename program = match program with
	Program(includes, classes) ->
	let cdecls = process_includes filename includes classes in
    let reserved = add_reserved_functions in
	let class_maps = build_class_maps reserved cdecls in
    let predecessors = build_inheritance_forest cdecls class_maps in
	let _ = check_cyclical_inheritance predecessors in
    let cmaps_with_inherited_fields = inherit_fields class_maps predecessors in
	let sast = convert_cdecls_to_sast cmaps_with_inherited_fields reserved cdecls predecessors in
	sast
