open Sast
open Ast
open Processor
open Utils
open Filepath

module Includes = Map.Make(String)
module Env = Map.Make(String)
module StringMap = Map.Make (String)

type class_map = {
		field_map       : Ast.field StringMap.t;
		func_map        : Ast.func_decl StringMap.t;
		constructor_map : Ast.func_decl StringMap.t;
}

type env = {
		env_class_map : class_map;
		env_name     : string;
		env_locals    : string StringMap.t;
		env_parameters: Ast.formal StringMap.t;
		env_returnType: datatype;
		env_callStack : stmt list;
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
				iterate_includes (classes) (StringMap.add realpath 1 m) (t)
			else 
				(function Program(i, c) -> iterate_includes (classes @ c) (StringMap.add realpath 1 m) (i @ t) ) result
	in
	iterate_includes classes (StringMap.add (Filepath.realpath filename) 1 StringMap.empty) includes

let get_name fdecl = 
	let params = List.fold_left (fun s -> (function Formal(t, s) -> s ^ ", " ^ Utils.string_of_datatype t)) "" fdecl.formals in
	let name = Utils.string_of_fname fdecl.fname in
	name ^ " " ^ params

(* Generate list of all classes to be used for semantic checking *)
let build_class_maps cdecls =
		(* helper global_obj cdecls *)
		let helper m (cdecl:Ast.class_decl) =  
			let fieldfun = (fun m -> (function Field(s, d, n) -> if (StringMap.mem (n) m) then raise(Exceptions.DuplicateField) else (StringMap.add n (Field(s, d, n)) m))) in
			let funcfun = (fun m fdecl -> if (StringMap.mem (get_name fdecl) m) then raise(Exceptions.DuplicateFunction) else (StringMap.add (get_name fdecl) fdecl m)) in
			let constructorfun = (fun m fdecl -> if (StringMap.mem (get_name fdecl) m) then raise(Exceptions.DuplicateConstructor) else  (StringMap.add (get_name fdecl) fdecl m)) in
			(if (StringMap.mem cdecl.cname m) then raise (Exceptions.DuplicateClassName) else
				StringMap.add cdecl.cname 
						{ field_map = List.fold_left fieldfun StringMap.empty cdecl.cbody.fields; 
							func_map = List.fold_left funcfun StringMap.empty cdecl.cbody.methods;
							constructor_map = List.fold_left constructorfun StringMap.empty cdecl.cbody.constructors; } 
											 m) in
		List.fold_left helper StringMap.empty cdecls



let get_ID_type env s = Datatype(Objecttype("String"))

let rec check_array_primitive env el = Arraytype(Char_t, 1)

let check_object_constructor env s el = Datatype(Objecttype("String"))

let resolve_this_type env = Datatype(Objecttype("String"))

let check_array_init env d el = Arraytype(Char_t, 1)

let check_array_access e el = Datatype(Char_t)

let check_call_type env s el = Datatype(Objecttype("String"))

let check_call_type env s el = Datatype(Objecttype("String"))

(* Need to handle unops *)
let rec check_obj_access env e1 e2 = 
		Datatype(Objecttype("String"))

and check_call_type env s el = 
		Datatype(Objecttype("String"))

and check_object_constructor env s el = 
		Datatype(Objecttype("String"))

and check_array_primitive env el = 
		Datatype(Objecttype("String"))

and check_binop env e1 op e2 =
		let type1 = get_expr_type env e1 in
		let type2 = get_expr_type env e2 in
		match (type1, type2) with
				(Datatype(Int_t), Datatype(Int_t)) -> Datatype(Int_t)
			| _ -> raise Exceptions.InvalidBinopExpression

and get_expr_type (env:env) = function
			Int_Lit i           -> Datatype(Int_t)
	|   Boolean_Lit b       -> Datatype(Bool_t)
	|   Float_Lit f         -> Datatype(Float_t)
	|   String_Lit s        -> Arraytype(Char_t, 1)
	|   Char_Lit c          -> Datatype(Char_t)
	|   This                -> resolve_this_type env
	|   Id s                -> get_ID_type env s
	|   Binop(e1, op, e2)   -> check_binop env e1 op e2
	|   Assign(e1, e2)      -> get_expr_type env e1
	|   Noexpr              -> Datatype(Void_t)
	|   ArrayCreate(d, el)  -> check_array_init env d el
	|   ArrayAccess(e, el)  -> check_array_access e el
	|   ObjAccess(e1, e2)   -> check_obj_access env e1 e2
	|   Call(s, el)         -> check_call_type env s el
	|   ObjectCreate(s, el) -> check_object_constructor env s el
	|   ArrayPrimitive el   -> check_array_primitive env el
	|   Unop(op, e)         -> get_expr_type env e
	|   Null                -> Datatype(Null_t)

(* Update this function to return an env object *)
let convert_stmt_list_to_sstmt_list (env:env) stmt_list = 
	let rec helper env = function 
				Block sl 						-> SBlock(List.rev (List.fold_left (fun l s -> (helper env s) :: l) [] sl))
		| 	Expr e 							-> SExpr(e, get_expr_type env e)
		| 	Return e 						-> if get_expr_type env e = env.env_returnType then SReturn(e) else raise Exceptions.ReturnTypeMismatch
		| 	If(e, s1, s2) 			-> if get_expr_type env e = Datatype(Bool_t) then SIf(e, helper env s1, helper env s2) else raise Exceptions.InvalidIfStatementType
		| 	For(e1, e2, e3, s)	-> 
					let _ = get_expr_type env e1 in
					let expr_type2 = get_expr_type env e2 in
					let _ = get_expr_type env e3 in
					if (expr_type2 = Datatype(Bool_t) || expr_type2 = Datatype(Void_t)) then SFor(e1, e2, e3, helper env s) else raise Exceptions.InvalidForStatementType
		| 	While(e, s)					->
					let expr_type = get_expr_type env e in
					if (expr_type = Datatype(Bool_t) || expr_type = Datatype(Void_t)) then SWhile(e, helper env s) else raise Exceptions.InvalidWhileStatementType
		|  	Break 							-> SBreak (* Need to check if in right context *)
		|   Continue 						-> SContinue (* Need to check if in right context *)
		|   Local(d, s, e) 			-> if get_expr_type env e = d (* AND s not in env.locals *) then SLocal(d, s, e) else raise Exceptions.LocalTypeMismatch
	in
	List.map (helper env) stmt_list

let convert_constructor_to_sfdecl class_map cname constructor = 
	let env = {
		env_class_map = class_map;
		env_name     = cname;
		env_locals    = StringMap.empty;
		env_parameters= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m)) StringMap.empty constructor.formals;
		env_returnType= Datatype(Objecttype(cname));
		env_callStack = [];
	} in 
	{
		sfname 			= Constructor;
		sreturnType = Datatype(Objecttype(cname));
		sformals 		= constructor.formals;
		sbody 			= convert_stmt_list_to_sstmt_list env constructor.body;
	}

let convert_fdecl_to_sfdecl class_map cname fdecl = 
	let env = {
		env_class_map = class_map;
		env_name     = cname;
		env_locals    = StringMap.empty;
		env_parameters= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m)) StringMap.empty fdecl.formals;
		env_returnType= fdecl.returnType;
		env_callStack = [];
	} in 
	(* We add the class as the first parameter to the function for codegen *)
	{
		sfname 			= fdecl.fname;
		sreturnType = fdecl.returnType;
		sformals 		= fdecl.formals;
		sbody 			= convert_stmt_list_to_sstmt_list env fdecl.body;
	}

let convert_cdecl_to_sast (cdecl:Ast.class_decl) = 
	{
		scname = cdecl.cname;
		sfields = cdecl.cbody.fields;
	}

let convert_cdecls_to_sast class_maps (cdecls:Ast.class_decl list) = 
	let handle_cdecl cdecl = 
			let class_map = StringMap.find cdecl.cname class_maps in 
			let scdecl = convert_cdecl_to_sast cdecl in
			let sconstructor_list = List.fold_left (fun l c -> (convert_constructor_to_sfdecl class_map cdecl.cname c) :: l) [] cdecl.cbody.constructors in
			let func_list = List.fold_left (fun l f -> (convert_fdecl_to_sfdecl class_map cdecl.cname f) :: l) [] cdecl.cbody.methods in
			(scdecl, func_list @ sconstructor_list)
	in 
		let overall_list = List.fold_left (fun t c -> let scdecl = handle_cdecl c in (fst scdecl :: fst t, snd scdecl @ snd t)) ([], []) cdecls in
(* 		let _ = List.iter (fun f -> match f.sfname with FName n -> print_string (n ^ "\n") | _ -> ()) (snd overall_list) in
 *)		let mains = (List.find_all (fun f -> match f.sfname with FName n -> n = "main" | _ -> false) (snd overall_list)) in
		let main = if List.length mains < 1 then raise Exceptions.MainNotDefined else if List.length mains > 1 then raise Exceptions.MultipleMainsDefined else List.hd mains in
		SProgram(fst overall_list, snd overall_list, main)

(* Main method for analyzer *)
let analyze filename program = match program with
	Program(includes, classes) ->
	let cdecls = process_includes filename includes classes in
	let class_maps = build_class_maps cdecls in
	let sast = convert_cdecls_to_sast class_maps cdecls in
	sast
