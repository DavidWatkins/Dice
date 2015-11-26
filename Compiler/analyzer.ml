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
		env_name      : string;
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
				iterate_includes (classes) (m) (t)
			else 
				(function Program(i, c) -> iterate_includes (classes @ c) (StringMap.add realpath 1 m) (i @ t) ) result
	in
	iterate_includes classes (StringMap.add (Filepath.realpath filename) 1 StringMap.empty) includes

let get_name fdecl = 
	let params = List.fold_left (fun s -> (function Formal(t, s) -> s ^ "," ^ Utils.string_of_datatype t)) "" fdecl.formals in
	let name = Utils.string_of_fname fdecl.fname in
	name ^ "," ^ params

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

let rec get_ID_type env s = Datatype(Int_t)

and check_array_primitive env el = SInt_Lit(0, Datatype(Int_t))

and check_array_init env d el = SInt_Lit(0, Datatype(Int_t))

and check_array_access e el = SInt_Lit(0, Datatype(Int_t))

and check_obj_access env e1 e2 = SInt_Lit(0, Datatype(Int_t))

and check_call_type env s el = 
	let sel = List.map (expr_to_sexpr env) el in
	SCall(s, sel, Datatype(Void_t))
	
and check_object_constructor env s el = SInt_Lit(0, Datatype(Int_t))

and check_assign env e1 e2 = 
	SInt_Lit(0, Datatype(Int_t))
	
and check_bool_unop = function
		Not 	-> Datatype(Bool_t)
	| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)

and check_num_unop t = function
		Sub 	-> t
	| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)

and check_unop env (op:Ast.op) e = 
	let se = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	match t with 
		Datatype(Int_t) 	
	|	Datatype(Float_t) 	-> SUnop(op, se, check_num_unop t op)
	|  	Datatype(Bool_t) 	-> SUnop(op, se, check_bool_unop op)
	| 	_ -> raise(Exceptions.InvalidUnaryOperation)

and check_binop env e1 op e2 =
	let se1 = expr_to_sexpr env e1 in
	let se2 = expr_to_sexpr env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	match (type1, type2) with
		(Datatype(Int_t), Datatype(Int_t)) -> SBinop(se1, op, se2, Datatype(Int_t))
	| 	_ -> raise Exceptions.InvalidBinopExpression

and expr_to_sexpr (env:env) = function
		Int_Lit i           -> SInt_Lit(i, Datatype(Int_t))
	|   Boolean_Lit b       -> SBoolean_Lit(b, Datatype(Bool_t))
	|   Float_Lit f         -> SFloat_Lit(f, Datatype(Float_t))
	|   String_Lit s        -> SString_Lit(s, Arraytype(Char_t, 1))
	|   Char_Lit c          -> SChar_Lit(c, Datatype(Char_t))
	|   This                -> SId("this", Datatype(Objecttype(env.env_name)))
	|   Id s                -> SId(s, get_ID_type env s)
	|   Null                -> SNull(Datatype(Null_t))
	|   Noexpr              -> SNoexpr(Datatype(Void_t))

	|   ObjAccess(e1, e2)   -> check_obj_access env e1 e2
	|   ObjectCreate(s, el) -> check_object_constructor env s el
	|   Call(s, el)         -> check_call_type env s el

	|   ArrayCreate(d, el)  -> check_array_init env d el
	|   ArrayAccess(e, el)  -> check_array_access e el
	|   ArrayPrimitive el   -> check_array_primitive env el

	|   Assign(e1, e2)      -> check_assign env e1 e2
	|   Unop(op, e)         -> check_unop env op e
	|   Binop(e1, op, e2)   -> check_binop env e1 op e2


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

(* Update this function to return an env object *)
let convert_stmt_list_to_sstmt_list (env:env) stmt_list = 
	let rec helper env = function 
			Block sl 				-> SBlock(List.rev (List.fold_left (fun l s -> (helper env s) :: l) [] sl))
		| 	Expr e 					-> 	let se = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in 
									   	SExpr(se, t)
		| 	Return e 				-> 	let se = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in
										if t = env.env_returnType 
											then SReturn(se, t) 
											else raise Exceptions.ReturnTypeMismatch
		| 	If(e, s1, s2) 			-> 	let se = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in
										if t = Datatype(Bool_t) 
											then SIf(se, helper env s1, helper env s2) 
											else raise Exceptions.InvalidIfStatementType
		| 	For(e1, e2, e3, s)		-> 	let se1 = expr_to_sexpr env e1 in
										let se2 = expr_to_sexpr env e2 in
										let se3 = expr_to_sexpr env e3 in
										let conditional = get_type_from_sexpr se2 in
										if (conditional = Datatype(Bool_t) || conditional = Datatype(Void_t))
											then SFor(se1, se2, se3, helper env s)
											else raise Exceptions.InvalidForStatementType
		| 	While(e, s)				->	let se = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in
										if (t = Datatype(Bool_t) || t = Datatype(Void_t)) 
											then SWhile(se, helper env s) 
											else raise Exceptions.InvalidWhileStatementType
		|  	Break 					-> SBreak (* Need to check if in right context *)
		|   Continue 				-> SContinue (* Need to check if in right context *)
		|   Local(d, s, e) 			-> 	let se = expr_to_sexpr env e in
										let t = get_type_from_sexpr se in
										if t = d (* AND s not in env.locals *) 
											then SLocal(d, s, se) 
											else raise Exceptions.LocalTypeMismatch
	in
	List.map (helper env) stmt_list

let convert_constructor_to_sfdecl class_map cname constructor = 
	let env = {
		env_class_map 	= class_map;
		env_name     	= cname;
		env_locals    	= StringMap.empty;
		env_parameters	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m)) StringMap.empty constructor.formals;
		env_returnType	= Datatype(Objecttype(cname));
		env_callStack 	= [];
	} in 
	{
		sfname 			= Constructor;
		sreturnType 	= Datatype(Objecttype(cname));
		sformals 		= constructor.formals;
		sbody 			= convert_stmt_list_to_sstmt_list env constructor.body;
	}

let convert_fdecl_to_sfdecl class_map cname fdecl = 
	let env = {
		env_class_map 	= class_map;
		env_name     	= cname;
		env_locals    	= StringMap.empty;
		env_parameters	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m)) StringMap.empty fdecl.formals;
		env_returnType	= fdecl.returnType;
		env_callStack 	= [];
	} in 
	(* We add the class as the first parameter to the function for codegen *)
	{
		sfname 			= fdecl.fname;
		sreturnType 	= fdecl.returnType;
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
