open Sast
open Ast
open Processor
open Utils
open Filepath

module StringMap = Map.Make (String)

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
		env_class_map : class_map;
		env_name      : string;
		env_locals    : datatype StringMap.t;
		env_parameters: Ast.formal StringMap.t;
		env_returnType: datatype;
		env_callStack : stmt list;
		env_reserved  : sfunc_decl list;
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
	let params = List.fold_left (fun s -> (function Formal(t, s) -> s ^ "," ^ Utils.string_of_datatype t | _ -> "" )) "" fdecl.formals in
	let name = Utils.string_of_fname fdecl.fname in
	name ^ " " ^ params

(* Generate list of all classes to be used for semantic checking *)
let build_class_maps reserved cdecls =
		let reserved_map = List.fold_left (fun m f -> StringMap.add (Utils.string_of_fname f.sfname) f m) StringMap.empty reserved in
		(* helper global_obj cdecls *)
		let helper m (cdecl:Ast.class_decl) =  
			let fieldfun = (fun m -> (function Field(s, d, n) -> if (StringMap.mem (n) m) then raise(Exceptions.DuplicateField) else (StringMap.add n (Field(s, d, n)) m))) in
			let funcfun = 
				(fun m fdecl -> 
					if (StringMap.mem (get_name fdecl) m) || (StringMap.mem (Utils.string_of_fname fdecl.fname) reserved_map)
						then raise(Exceptions.DuplicateFunction) 
						else (StringMap.add (get_name fdecl) fdecl m)) 
			in
			let constructorfun = (fun m fdecl -> if (StringMap.mem (get_name fdecl) m) then raise(Exceptions.DuplicateConstructor) else  (StringMap.add (get_name fdecl) fdecl m)) in
			(if (StringMap.mem cdecl.cname m) then raise (Exceptions.DuplicateClassName) else
				StringMap.add cdecl.cname 
						{ field_map = List.fold_left fieldfun StringMap.empty cdecl.cbody.fields; 
							func_map = List.fold_left funcfun StringMap.empty cdecl.cbody.methods;
							constructor_map = List.fold_left constructorfun StringMap.empty cdecl.cbody.constructors; 
							reserved_map = reserved_map; } 
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


let get_type_string = function
    Datatype(Int_t) -> "INT"
    | Datatype(Float_t) -> "FLOAT" 
    | Datatype(Char_t) -> "CHAR"
    | Datatype(Bool_t) -> "BOOL"
    | Datatype(Void_t) -> "VOID"
    | Datatype(Null_t) -> "NULL"
    | Datatype(Objecttype(s)) -> s
    | _ -> "some other type"

let get_formal_typestr = function 
    Formal(d, s) -> get_type_string d
    | Many(d) -> get_type_string d

(* given a list of formals, returns a list of types only *)
let get_formal_types fmls = 
List.fold_left (fun tstrings fml -> (get_formal_typestr fml)::tstrings) [] fmls
(*["CHAR";"BOOL"]*)

let rec get_ID_type env s = StringMap.find s env.env_locals

and check_array_primitive env el = SInt_Lit(0, Datatype(Int_t))

and check_array_init env d el = SInt_Lit(0, Datatype(Int_t))

and check_array_access e el = SInt_Lit(0, Datatype(Int_t))

and check_obj_access env e1 e2 = SInt_Lit(0, Datatype(Int_t))

and check_call_type global_cmap env s el = 
	let sel, env = exprl_to_sexprl global_cmap env el in
	SCall(s, sel, Datatype(Void_t))

and check_object_constructor global_cmap env s el = 
(* check that `s` is in the list of defined classes *)
if not (StringMap.mem s global_cmap) then raise (Exceptions.UndefinedClass s)
else
    let sel, env = exprl_to_sexprl global_cmap env el in
    let cmap = StringMap.find s global_cmap in 
    (* get a list of the types of the actuals to match against defined constructor formals *)
    let types_of_actuals = List.fold_left (fun acc x -> acc @ [get_type_string (get_type_from_sexpr x)]) [] sel in 
    (* get constructors for the class being instantiated *)
    let constructor_decls = List.fold_left (fun acc (k,v) -> v::acc) [] (StringMap.bindings cmap.constructor_map) in 
    (* match list of the types of actuals against the types in all known formals *) 
    let constructor_formal_lists = List.fold_left (fun acc x -> (get_formal_types x.formals)::acc) [] constructor_decls in  
    let matched = List.exists (fun l -> l = types_of_actuals) constructor_formal_lists in 
    if matched then SObjectCreate(s, sel,Datatype(Objecttype(s))) 
    else raise Exceptions.ConstructorNotFound

and check_assign global_cmap env e1 e2 = 
	let se1, env = expr_to_sexpr global_cmap env e1 in
	let se2, env = expr_to_sexpr global_cmap env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in 
	if type1 = type2 
		then SAssign(se1, se2, type1)
		else raise (Exceptions.AssignmentTypeMismatch)

and check_unop global_cmap env op e = 
	let check_num_unop t = function
			Sub 	-> t
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in 
	let check_bool_unop = function
			Not 	-> Datatype(Bool_t)
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let se, env = expr_to_sexpr global_cmap env e in
	let t = get_type_from_sexpr se in
	match t with 
		Datatype(Int_t) 	
	|	Datatype(Float_t) 	-> SUnop(op, se, check_num_unop t op)
	|  	Datatype(Bool_t) 	-> SUnop(op, se, check_bool_unop op)
	| 	_ -> raise(Exceptions.InvalidUnaryOperation)

and check_binop global_cmap env e1 op e2 =
    let ts = List.fold_left (fun map (key, value) -> TM.add key value map) TM.empty [(Equal, "Equal"); (Add, "Add"); (Sub, "Sub"); (Mult, "Mult"); (Div, "Div"); (And, "And"); (Or, "Or")] in   
	let se1, env = expr_to_sexpr global_cmap env e1 in
	let se2, env = expr_to_sexpr global_cmap env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
    match op with
    Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
    | And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
    | Less | Leq | Greater | Geq -> get_comparison_binop_type type1 type2 se1 se2 op
    | Add | Mult | Sub | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2) 
    | _ -> raise (Exceptions.InvalidBinopExpression ((TM.find op ts) ^ " is not a supported binary op"))

and expr_to_sexpr global_cmap env = function
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
	|   ObjectCreate(s, el) -> check_object_constructor global_cmap env s el, env
	|   Call(s, el)         -> check_call_type global_cmap env s el, env

	|   ArrayCreate(d, el)  -> check_array_init env d el, env
	|   ArrayAccess(e, el)  -> check_array_access e el, env
	|   ArrayPrimitive el   -> check_array_primitive env el, env

	|   Assign(e1, e2)      -> check_assign global_cmap env e1 e2, env
	|   Unop(op, e)         -> check_unop global_cmap env op e, env
	|   Binop(e1, op, e2)   -> check_binop global_cmap env e1 op e2, env


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

and exprl_to_sexprl global_cmap env el =
  let env_ref = ref(env) in
  let rec helper = function
      head::tail ->
        let a_head, env = expr_to_sexpr global_cmap !env_ref head in
        env_ref := env;
        a_head::(helper tail)
    | [] -> []
  in (helper el), !env_ref

(* Update this function to return an env object *)
let rec convert_stmt_list_to_sstmt_list global_cmap env stmt_list = 
	let rec helper env = function 
			Block sl 				-> 	let sl, _ = convert_stmt_list_to_sstmt_list global_cmap env sl in
										SBlock(sl), env

		| 	Expr e 					-> 	let se, env = expr_to_sexpr global_cmap env e in
										let t = get_type_from_sexpr se in 
									   	SExpr(se, t), env

		| 	Return e 				-> 	let se, _ = expr_to_sexpr global_cmap env e in
										let t = get_type_from_sexpr se in
										if t = env.env_returnType 
											then SReturn(se, t), env
											else raise Exceptions.ReturnTypeMismatch

		| 	If(e, s1, s2) 			-> 	let se, _ = expr_to_sexpr global_cmap env e in
										let t = get_type_from_sexpr se in
										let ifbody, _ = helper env s1 in
										let elsebody, _ = helper env s2 in
										if t = Datatype(Bool_t) 
											then SIf(se, ifbody, elsebody), env
											else raise Exceptions.InvalidIfStatementType

		| 	For(e1, e2, e3, s)		-> 	let se1, _ = expr_to_sexpr global_cmap env e1 in
										let se2, _ = expr_to_sexpr global_cmap env e2 in
										let se3, _ = expr_to_sexpr global_cmap env e3 in
										let forbody, _ = helper env s in
										let conditional = get_type_from_sexpr se2 in
										if (conditional = Datatype(Bool_t) || conditional = Datatype(Void_t))
											then SFor(se1, se2, se3, forbody), env
											else raise Exceptions.InvalidForStatementType

		| 	While(e, s)				->	let se, _ = expr_to_sexpr global_cmap env e in
										let t = get_type_from_sexpr se in
										let sstmt, _ = helper env s in 
										if (t = Datatype(Bool_t) || t = Datatype(Void_t)) 
											then SWhile(se, sstmt), env
											else raise Exceptions.InvalidWhileStatementType

		|  	Break 					-> SBreak, env (* Need to check if in right context *)
		|   Continue 				-> SContinue, env (* Need to check if in right context *)

		|   Local(d, s, e) 			-> 	if StringMap.mem s env.env_locals then raise (Exceptions.DuplicateLocal s)
                                        else
                                        let se, env = expr_to_sexpr global_cmap env e in
										let t = get_type_from_sexpr se in
                                        (* TODO allow class Foo someObj = new Goo()
                                        if class Goo extends Foo *)
										if t = Datatype(Void_t) || t = d 
										then
                                            let new_env = {
                                                env_class_map = env.env_class_map;
                                                env_name = env.env_name;
                                                env_locals = StringMap.add s d env.env_locals;
                                                env_parameters = env.env_parameters;
                                                env_returnType = env.env_returnType;
                                                env_callStack = env.env_callStack;
                                                env_reserved = env.env_reserved;
                                            } in 
                                            (* if the user-defined type being declared is not 
                                            in global classes map, it is an undefined class *)
                                            (match d with
                                            Datatype(Objecttype(x)) -> (if not (StringMap.mem (get_type_string d) global_cmap) then raise (Exceptions.UndefinedClass (get_type_string d)) else SLocal(d, s, se), new_env)
                                            | _ -> SLocal(d, s, se), new_env) 
                                        else raise Exceptions.LocalTypeMismatch
	in
	let env_ref = ref(env) in
	let rec iter = function
	  head::tail ->
	    let a_head, env = helper !env_ref head in
	    env_ref := env;
	    a_head::(iter tail)
	| [] -> []
	in (iter stmt_list), !env_ref


let convert_constructor_to_sfdecl global_cmap reserved class_map cname constructor = 
	let env = {
		env_class_map 	= class_map;
		env_name     	= cname;
		env_locals    	= StringMap.empty;
		env_parameters	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m) | _ -> m) StringMap.empty constructor.formals;
		env_returnType	= Datatype(Objecttype(cname));
		env_callStack 	= [];
		env_reserved 	= reserved;
	} in 
	{
		sfname 			= Constructor;
		sreturnType 	= Datatype(Objecttype(cname));
		sformals 		= constructor.formals;
		sbody 			= fst (convert_stmt_list_to_sstmt_list global_cmap env constructor.body);
		func_type		= Sast.User;
	}

let convert_fdecl_to_sfdecl global_cmap reserved class_map cname fdecl = 
	let env = {
		env_class_map 	= class_map;
		env_name     	= cname;
		env_locals    	= StringMap.empty;
		env_parameters	= List.fold_left (fun m f -> match f with Formal(d, s) -> (StringMap.add s f m) | _ -> m) StringMap.empty fdecl.formals;
		env_returnType	= fdecl.returnType;
		env_callStack 	= [];
		env_reserved 	= reserved;
	} in 
	(* We add the class as the first parameter to the function for codegen *)
	{
		sfname 			= fdecl.fname;
		sreturnType 	= fdecl.returnType;
		sformals 		= fdecl.formals;
		sbody 			= fst (convert_stmt_list_to_sstmt_list global_cmap env fdecl.body);
		func_type		= Sast.User;
	}

let convert_cdecl_to_sast (cdecl:Ast.class_decl) = 
	{
		scname = cdecl.cname;
		sfields = cdecl.cbody.fields;
	}

let convert_cdecls_to_sast class_maps reserved (cdecls:Ast.class_decl list) = 
	let handle_cdecl cdecl = 
		let class_map = StringMap.find cdecl.cname class_maps in 
		let scdecl = convert_cdecl_to_sast cdecl in
		let sconstructor_list = List.fold_left (fun l c -> (convert_constructor_to_sfdecl class_maps reserved class_map cdecl.cname c) :: l) [] cdecl.cbody.constructors in
		let func_list = List.fold_left (fun l f -> (convert_fdecl_to_sfdecl class_maps reserved class_map cdecl.cname f) :: l) [] cdecl.cbody.methods in
		(scdecl, func_list @ sconstructor_list)
	in 
		let overall_list = List.fold_left (fun t c -> let scdecl = handle_cdecl c in (fst scdecl :: fst t, snd scdecl @ snd t)) ([], []) cdecls in
(* 		let _ = List.iter (fun f -> match f.sfname with FName n -> print_string (n ^ "\n") | _ -> ()) (snd overall_list) in
 *)	let mains = (List.find_all (fun f -> match f.sfname with FName n -> n = "main" | _ -> false) (snd overall_list)) in
		let main = if List.length mains < 1 then raise Exceptions.MainNotDefined else if List.length mains > 1 then raise Exceptions.MultipleMainsDefined else List.hd mains in
		let funcs = (List.filter (fun f -> match f.sfname with FName n -> n <> "main" | _ -> true) (snd overall_list)) in
		{
			classes 		= fst overall_list;
			functions 	= funcs;
			main 				= main;
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
	reserved

(* Main method for analyzer *)
let analyze filename program = match program with
	Program(includes, classes) ->
	let cdecls = process_includes filename includes classes in
	let reserved = add_reserved_functions in
	let class_maps = build_class_maps reserved cdecls in
	let sast = convert_cdecls_to_sast class_maps reserved cdecls in
	sast
