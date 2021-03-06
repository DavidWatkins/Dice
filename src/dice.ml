open Llvm
open Llvm_analysis
open Analyzer
open Utils
open Ast
open Yojson
open Exceptions
open Filepath

type action = Tokens | TokenEndl | PrettyPrint | Ast | Sast | Compile | CompileToFile | Help

let get_action = function
		"-tendl" 	-> TokenEndl
	| 	"-t" 		-> Tokens
	| 	"-p" 		-> PrettyPrint
	| 	"-ast" 		-> Ast
	| 	"-sast" 	-> Sast
	| 	"-h" 		-> Help
	| 	"-c" 		-> Compile
	| 	"-f" 		-> CompileToFile
	|  	_ as s		-> raise (Exceptions.InvalidCompilerArgument s)

let check_single_argument = function
		"-h" 	-> Help, ""
	| 	"-tendl"
	| 	"-t" 	
	| 	"-p" 	
	| 	"-ast" 	
	| 	"-sast" 
	| 	"-c" 	
	| 	"-f" 	-> raise (Exceptions.NoFileArgument)
	|  	_ as s 	-> CompileToFile, s

let dice_name filename = 
	let basename = Filename.basename filename in
	let filename = Filename.chop_extension basename in
	filename ^ ".ll"

let help_string = (
	  "Usage: dice [optional-option] <source file>\n" ^
		"optional-option:\n" ^
		"\t-h: Print help text\n" ^
		"\t-tendl: Prints tokens with newlines intact\n" ^ 
		"\t-t: Prints token stream\n" ^
		"\t-p: Pretty prints Ast as a program\n" ^
		"\t-ast: Prints abstract syntax tree as json\n" ^
		"\t-sast: Prints semantically checked syntax tree as json\n" ^
		"\t-c: Compiles source\n" ^
		"\t-f: Compiles source to file (<filename>.<ext> -> <filename>.ll)\n" ^
		"Option defaults to \"-f\"\n"
	)

let _ =
	ignore(Printexc.record_backtrace true);
	try
		let action, filename = 
			if Array.length Sys.argv = 1 then
				Help, ""
	  		else if Array.length Sys.argv = 2 then
	  			check_single_argument (Sys.argv.(1))
			else if Array.length Sys.argv = 3 then 
				get_action Sys.argv.(1), Sys.argv.(2)
			else raise (Exceptions.InvalidNumberCompilerArguments (Array.length Sys.argv)) 
		in 
		(* Added fun () -> <x> so that each is evaluated only when requested *)
		let filename 	= Filepath.realpath filename in
		let file_in 	= fun () -> open_in filename in
	  	let lexbuf 		= fun () ->	Lexing.from_channel (file_in ()) in
	  	let token_list 	= fun () -> Processor.build_token_list (lexbuf ()) in
	  	let program 	= fun () -> Processor.parser filename (token_list ()) in
	  	let sprogram 	= fun () -> Analyzer.analyze filename (program ()) in
	  	let llm 		= fun () -> Codegen.codegen_sprogram (sprogram ()) in
	  (* let _ = Llvm_analysis.assert_valid_module llm in *)
	  match action with
	  		Help 			-> print_string help_string
		| 	Tokens 			-> print_string (Utils.token_list_to_string (token_list ()))
		| 	TokenEndl 		-> print_string (Utils.token_list_to_string_endl (token_list ()))
		| 	Ast 			-> print_string (pretty_to_string  (Utils.print_tree (program ())))
		| 	Sast 			-> print_string (pretty_to_string (Utils.map_sprogram_to_json (sprogram ())))
		| 	PrettyPrint 	-> print_string (Utils.string_of_program (program ()))
		| 	Compile 		-> dump_module (llm ())
		| 	CompileToFile 	-> print_module (dice_name filename) (llm ())
	with 
		Exceptions.IllegalCharacter(filename, c, ln) ->
			print_string
			(
				"In \"" ^ filename ^ "\", Illegal Character, '" ^
				Char.escaped c ^ "', line " ^ string_of_int ln ^ "\n"
			)
	| 	Exceptions.UnmatchedQuotation(ln) 	-> print_endline("Unmatched Quotation, line " ^ string_of_int ln)
	| 	Exceptions.IllegalToken(tok) 		-> print_endline("Illegal token " ^ tok)
	| 	Exceptions.MissingEOF 				-> print_endline("Missing EOF")
	| 	Parsing.Parse_error ->
			print_string
			(
				"File \"" ^ !Processor.filename ^ "\", " ^
				"line " ^ string_of_int !Processor.line_number ^ ", " ^
				"character " ^ string_of_int !Processor.char_num ^ ", " ^
				"Syntax Error, token " ^ Utils.string_of_token !Processor.last_token ^ "\n" 
			)

	|  	Exceptions.InvalidNumberCompilerArguments i -> print_endline ("Invalid argument passed " ^ (string_of_int i)); print_string help_string
	| 	Exceptions.InvalidCompilerArgument s 		-> print_endline ("Invalid argument passed " ^ s); print_string help_string
	| 	Exceptions.NoFileArgument 					-> print_string ("Must include file argument\n" ^ help_string)

	| 	Exceptions.IncorrectNumberOfArgumentsException 			-> print_endline("Incorrect number of arguments passed to function")
	| 	Exceptions.ConstructorNotFound(cname) 					-> print_endline("Constructor" ^ cname ^ " not found")
	| 	Exceptions.DuplicateClassName(cname) 					-> print_endline("Class " ^ cname ^ " not found")
	| 	Exceptions.DuplicateField 								-> print_endline("Duplicate field defined")
	| 	Exceptions.DuplicateFunction(fname) 					-> print_endline("Duplicate function defined " ^ fname)
	| 	Exceptions.DuplicateConstructor 						-> print_endline("Duplicate constructor found")
	| 	Exceptions.DuplicateLocal(lname) 						-> print_endline("Duplicate local variable defined " ^ lname)
	| 	Exceptions.UndefinedClass(cname) 						-> print_endline("Undefined class " ^ cname)
	| 	Exceptions.UnknownIdentifier(id) 						-> print_endline("Unkown identifier " ^ id)
	| 	Exceptions.InvalidBinopExpression(binop) 				-> print_endline("Invalid binary expression " ^ binop)
	| 	Exceptions.InvalidIfStatementType 						-> print_endline("Invalid type passed to if statement, must be bool")
	| 	Exceptions.InvalidForStatementType 						-> print_endline("Invalid type passed to for loop, must be bool")
	| 	Exceptions.ReturnTypeMismatch(t1, t2)					-> print_endline("Incorrect return type " ^ t1 ^ " expected " ^ t2)
	| 	Exceptions.MainNotDefined 								-> print_endline("Main not found in program")
	| 	Exceptions.MultipleMainsDefined							-> print_endline("Multiple mains defined, can only define 1")
	| 	Exceptions.InvalidWhileStatementType 					-> print_endline("Invalid type passed to while loop, must be bool")
	| 	Exceptions.LocalAssignTypeMismatch(t1, t2) 				-> print_endline("Invalid assignment of " ^ t1 ^ " to " ^ t2)
	| 	Exceptions.InvalidUnaryOperation 						-> print_endline("Invalid unary operator")
	| 	Exceptions.AssignmentTypeMismatch(t1, t2) 				-> print_endline("Invalid assignment of " ^ t1 ^ " to " ^ t2)
	| 	Exceptions.FunctionNotFound(fname, scope) 				-> print_endline("function " ^ fname ^ " not found in scope " ^ scope)
	| 	Exceptions.UndefinedID(id) 								-> print_endline("Undefined id " ^ id)
	| 	Exceptions.InvalidAccessLHS(t) 							-> print_endline("Invalid LHS expression of dot operator with " ^ t)
	| 	Exceptions.LHSofRootAccessMustBeIDorFunc(lhs) 			-> print_endline("Dot operator expects ID, not " ^ lhs)
	| 	Exceptions.ObjAccessMustHaveObjectType(t) 				-> print_endline("Can only dereference objects, not " ^ t)
	| 	Exceptions.UnknownIdentifierForClass(c, id) 			-> print_endline("Unknown id " ^ id ^ " for class " ^ c)
	| 	Exceptions.CannotUseReservedFuncName(f) 				-> print_endline("Cannot use name " ^ f ^ " because it is reserved")
	| 	Exceptions.InvalidArrayPrimitiveConsecutiveTypes(t1,t2)	-> print_endline("Array primitive types must be equal, not " ^ t1 ^ " " ^ t2)
	| 	Exceptions.InvalidArrayPrimitiveType(t) 				-> print_endline("Array primitive type invalid, " ^ t)
	| 	Exceptions.MustPassIntegerTypeToArrayCreate				-> print_endline("Only integer types can be passed to an array initializer")
	| 	Exceptions.ArrayInitTypeInvalid(t) 						-> print_endline("Only integer types can be passed to an array initializer, not " ^ t)
	| 	Exceptions.MustPassIntegerTypeToArrayAccess 			-> print_endline("Only integer types can be passed to an array access")
	| 	Exceptions.ArrayAccessInvalidParamLength(o,a) 			-> print_endline("Only arrays can have access to length, not " ^ o ^ " " ^ a)
	| 	Exceptions.ArrayAccessExpressionNotArray(a) 			-> print_endline("This expression is not an array " ^ a)
	| 	Exceptions.CanOnlyAccessLengthOfArray 					-> print_endline("Can only access the length of an array")
	| 	Exceptions.CanOnlyDeleteObjectsOrArrays 				-> print_endline("Can only delete objects or arrays")
	| 	Exceptions.CannotAccessLengthOfCharArray 				-> print_endline("Cannot access the length of a char array")
	| 	Exceptions.AllNonVoidFunctionsMustEndWithReturn(f) 		-> print_endline("Non-void function " ^ f ^ " does not end in return")
	| 	Exceptions.CyclicalDependencyBetween(c1, c2) 			-> print_endline("Class " ^ c1 ^ " and " ^ c2 ^ " have a cylical dependence")
	| 	Exceptions.CannotAccessPrivateFieldInNonProperScope(f, cp, cc) -> print_endline("Cannot access private field " ^ f ^ " in scope " ^ cp ^ " from object " ^ cc)
	| 	Exceptions.CannotCallBreakOutsideOfLoop 				-> print_endline("Cannot call break outside of loop")
	| 	Exceptions.CannotCallContinueOutsideOfLoop				-> print_endline("Cannot call continue outside of loop")
	| 	Exceptions.CannotAccessPrivateFunctionInNonProperScope(f, cp, cc) -> print_endline("Cannot access private function " ^ f ^ " in scope " ^ cp ^ " from object " ^ cc)
	| 	Exceptions.CannotPassNonInheritedClassesInPlaceOfOthers(c1, c2) 	-> print_endline("Cannot pass non-inherited classe" ^ c1 ^ " to parameter " ^ c2)
	| 	Exceptions.IncorrectTypePassedToFunction(id, t) 					-> print_endline("Canot pass type " ^ t ^ " to " ^ id)
	| 	Exceptions.IncorrectNumberOfArguments(f, a1, a2) -> print_endline("Cannot pass " ^ string_of_int a1 ^ " args when expecting " ^ string_of_int a2 ^ " in " ^f)
	| 	Exceptions.ClassIsNotExtendedBy(c1, c2) 			-> print_endline("Class " ^ c1 ^ " not extended by " ^ c2)

	| 	Exceptions.InvalidTypePassedToPrintf				-> print_endline("Invalid type passed to print")
	| 	Exceptions.InvalidBinaryOperator					-> print_endline("Invalid binary operator")
	| 	Exceptions.UnknownVariable(id) 						-> print_endline("Unknown variable " ^ id)
	| 	Exceptions.AssignLHSMustBeAssignable 				-> print_endline("Assignment lhs must be assignable")
	| 	Exceptions.CannotCastTypeException(t1, t2) 			-> print_endline("Cannot cast " ^ t1 ^ " to " ^ t2)
	| 	Exceptions.InvalidBinopEvaluationType 				-> print_endline("Invalid binary expression evaluation type")
	| 	Exceptions.FloatOpNotSupported 						-> print_endline("Float operation not supported")
	| 	Exceptions.IntOpNotSupported 						-> print_endline("Integer operation not supported")
	| 	Exceptions.LLVMFunctionNotFound(f)  				-> print_endline("LLVM function " ^ f ^ " not found")
	| 	Exceptions.InvalidStructType(t) 					-> print_endline("Invalid structure type " ^ t)
	| 	Exceptions.UnableToCallFunctionWithoutParent(f) 	-> print_endline("Unable to call function " ^ f ^ " without parent")
	| 	Exceptions.CannotAssignParam(p) 					-> print_endline("Cannot assign to param " ^ p)
	| 	Exceptions.InvalidUnopEvaluationType 				-> print_endline("Invalid unary expression evaluation type")
	| 	Exceptions.UnopNotSupported 						-> print_endline("Unary operator not supported")
	| 	Exceptions.ArrayLargerThan1Unsupported 				-> print_endline("Array dimensions greater than 1 not supported")
	| 	Exceptions.CanOnlyCompareObjectsWithNull(e1, e2) 	-> print_endline("Can only compare objects with null " ^ e1 ^ " " ^ e2)
	| 	Exceptions.ObjOpNotSupported(op) 					-> print_endline("Object operator not supported " ^ op)
	| 	Exceptions.CanOnlyCompareArraysWithNull(e1, e2) 	-> print_endline("Can only compare arrays with null " ^ e1 ^ " " ^ e2)