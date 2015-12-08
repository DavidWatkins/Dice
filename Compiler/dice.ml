open Llvm
open Llvm_analysis
open Analyzer
open Utils
open Ast
open Yojson
open Exceptions

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
	| 	Exceptions.UnmatchedQuotation(ln) -> print_string("Unmatched Quotation, line " ^ string_of_int ln ^ "\n")
	| 	Parsing.Parse_error ->
			print_string
			(
				"File \"" ^ !Processor.filename ^ "\", " ^
				"line " ^ string_of_int !Processor.line_number ^ ", " ^
				"character " ^ string_of_int !Processor.char_num ^ ", " ^
				"Syntax Error, token " ^ Utils.string_of_token !Processor.last_token ^ "\n" 
			)

	| 	Exceptions.NoFileArgument 				-> print_string ("Must include file argument\n" ^ help_string)
	|   Exceptions.ConstructorNotFound(str)     -> print_string ("ConstructorNotFound: " ^ str ^ "\n")
	| 	Exceptions.DuplicateClassName			-> print_string "DuplicateClassName \n"
	| 	Exceptions.DuplicateField				-> print_string "DuplicateField \n"
	| 	Exceptions.DuplicateFunction(str)		-> print_string ("DuplicateFunction: " ^ str ^ "\n")
	| 	Exceptions.DuplicateConstructor			-> print_string "DuplicateConstructor \n"
	|   Exceptions.DuplicateLocal(str)          -> print_string ("DuplicateLocal: " ^ str ^ "\n")
	|   Exceptions.UndefinedClass(str)        	-> print_string("UndefinedClass: " ^ str ^ "\n")

	|   Exceptions.UnknownIdentifier(str)       -> print_string("UnknownIdentifier: " ^ str ^ "\n")
	| 	Exceptions.InvalidBinopExpression(str)	-> print_string ("InvalidBinopExpression: " ^ str ^ "\n")
	| 	Exceptions.InvalidIfStatementType		-> print_string "InvalidIfStatementType \n"
	| 	Exceptions.InvalidForStatementType		-> print_string "InvalidForStatementType \n"
	| 	Exceptions.ReturnTypeMismatch			-> print_string "ReturnTypeMismatch \n"
	| 	Exceptions.MainNotDefined				-> print_string "MainNotDefined \n"
	| 	Exceptions.MultipleMainsDefined			-> print_string "MultipleMainsDefined \n"
	| 	Exceptions.InvalidWhileStatementType	-> print_string "InvalidWhileStatementType \n"
	| 	Exceptions.LocalTypeMismatch			-> print_string "LocalTypeMismatch \n"
	|   Exceptions.InvalidUnaryOperation 		-> print_string "InvalidUnaryOperation\n" 
	|   Exceptions.AssignmentTypeMismatch 		-> print_string "AssignmentTypeMismatch\n" 
	|   Exceptions.InvalidTypePassedToPrintf 	-> print_string "InvalidTypePassedToPrintf\n" 
	|   Exceptions.InvalidBinaryOperator 		-> print_string "InvalidBinaryOperator\n"     

	|  	Exceptions.InvalidNumberCompilerArguments i -> print_endline ("Invalid argument passed " ^ (string_of_int i)); print_string help_string
	| 	Exceptions.InvalidCompilerArgument s 		-> print_endline ("Invalid argument passed " ^ s); print_string help_string

	(* | 	_ -> Printexc.print_backtrace stdout; print_string "Something went horribly wrong\n" *)