open Llvm
open Llvm_analysis
open Analyzer
open Utils
open Ast

type action = Tokens | TokenEndl | PrettyPrint | Ast | Compile

let _ =
  if Array.length Sys.argv < 2 then
    print_string (
      "Usage: dice [required-option] <source file>\n" ^
        "required-option:\n" ^
        "\t-tendl: Prints tokens with newlines intact\n" ^ 
        "\t-t: Prints token stream\n" ^
        "\t-p: Pretty prints Ast as a program\n" ^
        "\t-a: Prints abstract syntax tree\n" ^
        "\t-c: Compiles source\n"
    )
  else
    let action = List.assoc Sys.argv.(1) [ ("-tendl", TokenEndl);
                                           ("-t", Tokens);
                                           ("-p", PrettyPrint);
                                           ("-a", Ast);
                                           ("-c", Compile) ] and
    filename = Sys.argv.(2) in
    let file_in = open_in filename in
    try
      let lexbuf = Lexing.from_channel file_in in
      let token_list = Processor.build_token_list lexbuf in
      let program = Processor.parser filename token_list in
      let sprogram = Analyzer.analyze filename program in
      let llm = Codegen.codegen_sprogram sprogram in
      match action with
          Tokens -> print_string (Utils.token_list_to_string token_list)
        | TokenEndl -> print_string (Utils.token_list_to_string_endl token_list)
        | Ast ->
            print_string (Utils.print_tree program)
        | PrettyPrint ->
            print_string (Utils.string_of_program program)
        | Compile -> dump_module llm
(*               let _ = Llvm_analysis.assert_valid_module llm in
 *)              

    with
        Exceptions.IllegalCharacter(c, ln) ->
          print_string
          (
            "In \"" ^ filename ^ "\", Illegal Character, '" ^
            Char.escaped c ^ "', line " ^ string_of_int ln ^ "\n"
          )
      	| Exceptions.UnmatchedQuotation(ln) ->
          print_string("Unmatched Quotation, line " ^ string_of_int ln ^ "\n")
      	| Parsing.Parse_error ->
          print_string
          (
            "File \"" ^ !Processor.filename ^ "\", " ^
            "line " ^ string_of_int !Processor.line_number ^ ", " ^
            "character " ^ string_of_int !Processor.char_num ^ ", " ^
            "Syntax Error, token " ^ Utils.string_of_token !Processor.last_token ^ "\n" 
          )
		| 	Exceptions.DuplicateClassName			-> print_string "DuplicateClassName \n"
		| 	Exceptions.DuplicateField				-> print_string "DuplicateField \n"
		| 	Exceptions.DuplicateFunction			-> print_string "DuplicateFunction \n"
		| 	Exceptions.DuplicateConstructor			-> print_string "DuplicateConstructor \n"
		| 	Exceptions.InvalidBinopExpression		-> print_string "InvalidBinopExpression \n"
		| 	Exceptions.InvalidIfStatementType		-> print_string "InvalidIfStatementType \n"
		| 	Exceptions.InvalidForStatementType		-> print_string "InvalidForStatementType \n"
		| 	Exceptions.ReturnTypeMismatch			-> print_string "ReturnTypeMismatch \n"
		| 	Exceptions.MainNotDefined				-> print_string "MainNotDefined \n"
		| 	Exceptions.MultipleMainsDefined			-> print_string "MultipleMainsDefined \n"
		| 	Exceptions.InvalidWhileStatementType	-> print_string "InvalidWhileStatementType \n"
		| 	Exceptions.LocalTypeMismatch			-> print_string "LocalTypeMismatch \n"
		| _ -> Printexc.print_backtrace stdout
