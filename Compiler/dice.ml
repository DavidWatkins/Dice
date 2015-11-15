open Llvm
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
        "\t-c: Compiles to Java\n"
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
      match action with
          Tokens -> print_string (Utils.token_list_to_string token_list)
        | TokenEndl -> print_string (Utils.token_list_to_string_endl token_list)
        | Ast ->
            print_string (Utils.print_tree program)
        | PrettyPrint ->
            print_string (Utils.string_of_program program)
        | Compile ->
            (* let ll_filename = (Utils.replace ".dice" "" filename) ^ ".ll" in *)
            match program with
              Program(includes, classes) -> 
            dump_module (Codegen.codegen_cdecls (Analyzer.analyze filename (includes, classes)))

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
