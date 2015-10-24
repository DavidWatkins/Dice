{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "(*"     { comment lexbuf }           (* Comments *)

| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }

(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| '.'      { DOT }
| '['      { LBRACKET }
| ']'      { RBRACKET }

(* Branch Control *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Data Types *)
| "int"    { TYPE }
| "double" { TYPE }
| "bool"   { TYPE }
| "char"   { TYPE }
| "void"   { TYPE }
| "null"   { NULL }
| "true"   { BOOLEAN_LIT(true) }
| "false"  { BOOLEAN_LIT(false) }

(* Classes *)
| "class"       { CLASS }
| "constructor" { CONSTRUCTOR }
| "public"      { SCOPE }
| "private"     { SCOPE }
| "extends"     { EXTENDS }
| "include"     { INCLUDE }
| "this"        { THIS }

| ['-']['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
| ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
| ['-']['0'-'9']+'.'['0'-'9']+ as lxm { DOUBLE_LITERAL(double_of_string lxm) }
| ['0'-'9']+'.'['0'-'9']+ as lxm { DOUBLE_LITERAL(double_of_string lxm) }
| '"' (([' '-'!' '#'-'[' ']'-'~'] | '\\' ['\\' '"' 'n' 'r' 't'])* as s) '"' { STRING_LIT(s) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| (* Added String LIteral *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*)" { token lexbuf }
| _    { comment lexbuf }
