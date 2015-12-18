{ 
	open Parser 
    let lineno = ref 1
    let depth = ref 0
    let filename = ref ""

    let unescape s =
    	Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let string = '"' ( (ascii | escape)* as s) '"'
let char = ''' ( ascii | digit ) '''
let float = (digit+) ['.'] digit+
let int = digit+
let whitespace = [' ' '\t' '\r']
let return = '\n'

rule token = parse
  whitespace { token lexbuf }
| return 	 { incr lineno; token lexbuf}
| "(*"       { incr depth; comment lexbuf }

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
| '|'	   { BAR }

(* Branch Control *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Data Types *)
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "char"   { CHAR }
| "void"   { VOID }
| "null"   { NULL }
| "true"   { TRUE }
| "false"  { FALSE }

(* Classes *)
| "class"       { CLASS }
| "constructor" { CONSTRUCTOR }
| "public"      { PUBLIC }
| "private"     { PRIVATE }
| "extends"     { EXTENDS }
| "include"     { INCLUDE }
| "this"        { THIS }
| "break" 		{ BREAK }
| "continue"	{ CONTINUE }
| "new" 		{ NEW }
| "delete" 		{ DELETE }

| int as lxm   		{ INT_LITERAL(int_of_string lxm) }
| float as lxm 		{ FLOAT_LITERAL(float_of_string lxm) }
| char as lxm  		{ CHAR_LITERAL( String.get lxm 1 ) }
| escape_char as lxm{ CHAR_LITERAL( String.get (unescape lxm) 1) }
| string       		{ STRING_LITERAL(unescape s) }
| id as lxm    		{ ID(lxm) }
| eof          		{ EOF }

| '"' 			{ raise (Exceptions.UnmatchedQuotation(!lineno)) }
| _ as illegal  { raise (Exceptions.IllegalCharacter(!filename, illegal, !lineno)) }

and comment = parse
    	return 	{ incr lineno; comment lexbuf }
  	|	"*)" 	{ decr depth; print_string (string_of_int (!depth)); if !depth > 0 then comment lexbuf else token lexbuf }
	|   "(*" 	{ incr depth; comment lexbuf }
	|	_    	{ comment lexbuf }