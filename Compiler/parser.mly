%{  open Ast  %}

%token CLASS EXTENDS CONSTRUCTOR INCLUDE DOT THIS PRIVATE PUBLIC
%token INT FLOAT BOOL CHAR VOID NULL TRUE FALSE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ BAR
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE NEW
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token <char> CHAR_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%right RBRACKET
%left LBRACKET 
%left DOT

%start program
%type <Ast.program> program

%%

program:
		includes cdecls EOF { Program($1, $2) }

/******************
	INCLUDE
******************/

includes:
		/* nothing */ { [] }
  	| 	include_list  { List.rev $1 }

include_list:
    	include_decl              { [$1] }
  	| 	include_list include_decl { $2::$1 }

include_decl:
	INCLUDE LPAREN ID RPAREN SEMI { Include($3) }


/******************
 CLASSES
******************/
cdecls:
    cdecl_list    { List.rev $1 }

cdecl_list:
    cdecl             { [$1] }
  | cdecl_list cdecl  { $2::$1 }

cdecl:
		CLASS ID LBRACE cbody RBRACE { {
			cname = $2;
			extends = NoParent;
			cbody = $4
		} }
	| 	CLASS ID EXTENDS ID LBRACE cbody RBRACE { {
			cname = $2;
			extends = Parent($4);
			cbody = $6
		} }

cbody:
		/* nothing */ { { 
			fields = [];
			constructors = [];
			methods = [];
		} }
 	| 	cbody field { { 
			fields = $2 :: $1.fields;
			constructors = $1.constructors;
			methods = $1.methods;
		} }
 	| 	cbody constructor { { 
			fields = $1.fields;
			constructors = $2 :: $1.constructors;
			methods = $1.methods;
		} }
 	| 	cbody fdecl { { 
			fields = $1.fields;
			constructors = $1.constructors;
			methods = $2 :: $1.methods;
		} }


/******************
 CONSTRUCTORS
******************/

constructor:
	CONSTRUCTOR LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE {
		{
			scope = Public;
			fname = Constructor;
			returnType = Datatype(ConstructorType);
			formals = $3;
			body = List.rev $6;
		}
	}

/******************
 FIELDS
******************/

scope:
		PRIVATE { Private }
	| 	PUBLIC  { Public }

/* public UserObj name; */
field:
		scope datatype ID SEMI { Field($1, $2, $3) }

/******************
 METHODS
******************/

fname:
	ID { $1 }

fdecl:
	scope datatype fname LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE 
	{ 
		{
			scope = $1;
			fname = FName($3);
			returnType = $2;
			formals = $5;
			body = List.rev $8;
		} 
	}

/******************
 FORMALS/PARAMETERS & VARIABLES & ACTUALS
******************/

formals_opt:
		/* nothing */ { [] }
	| 	formal_list   { List.rev $1 }

formal_list:
		formal                   { [$1] }
	| 	formal_list COMMA formal { $3 :: $1 }

formal:
	datatype ID { Formal($1, $2) }

actuals_opt:
		/* nothing */ { [] }
	| 	actuals_list  { List.rev $1 }

actuals_list:
		expr                    { [$1] }
	| 	actuals_list COMMA expr { $3 :: $1 }


/***************
	DATATYPES
***************/
primitive:
		INT 		{ Int_t }
	| 	FLOAT		{ Float_t } 
	| 	CHAR		{ Char_t }
	| 	BOOL 		{ Bool_t }
	| 	VOID    	{ Void_t }

name:
	CLASS ID { Objecttype($2) }

type_tag:
		primitive { $1 }
	|	name	  { $1 }

array_type:
	type_tag LBRACKET brackets RBRACKET { Arraytype($1, $3) }

datatype:
		type_tag   { Datatype($1) }
	| 	array_type { $1 }

brackets:
		/* nothing */ 			   { 1 }
	| 	brackets RBRACKET LBRACKET { $1 + 1 }

/******************
 EXPRESSIONS
******************/

stmt_list:
		/* nothing */  { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
		expr SEMI { Expr($1) }
	| 	RETURN expr SEMI { Return($2) }
	|	RETURN SEMI		 { Return(Noexpr) }
	| 	LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| 	IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
	| 	IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
	| 	FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
		 { For($3, $5, $7, $9) }
	| 	WHILE LPAREN expr RPAREN stmt 	{ While($3, $5) }
	|	BREAK SEMI					 	{ Break }
	|	CONTINUE SEMI				 	{ Continue }
	|   datatype ID SEMI 			 	{ Local($1, $2, Noexpr) }
	| 	datatype ID ASSIGN expr SEMI 	{ Local($1, $2, $4) }

expr_opt:
		/* nothing */ { Noexpr }
	| 	expr          { $1 }

expr:
		literals		 					{ $1 }
	| 	expr PLUS   expr 					{ Binop($1, Add,   $3) }
	| 	expr MINUS  expr 					{ Binop($1, Sub,   $3) }
	| 	expr TIMES  expr 					{ Binop($1, Mult,  $3) }
	| 	expr DIVIDE expr 					{ Binop($1, Div,   $3) }
	| 	expr EQ     expr 					{ Binop($1, Equal, $3) }
	| 	expr NEQ    expr 					{ Binop($1, Neq,   $3) }
	| 	expr LT     expr 					{ Binop($1, Less,  $3) }
	| 	expr LEQ    expr 					{ Binop($1, Leq,   $3) }
	| 	expr GT     expr 					{ Binop($1, Greater,  $3) }
	| 	expr GEQ    expr 					{ Binop($1, Geq,   $3) }
	| 	expr AND    expr 					{ Binop($1, And,   $3) }
	| 	NOT  expr 							{ Unop (Not,   $2) }
	| 	expr OR     expr 					{ Binop($1, Or,    $3) }
	| 	expr DOT    expr 					{ ObjAccess($1, $3) }
	| 	expr ASSIGN expr 					{ Assign($1, $3) }
	|   MINUS expr 							{ Unop (Sub, $2) }
	| 	ID LPAREN actuals_opt RPAREN 		{ Call($1, $3) }
	| 	NEW ID LPAREN actuals_opt RPAREN 	{ ObjectCreate($2, $4) }
	|	NEW type_tag bracket_args RBRACKET 	{ ArrayCreate(Datatype($2), List.rev $3) }
	| 	expr bracket_args RBRACKET		 	{ ArrayAccess($1, List.rev $2) } 
	| 	LPAREN expr RPAREN 					{ $2 }

bracket_args:
		LBRACKET expr						 { [$2] }
	| 	bracket_args RBRACKET LBRACKET expr { $4 :: $1 }

literals:
	  INT_LITERAL      		{ Int_Lit($1) }
	| FLOAT_LITERAL    		{ Float_Lit($1) }
	| TRUE			   		{ Boolean_Lit(true) }
	| FALSE			   		{ Boolean_Lit(false) }
	| STRING_LITERAL   		{ String_Lit($1) }  
	| CHAR_LITERAL			{ Char_Lit($1) }
	| THIS 			   		{ This }
	| ID 			   		{ Id($1) }	
	| NULL				    { Null }
	| BAR array_prim BAR 	{ ArrayPrimitive($2) }

/* ARRAY LITERALS */

array_prim:
		expr 					{ [$1] }
	|	array_prim COMMA expr 	{ $3 :: $1 }
