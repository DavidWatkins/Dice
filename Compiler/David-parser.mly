%{ open Ast %}

%token CLASS EXTENDS SCOPE CONSTRUCTOR INCLUDE THIS
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> BOOLEAN_LIT
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND NOT OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
  includes cdecls EOF { Program($1, $2) }


/******************
  INCLUDES
******************/

includes:
   /* nothing */ { [] }
  | include_list { List.rev $1 }

include_list:
    include              { [$1] }
  | include_list include { $2::$1 }

include:
   INCLUDE LPAREN ID RPAREN SEMI { $3 }


/******************
 CLASSES
******************/

cdecls:
    cdecl_list    { List.rev $1 }

cdecl_list:
    cdecl             { [$1] }
  | cdecl_list cdecl  { $2::$1 }

cdecl:
    CLASS ID LBRACE field_decls constructor_decls fdecl_list RBRACE { {
      cname = $2;
      extends = ();
      constructors = $5;
      fields = $4;
      methods = $6;
    } }
  | CLASS ID EXTENDS ID LBRACE field_list constructor_decls fdecl_list RBRACE { {
      cname = $2;
      extends = $4;
      constructors = $7;
      fields = $6;
      methods = $8;
    } }

/******************
 CONSTRUCTORS
******************/

constructor_decls:
    /* nothing */  { [] }
  | constructor_decl_list  { List.rev $1 }

constructor_decl_list:
   constructor_decl { [$1] }
  |constructor_decl_list constructor_decl { $2::$1 }

(* constructor(int a, int b,..., int z) { vdecl_list; stmt_list; } *)
constructor_decl:
  CONSTRUCTOR LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE

/******************
 FIELDS
******************/

field_list:
    /*Nothing*/ { [] }
    field       { [$1] }
  | field_list field { $2::$1 }

field:
    SCOPE TYPE ID SEMI { Field($1, $2, $3) }
  | SCOPE ID ID SEMI   { Field($1, $2, $3) }

/******************
 METHODS
******************/
fdecl:
   SCOPE ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = List.rev $6;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   TYPE ID SEMI { $2 }
/******************
 EXPRESSIONS
******************/

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr NOT    expr { Binop($1, Not,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | access           { Binop($1, Deref, $3) }
  | { ArrayAccess of expr * expr }
  | { ArrayCreate of datatype * string * expr list }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

  /* int a[3]; */
  /* int a[3][4]; */
  list_create:
      ID ID bracket_list   { ListCreate($1, $2, $3) }
    | TYPE ID bracket_list { ListCreate($1, $2, $3) }

  bracket_list:
      bracket { [$1] }
    | bracket_list bracket { $2 :: $1 }

  bracket:
    LBRACK INT_LITERAL RBRACK { Literal($2) }

  /* a[3] */
  /* a.var */
  /* a.toString() */
  access:
      obj_access  { $1 }
    | list_access { $1 }

  obj_access:
      expr ACCESS ID { ObjAccess($1, $3) }

  list_access:
      expr LBRACK expr RBRACK { ListAccess($1, $3) }

/***************
  PARAMS
***************/
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
