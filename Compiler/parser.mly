%{ open Ast %}

%token CLASS EXTENDS SCOPE CONSTRUCTOR INCLUDE THIS
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
%token <int> INT_LITERAL
%token <float> DOUBLE_LITERAL
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
   INCLUDE LPAREN ID RPAREN { $3 }


/******************
 CLASSES
******************/

cdecls:
    /* nothing */ { [] }
  | cdecl_list    { List.rev $1 }

cdecl_list:
    /* nothing */     { [] }
  | cdecl_list cdecl  { $2::$1 }

cdecl:
    CLASS ID LBRACE constructor_decls field_decls fdecl_list RBRACE { {
      cname = $2;
      extends = ();
      constructors = $4;
      fields = $5;
      methods = $6;
    } }
  | CLASS ID EXTENDS ID LBRACE constructor_decls field_list fdecl_list RBRACE { {
      cname = $2;
      extends = $4;
      constructors = $6;
      fields = $7;
      methods = $8;
    } }

/******************
 CONSTRUCTORS
******************/

constructor_decls:
    /* nothing */  { [] }
  | constructor_decl_list  { List.rev $1 }

constructor_decl_list:
   constructor {}

(* Class Declarations are composed of variable declarations and function declarations *)
cidecls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

cname:
    CLASS ID {$1}
  | CLASS ID EXTENDS ID



formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }


field_list:
    /*Nothing*/ { [] }
  | SCOPE ID    {}

fdecl:
   ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
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

field:
  SCOPE TYPE ID SEMI { { scope=$1;
                         fname=$3; } }

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
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | ID DOT ID        { Deref(ID, ID) }
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
  | expr OR     expr { Binop($1, Or,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
