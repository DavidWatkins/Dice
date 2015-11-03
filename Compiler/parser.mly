%{ open Ast %}

%token CLASS EXTENDS CONSTRUCTOR INCLUDE DOT THIS 
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> BOOLEAN_LITERAL
%token <string> STRING_LITERAL
%token <string> ID TYPE SCOPE
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
    includes EOF { Program(fst $1, snd $1) }

includes:
    /* nothing */   { [], [] }
    | includes include_decl { ($2 :: fst $1), snd $1 }
    | includes cdecls   { $1, $2 }

cdecls:
    /* nothing */ { [] }
  | cdecls cdecl  { $2 :: $1 }

/******************
  INCLUDE
******************/

include_decl:
   INCLUDE LPAREN ID RPAREN SEMI { $3 }


/******************
 CLASSES
******************/

cdecl:
    CLASS ID LBRACE cbody RBRACE { {
      cname = $2;
      extends = "";
      body = $4
    } }
  | CLASS ID EXTENDS ID LBRACE cbody RBRACE { {
      cname = $2;
      extends = $4;
      body = $6
    } }

cbody:
  /* nothing */    { { 
             fields = [];
               constructors = [];
               methods = [];
             } }
 | cbody field     { { 
             fields = $2 :: $1.fields;
               constructors = $1.constructors;
               methods = $1.methods;
             } }
 | cbody constructor { { 
             fields = $1.fields;
               constructors = $2 :: $1.constructors;
               methods = $1.methods;
             } }
 | cbody fdecl     { { 
             fields = $1.fields;
               constructors = $1.constructors;
               methods = $2 :: $1.methods;
             } }


/******************
 CONSTRUCTORS
******************/

constructor:
  CONSTRUCTOR LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE {
    {
      scope = "Public";
      fname = "";
      formals = List.rev $3;
      locals = List.rev $6;
      body = List.rev $7;
    }
  }

/******************
 FIELDS
******************/

field:
    SCOPE TYPE ID SEMI { Field($1, $2, $3) }
    /* public UserObj name; */
  | SCOPE ID ID SEMI   { Field($1, $2, $3) }

/******************
 METHODS
******************/

fdecl:
    SCOPE TYPE ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE { 
      {
      scope = $1;
      fname = $3;
      formals = List.rev $5;
      locals = List.rev $8;
      body = List.rev $9;
      } 
    }

/******************
 FORMALS/PARAMETERS & VARIABLES & ACTUALS
******************/

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    formal                   { [$1] }
  | formal_list COMMA formal { $3 :: $1 }

formal:
    datatype ID { Formal($1, $2) }

datatype:
    array_type { $1 }
    | singular_type { $1 }

array_decl:
    TYPE ID LBRACKET RBRACKET { Arraytype($1) }

singular_type:
    TYPE { Datatype($1) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   type ID SEMI   { Vdecl($1, $2) }

type:
    ID     { Datatype($1) }
  | TYPE   { Datatype($1) }

/***************
  OBJECTS AND ARRAYS
***************/

  /* Create objects and arrays */
create:
    obj_create      { $1 }
  | array_create    { $1 }

  /* String(args) */
obj_create:
    ID LPAREN actuals_opt RPAREN    { ObjCreate($1, List.rev $3) }

  /* int a[3]; */
  /* int a[3,4] */
array_create:
      ID ID LBRACKET bracket_args RBRACKET   { ArrayCreate($1, $2, $4) }
    | TYPE ID LBRACKET bracket_args RBRACKET { ArrayCreate($1, $2, $4) }

bracket_args:
    INT_LITERAL                    { Int_Lit($1) }
  | bracket_args COMMA INT_LITERAL { $3 :: $1 }


  /* a.field */
  /* a.methodReturnsObject.field <- NOT SUPPORTED!! */
  /* a.method(actuals) */
  /* a[3] */
  /* method(4) */
access:
      obj_access  { $1 }
    | array_access { $1 }

obj_access: 
      ID DOT ID          { ObjAccess($1, $3)}
    | THIS DOT ID        { SelfAccess($3)}
    | ID DOT ID LPAREN actuals_opt RPAREN { ObjAccess($1, List.rev $3) }

array_access:
      ID LBRACKET bracket_args RBRACKET { ArrayAccess($1, $3)}
    | THIS DOT ID LBRACKET bracket_args RBRACKET { ThisArrayAccess($3, $5)}

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

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
    INT_LITERAL      { Literal($1)}
  | FLOAT_LITERAL    { Float_Lit($1)}
  | BOOLEAN_LITERAL  { Boolean_Lit($1)}
  | STRING_LITERAL   { String_Lit($1)}  
  | ID               { Id($1) }
  | create           { $1 }
  | access           { $1 }
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
  | ID ASSIGN expr   { Assign($1, $3) }
  | LPAREN expr RPAREN { $2 }