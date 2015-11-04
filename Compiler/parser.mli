type token =
  | CLASS
  | EXTENDS
  | CONSTRUCTOR
  | INCLUDE
  | DOT
  | THIS
  | PRIVATE
  | PUBLIC
  | INT
  | FLOAT
  | BOOL
  | CHAR
  | VOID
  | NULL
  | TRUE
  | FALSE
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | AND
  | NOT
  | OR
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | BAR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | ID of (string)
  | TYPE of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
