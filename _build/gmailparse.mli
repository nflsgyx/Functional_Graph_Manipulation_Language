type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | DOLLAR
  | AT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | CONCAT
  | ASSIGN
  | DOT
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | FUNCTION
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | STRING
  | VOID
  | LIST
  | NODE
  | EDGE
  | GRAPH
  | FUNC
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | SLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
