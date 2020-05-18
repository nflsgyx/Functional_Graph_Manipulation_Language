/* Ocamlyacc parser for GMAIL */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA COLON DOLLAR AT PLUS MINUS TIMES DIVIDE CONCAT ASSIGN DOT
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token FUNCTION RETURN IF ELSE FOR WHILE
%token INT BOOL FLOAT STRING VOID LIST NODE EDGE GRAPH FUNC
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT SLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%nonassoc AT
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left CONCAT
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%left DOT
%nonassoc LPAREN RPAREN
%%


program: stmt_list EOF { List.rev $1 }

stmt_list:
  { [] }
| stmt_list stmt { $2 :: $1 }


typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }
  | STRING { String }
  | LIST LT typ GT { Lst($3) }
  | NODE LT typ GT { Node($3) }
  | EDGE LT typ GT { Edge($3) }
  | GRAPH LT typ COMMA typ GT { Graph($3, $5) }
  | func_type { Func($1) }


func_type:
    FUNC LPAREN typ_opt SEMI ret_typ RPAREN
    { { param_typs = $3;
        return_typ = $5 } }



/*
vdecl_list:
    { [] }
  | vdecl_list vdecl { $2 :: $1 }
*/

/*vdecl:*/
   /* typ ID SEMI { ($1, $2) } */
  /* | typ ID LBRACK LITERAL RBRACK SEMI {($1, $2)} */
/*  | typ ID LBRACK LITERAL RBRACK SEMI { (Array($1, $4), $2) }*/



stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | typ ID opt_init SEMI                    { VDecl($1, $2, $3) }
  | RETURN expr_opt SEMI                    { Return $2             }
  /*int foo(int i) int {}*/
  | FUNCTION ID LPAREN params_opt RPAREN ret_typ stmt {
      VDecl(Func({param_typs = List.map (fun (ty, _) -> ty) $4; return_typ = $6 }),
      $2, Some(FExpr({ name = $2; typ = $6; params = $4; body = $7 })))}
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

opt_init:
    { None }
  | ASSIGN expr { Some($2) }

expr_opt:
    { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	           { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | SLIT             { StrLit($1)             }
  | ID               { Id($1)                 }
  | arr_el           { ArrEl($1) }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr CONCAT expr { Binop($1, Concat, $3)  }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | lval ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | expr DOT ID LPAREN args_opt RPAREN {CMCall($3, $1::$5)}
  | LPAREN expr RPAREN { $2                   }
  | LBRACK exprs_opt RBRACK { Seq($2) }
  | expr AT expr { NodeExpr($1, $3) }
  /* | DOLLAR expr MINUS LPAREN expr RPAREN GT expr DOLLAR { EdgeExpr($2, $5, $8) }  */
  | expr MINUS GT expr MINUS GT expr { EdgeExpr($1, $4, $7) }
  /*$0-(2)>1$*/
  | function_expr { FExpr($1) }


function_expr:
    FUNCTION LPAREN params_opt RPAREN ret_typ stmt
    { { name = "";
        params = $3;
        typ = $5;
        body = $6} }

params_opt:
    { [] }
  | param_list { List.rev $1 }

param_list:
  typ ID { [($1, $2)] }
| param_list COMMA typ ID { ($3, $4) :: $1 }

typ_opt:
  { [] }
  | typ_list { List.rev $1 }

typ_list:
  typ { [$1] }
| typ_list COMMA typ { $3 :: $1 }

ret_typ:
  VOID { Void }
| typ { $1 }

lval:
    arr_el { LArrEl($1) }
  | ID { LId($1)}

arr_el:
    ID LBRACK expr RBRACK {OneDim($1, $3)}
  | arr_el LBRACK expr RBRACK {MultiDim($1, $3)}

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

exprs_opt:
    /* nothing */ { [] }
  | exprs_list  { List.rev $1 }

exprs_list:
    expr                    { [$1] }
  | exprs_list COMMA expr { $3 :: $1 }


