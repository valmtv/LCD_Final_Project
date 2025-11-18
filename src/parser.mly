%{
open Ast
%}

%token <int> INT
%token <string> ID
%token PLUS MINUS TIMES DIV LPAREN RPAREN AND OR NOT EQ NEQ LT LE GT GE TRUE FALSE EOF
%token LET IN LETAND
%token IF THEN ELSE WHILE DO
%token NEW FREE DEREF ASSIGN
%token PRINTINT PRINTBOOL PRINTENDLINE
%token SEMICOLON

%start main
%type <Ast.ast> main

%%
main:
  expr EOF                { $1 }

expr:
  | LET bindings IN expr  { Let($2, $4) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | WHILE expr DO expr    { While($2, $4) }
  | seq                   { $1 }

bindings:
  | ID EQ seq                    { [($1, $3)] }
  | ID EQ seq LETAND bindings    { ($1, $3) :: $5 }

seq:
  | seq SEMICOLON assign  { Seq($1, $3) }
  | assign                { $1 }

assign:
  | disj ASSIGN assign    { Assign($1, $3) }
  | disj                  { $1 }

disj:
  | disj OR conj          { Or($1,$3)}
  | conj                  { $1 }

conj:
  | conj AND comp         { And($1,$3)}
  | comp                  { $1 }

comp:
  | comp EQ arith         { Eq($1, $3)}
  | comp NEQ arith        { Neq($1, $3) }
  | comp LT arith         { Lt($1, $3) }
  | comp LE arith         { Le($1, $3) }
  | comp GT arith         { Gt($1, $3) }
  | comp GE arith         { Ge($1, $3) }
  | arith                 {$1}

arith:
  | arith PLUS  term      { Add ($1, $3) }
  | arith MINUS term      { Sub ($1, $3) }
  | term                  { $1 }

term:
  | term TIMES  factor    { Mul ($1, $3) }
  | term DIV    factor    { Div ($1, $3) }
  | factor                {$1}

factor:
  | TRUE                  { Bool true }
  | FALSE                 { Bool false }
  | INT                   { Num $1 }
  | ID                    { Id $1 }
  | LPAREN RPAREN         { Unit }
  | LPAREN expr RPAREN    { $2 }
  | MINUS factor          { Neg $2 }
  | NOT factor            { Not $2 }
  | DEREF factor          { Deref $2 }
  | NEW LPAREN expr RPAREN { New $3 }
  | FREE LPAREN expr RPAREN { Free $3 }
  | PRINTINT LPAREN expr RPAREN { PrintInt $3 }
  | PRINTBOOL LPAREN expr RPAREN { PrintBool $3 }
  | PRINTENDLINE LPAREN RPAREN { PrintEndLine }
;