%{
open Ast
%}

%token <int> INT
%token <string> ID
%token PLUS MINUS TIMES DIV LPAREN RPAREN AND OR NOT EQ NEQ LT LE GT GE TRUE FALSE EOF
%token LET IN LETAND

%start main
%type <Ast.ast> main

%%
main:
  expr EOF                { $1 }

expr:
  | LET bindings IN expr  { Let($2, $4) }
  | disj                  { $1 }

bindings:
  | ID EQ disj                    { [($1, $3)] }
  | ID EQ disj LETAND bindings    { ($1, $3) :: $5 }

disj:
  | disj OR conj         { Or($1,$3)}
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
  | LPAREN expr RPAREN    { $2 }
  | MINUS factor          { Neg $2 }
  | NOT factor            { Not $2 }
;