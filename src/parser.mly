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
%token SEMICOLON COMMA COLON ARROW
%token FUN TINT TBOOL TUNIT TREF DOT
%token LBRACE RBRACE

%start main
%type <Ast.ast> main

%%
main:
  | expr EOF                { $1 }

expr:
  | LET bindings IN expr  { Let($2, $4) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | WHILE expr DO assign    { While($2, $4) }
  | FUN LPAREN ID COLON typ RPAREN ARROW expr { Fun($3, $5, $8) }
  | seq                   { $1 }

while_body:
  | LPAREN seq RPAREN     { $2 }
  | assign                { $1 }

typ:
  | typ_arrow             { $1 }

typ_arrow:
  | typ_tuple ARROW typ_arrow { TFun($1, $3) }
  | typ_tuple                 { $1 }

typ_tuple:
  | typ_base TIMES typ_tuple_list { TTuple($1 :: $3) }
  | typ_base                      { $1 }

typ_tuple_list:
  | typ_base TIMES typ_tuple_list { $1 :: $3 }
  | typ_base                      { [$1] }

typ_record_fields:
  | ID COLON typ SEMICOLON typ_record_fields { ($1, $3) :: $5 }
  | ID COLON typ                             { [($1, $3)] }

typ_base:
  | TINT                  { TInt }
  | TBOOL                 { TBool }
  | TUNIT                 { TUnit }
  | TREF typ_base         { TRef $2 }
  | LPAREN typ RPAREN     { $2 }
  | LBRACE typ_record_fields RBRACE { TRecord $2 }

bindings:
  | ID EQ expr_no_seq              { [($1, $3)] }
  | ID EQ expr_no_seq LETAND bindings { ($1, $3) :: $5 }

expr_no_seq:
  | FUN LPAREN ID COLON typ RPAREN ARROW expr_no_seq { Fun($3, $5, $8) }
  | assign                { $1 }

seq:
  | assign SEMICOLON seq  { Seq($1, $3) }
  | assign SEMICOLON assign { Seq($1, $3) }
  | seq SEMICOLON assign { Seq($1, $3) }
  | assign                { $1 }

assign:
  | disj ASSIGN assign    { Assign($1, $3) }
  | WHILE expr DO while_body { While($2, $4) }
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
  | term TIMES  app       { Mul ($1, $3) }
  | term DIV    app       { Div ($1, $3) }
  | app                   {$1}

app:
  | app LPAREN expr RPAREN { App($1, $3) }
  | app DOT INT            { TupleAccess($1, $3) }
  | app DOT ID             { RecordAccess($1, $3) }
  | factor                 { $1 }

factor:
  | TRUE                  { Bool true }
  | FALSE                 { Bool false }
  | INT                   { Num $1 }
  | ID                    { Id $1 }
  | LPAREN expr_list RPAREN { 
      match $2 with 
      | [] -> Unit 
      | [e] -> e 
      | es -> Tuple(es) 
    }  
  | MINUS factor          { Neg $2 }
  | NOT factor            { Not $2 }
  | DEREF factor          { Deref $2 }
  | NEW LPAREN expr RPAREN { New $3 }
  | LBRACE record_fields RBRACE { Record $2 }
  | FREE LPAREN expr RPAREN { Free $3 }
  | PRINTINT LPAREN expr RPAREN { PrintInt $3 }
  | PRINTBOOL LPAREN expr RPAREN { PrintBool $3 }
  | PRINTENDLINE LPAREN RPAREN { PrintEndLine }

expr_list:
  | expr COMMA expr_list { $1 :: $3 }
  | expr                 { [$1] }
  | /* empty */          { [] }

record_fields:
  | ID EQ expr_no_seq SEMICOLON record_fields { ($1, $3) :: $5 }
  | ID EQ expr_no_seq                         { [($1, $3)] }