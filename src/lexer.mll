{
  open Parser
  exception Lexing_error of string
}

rule read = parse
  | [' ' '\t' '\r' '\n']     { read lexbuf }       (* skip whitespace *)
  | ['0'-'9']+ as i          { INT (int_of_string i) }
  | "true"                   { TRUE }
  | "false"                  { FALSE }
  | "let"                    { LET }
  | "in"                     { IN }
  | "and"                    { LETAND }
  | "if"                     { IF }
  | "then"                   { THEN }
  | "else"                   { ELSE }
  | "while"                  { WHILE }
  | "do"                     { DO }
  | "new"                    { NEW }
  | "free"                   { FREE }
  | "printInt"               { PRINTINT }
  | "printBool"              { PRINTBOOL }
  | "printEndLine"           { PRINTENDLINE }
  | "fun"                    { FUN }
  | "int"                    { TINT }
  | "bool"                   { TBOOL }
  | "unit"                   { TUNIT }
  | "ref"                    { TREF }
  | "&&"                     { AND }
  | "||"                     { OR }
  | "not"                    { NOT }
  | "->"                     { ARROW }
  | ":="                     { ASSIGN }
  | ":"                      { COLON }
  | "="                      { EQ }
  | "!="                     { NEQ }
  | "!"                      { DEREF }
  | "<"                      { LT }
  | ">"                      { GT }
  | "<="                     { LE }
  | ">="                     { GE }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { TIMES }
  | '/'                      { DIV }
  | '('                      { LPAREN }
  | ')'                      { RPAREN }
  | ';'                      { SEMICOLON }
  | ','                      { COMMA }
  | '.'                      { DOT }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID id }
  | eof                      { EOF }
  | _ as c                   { raise (Lexing_error (Printf.sprintf "Unexpected char: %c" c)) }