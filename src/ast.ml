(* This file contains the description of the calc language and some utils related to the AST *)

(* The abstract syntax tree (AST) type for the calc language *)
type ast = 
    Num of int
  | Bool of bool
  
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  
  | Eq of ast * ast
  | Neq of ast * ast
  | Lt of ast * ast
  | Le of ast * ast
  | Gt of ast * ast
  | Ge of ast * ast
  | Neg of ast
  
  | And of ast * ast
  | Or of ast * ast
  | Not of ast

let paren = fun p q s -> if p > q then "("^s^")" else s

(* This function converts an AST back to a string representation of the expression *)
let rec unparse_ast p e = 
  match e with
  | Num x -> string_of_int x
  | Bool b -> string_of_bool b
  | Add (e1,e2) -> paren p 10 (unparse_ast 10 e1 ^ " + " ^ unparse_ast 10 e2)
  | Sub (e1,e2) -> paren p 10 (unparse_ast 10 e1 ^ " - " ^ unparse_ast 11 e2)
  | Mul (e1,e2) -> paren p 30 (unparse_ast 20 e1 ^ " * " ^ unparse_ast 20 e2)
  | Div (e1,e2) -> paren p 20 (unparse_ast 20 e1 ^ " / " ^ unparse_ast 21 e2)
  | Neg e1 -> paren p 30 ("-"^unparse_ast 31 e1)
  | And (e1,e2) -> paren p 5 (unparse_ast 5 e1 ^ " && " ^ unparse_ast 6 e2)
  | Or (e1,e2) -> paren p 3 (unparse_ast 3 e1 ^ " || " ^ unparse_ast 4 e2)
  | Not e1 -> paren p 30 ("not "^unparse_ast 31 e1)
  | Eq (e1,e2) -> paren p 7 (unparse_ast 7 e1 ^ " = " ^ unparse_ast 8 e2)
  | Neq (e1,e2) -> paren p 7 (unparse_ast 7 e1 ^ " != " ^ unparse_ast 8 e2)
  | Lt (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " < " ^ unparse_ast 10 e2)
  | Le (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " <= " ^ unparse_ast 10 e2)
  | Gt (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " > " ^ unparse_ast 10 e2)
  | Ge (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " >= " ^ unparse_ast 10 e2) 

