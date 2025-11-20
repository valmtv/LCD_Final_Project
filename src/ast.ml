(* This file contains the description of the calc language and some utils related to the AST *)

(* Type annotations for function parameters *)
type type_annotation =
  | TInt
  | TBool
  | TUnit
  | TString
  | TRef of type_annotation
  | TFun of type_annotation * type_annotation
  | TTuple of type_annotation list
  | TRecord of (string * type_annotation) list
  | TList of type_annotation

(* The abstract syntax tree (AST) type for the calc language *)
type ast =
    Num of int
  | Bool of bool
  | Str of string
  | Unit

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

  | Let of (string * ast) list * ast
  | Id of string

  | New of ast
  | Deref of ast
  | Assign of ast * ast
  | Free of ast

  | If of ast * ast * ast
  | While of ast * ast
  | Seq of ast * ast

  | PrintInt of ast
  | PrintBool of ast
  | PrintString of ast
  | IntToString of ast
  | PrintEndLine

  | Fun of string * type_annotation * ast
  | App of ast * ast

  | Tuple of ast list
  | TupleAccess of ast * int
  | Fst of ast
  | Snd of ast
  | Record of (string * ast) list
  | RecordAccess of ast * string
  | List of ast list
  | ListAccess of ast * ast

let rec unparse_type_annotation = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TRef t -> "ref " ^ unparse_type_annotation t
  | TFun (t1, t2) -> "(" ^ unparse_type_annotation t1 ^ " -> " ^ unparse_type_annotation t2 ^ ")"
  | TTuple ts -> "(" ^ String.concat " * " (List.map unparse_type_annotation ts) ^ ")"
  | TRecord fields -> "{" ^ String.concat "; " (List.map (fun (id, t) -> id ^ ":" ^ unparse_type_annotation t) fields) ^ "}"
  | TList t -> "[" ^ unparse_type_annotation t ^ "]"

let paren = fun p q s -> if p > q then "("^s^")" else s

(* This function converts an AST back to a string representation of the expression *)
let rec unparse_ast p e =
  match e with
  | Num x -> string_of_int x
  | Bool b -> string_of_bool b
  | Str s -> "\"" ^ s ^ "\""
  | Unit -> "()"
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
  | Let (bindings, body) ->
      let unparse_binding (id, expr) = id ^ " = " ^ unparse_ast 0 expr in
      let bindings_str = String.concat " and " (List.map unparse_binding bindings) in
      paren p 1 ("let " ^ bindings_str ^ " in " ^ unparse_ast 1 body)
  | Id x -> x
  | New e -> "new(" ^ unparse_ast 0 e ^ ")"
  | Deref e -> "!" ^ unparse_ast 30 e
  | Assign (e1, e2) -> paren p 2 (unparse_ast 3 e1 ^ " := " ^ unparse_ast 2 e2)
  | Free e -> "free(" ^ unparse_ast 0 e ^ ")"
  | If (e1, e2, e3) -> paren p 1 ("if " ^ unparse_ast 0 e1 ^ " then " ^ unparse_ast 0 e2 ^ " else " ^ unparse_ast 0 e3)
  | While (e1, e2) -> paren p 1 ("while " ^ unparse_ast 0 e1 ^ " do " ^ unparse_ast 0 e2)
  | Seq (e1, e2) -> paren p 1 (unparse_ast 1 e1 ^ "; " ^ unparse_ast 1 e2)
  | PrintInt e -> "printInt(" ^ unparse_ast 0 e ^ ")"
  | PrintBool e -> "printBool(" ^ unparse_ast 0 e ^ ")"
  | PrintString e -> "printString(" ^ unparse_ast 0 e ^ ")"
  | IntToString e -> "intToString(" ^ unparse_ast 0 e ^ ")"
  | PrintEndLine -> "printEndLine()"
  | Fun (param, typ, body) -> paren p 1 ("fun (" ^ param ^ ": " ^ unparse_type_annotation typ ^ ") -> " ^ unparse_ast 1 body)
  | App (e1, e2) -> paren p 40 (unparse_ast 40 e1 ^ "(" ^ unparse_ast 0 e2 ^ ")")
  | Tuple es -> "(" ^ String.concat ", " (List.map (unparse_ast 0) es) ^ ")"
  | TupleAccess (e, i) -> paren p 40 (unparse_ast 40 e ^ "." ^ string_of_int i)
  | Fst e -> "fst(" ^ unparse_ast 0 e ^ ")"
  | Snd e -> "snd(" ^ unparse_ast 0 e ^ ")"
  | Record fields -> "{" ^ String.concat "; " (List.map (fun (id, e) -> id ^ " = " ^ unparse_ast 0 e) fields) ^ "}"
  | RecordAccess (e, id) -> paren p 40 (unparse_ast 40 e ^ "." ^ id)
  | List es -> "[" ^ String.concat ", " (List.map (unparse_ast 0) es) ^ "]"
  | ListAccess (e, i) -> paren p 40 (unparse_ast 40 e ^ "[" ^ unparse_ast 0 i ^ "]")