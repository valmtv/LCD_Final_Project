(* This file contains the type system for the calcb language *)

type calc_type =
  | IntT
  | BoolT
  | None of string

type ann = calc_type

type ast =
    Num of int
  | Bool of bool
  | Id of string

  | Add of ann * ast * ast
  | Sub of ann * ast * ast
  | Mul of ann * ast * ast
  | Div of ann * ast * ast
  | Neg of ann * ast

  | Eq of ann * ast * ast
  | Neq of ann * ast * ast
  | Lt of ann * ast * ast
  | Le of ann * ast * ast
  | Gt of ann * ast * ast
  | Ge of ann * ast * ast

  | And of ann * ast * ast
  | Or of ann * ast * ast
  | Not of ann * ast

  | Let of ann * (string * ast) list * ast

let type_of = function
  | Num _ -> IntT
  | Bool _ -> BoolT
  | Id _ -> None "Identifier should have been replaced during typechecking"

  | Add (ann,_,_) -> ann
  | Sub (ann,_,_) -> ann
  | Mul (ann,_,_) -> ann
  | Div (ann,_,_) -> ann
  | Neg (ann,_) -> ann

  | Eq (ann,_,_) -> ann
  | Neq (ann,_,_) -> ann
  | Lt (ann,_,_) -> ann
  | Le (ann,_,_) -> ann
  | Gt (ann,_,_) -> ann
  | Ge (ann,_,_) -> ann

  | And (ann,_,_) -> ann
  | Or (ann,_,_) -> ann
  | Not (ann,_) -> ann

  | Let (ann,_,_) -> ann

let mk_add t e1 e2 = Add (t,e1,e2)
let mk_sub t e1 e2 = Sub (t,e1,e2)
let mk_mul t e1 e2 = Mul (t,e1,e2)
let mk_div t e1 e2 = Div (t,e1,e2)
let mk_neg t e1 = Neg (t,e1)

let mk_and t e1 e2 = And (t,e1,e2)
let mk_or t e1 e2 = Or (t,e1,e2)
let mk_not t e1 = Not (t,e1)

let mk_eq t e1 e2 = Eq (t,e1,e2)
let mk_neq t e1 e2 = Neq (t,e1,e2)
let mk_lt t e1 e2 = Lt (t,e1,e2)
let mk_le t e1 e2 = Le (t,e1,e2)
let mk_gt t e1 e2 = Gt (t,e1,e2)
let mk_ge t e1 e2 = Ge (t,e1,e2)

let mk_let t bindings body = Let (t,bindings,body)

let unparse_type = function
  | IntT -> "int"
  | BoolT -> "boolean"
  | None m -> "typing error: "^m

let type_int_int_int_bin_op mk e1 e2 =
  match type_of e1, type_of e2 with
  | IntT, IntT -> mk IntT e1 e2
  | _ -> mk (None "Expecting Integer") e1 e2

let type_int_int_bin_op mk e1 =
  match type_of e1 with
  | IntT -> mk IntT e1
  | _ -> mk (None "Expecting Integer") e1

let type_bool_bool_bool_bin_op mk e1 e2 =
  match type_of e1, type_of e2 with
  | BoolT, BoolT -> mk BoolT e1 e2
  | _ -> mk (None "Expecting Boolean") e1 e2

let type_bool_bool_bin_op mk e1 =
  match type_of e1 with
  | BoolT -> mk BoolT e1
  | _ -> mk (None "Expecting Boolean") e1

let type_int_int_bool_bin_op mk e1 e2 =
  match type_of e1, type_of e2 with
  | IntT, IntT -> mk BoolT e1 e2
  | _ -> mk (None "Expecting Integer") e1 e2

let type_a_a_bool_eqop mk e1 e2 =
  if type_of e1 = type_of e2
    then mk BoolT e1 e2
    else mk (None "Expecting equal types") e1 e2

let rec typecheck_env env e =
  match e with
  | Ast.Num n -> Num n
  | Ast.Bool b -> Bool b
  | Ast.Id x ->
      (match Env.lookup env x with
       | Some _ -> Id x
       | None -> failwith ("Unbound variable: " ^ x))
  | Ast.Add (e1,e2) -> type_int_int_int_bin_op mk_add (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Sub (e1,e2) -> type_int_int_int_bin_op mk_sub (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Mul (e1,e2) -> type_int_int_int_bin_op mk_mul (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Div (e1,e2) -> type_int_int_int_bin_op mk_div (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Neg e1 ->  type_int_int_bin_op mk_neg (typecheck_env env e1)
  | Ast.And (e1,e2) -> type_bool_bool_bool_bin_op mk_and (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Or (e1,e2) -> type_bool_bool_bool_bin_op mk_or (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Not e1 -> type_bool_bool_bin_op mk_not (typecheck_env env e1)
  | Ast.Eq (e1,e2) -> type_a_a_bool_eqop mk_eq (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Neq (e1,e2) -> type_a_a_bool_eqop mk_neq (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Lt (e1,e2) -> type_int_int_bool_bin_op mk_lt (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Le (e1,e2) -> type_int_int_bool_bin_op mk_le (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Gt (e1,e2) -> type_int_int_bool_bin_op mk_gt (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Ge (e1,e2) -> type_int_int_bool_bin_op mk_ge (typecheck_env env e1) (typecheck_env env e2)
  | Ast.Let (bindings, body) ->
      let env' = Env.begin_scope env in
      let env'', typed_bindings = List.fold_left (fun (acc_env, acc_bindings) (id, expr) ->
        let typed_expr = typecheck_env env' expr in
        let expr_type = type_of typed_expr in
        let new_env = Env.bind acc_env id expr_type in
        (new_env, acc_bindings @ [(id, typed_expr)])
      ) (env', []) bindings in
      let typed_body = typecheck_env env'' body in
      let body_type = type_of typed_body in
      mk_let body_type typed_bindings typed_body

let typecheck e = typecheck_env Env.empty_env e