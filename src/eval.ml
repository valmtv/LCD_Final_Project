(* This file contains the definitional interpreter for the calc language *)

open Ast

type result =
  | IntV of int
  | BoolV of bool

let unparse_result = function
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b

let int_int_binop f r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> IntV (f n1 n2)
  | _ -> failwith "Runtime typing error"

let bool_bool_binop f r1 r2 =
  match r1, r2 with
  | BoolV n1, BoolV n2 -> BoolV (f n1 n2)
  | _ -> failwith "Runtime typing error"

let int_int_bool_binop f r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (f n1 n2)
  | _ -> failwith "Runtime typing error"

let a_a_bool_eq r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 = n2)
  | BoolV n1, BoolV n2 -> BoolV (n1 = n2)
  | _ -> failwith "Runtime typing error"

let a_a_bool_neq r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 <> n2)
  | BoolV n1, BoolV n2 -> BoolV (n1 <> n2)
  | _ -> failwith "Runtime typing error"

let rec eval_env env e =
  match e with
  | Num n -> IntV n
  | Bool b -> BoolV b
  | Id x ->
      (match Env.lookup env x with
       | Some v -> v
       | None -> failwith ("Unbound variable: " ^ x))
  | Add (e1,e2) -> int_int_binop ( + ) (eval_env env e1) (eval_env env e2)
  | Sub (e1,e2) -> int_int_binop ( - ) (eval_env env e1) (eval_env env e2)
  | Mul (e1,e2) -> int_int_binop ( * ) (eval_env env e1) (eval_env env e2)
  | Div (e1,e2) -> int_int_binop ( / ) (eval_env env e1) (eval_env env e2)
  | Neg e1 ->  int_int_binop (-) (IntV 0) (eval_env env e1)
  (* Short-circuit evaluation for And *)
  | And (e1,e2) -> begin match eval_env env e1 with
                     | BoolV true -> eval_env env e2
                     | BoolV false -> BoolV false
                     | _ -> failwith "Runtime typing error"
                   end
  (* Short-circuit evaluation for Or *)
  | Or (e1,e2) -> begin match eval_env env e1 with
                    | BoolV true -> BoolV true
                    | BoolV false -> eval_env env e2
                    | _ -> failwith "Runtime typing error"
                  end
  | Not e1 -> begin match eval_env env e1 with
                | BoolV b -> BoolV (not b)
                | _ -> failwith "Runtime typing error"
              end
  | Eq (e1,e2) -> a_a_bool_eq (eval_env env e1) (eval_env env e2)
  | Neq (e1,e2) -> a_a_bool_neq (eval_env env e1) (eval_env env e2)
  | Lt (e1,e2) -> int_int_bool_binop ( < ) (eval_env env e1) (eval_env env e2)
  | Le (e1,e2) -> int_int_bool_binop ( <= ) (eval_env env e1) (eval_env env e2)
  | Gt (e1,e2) -> int_int_bool_binop ( > ) (eval_env env e1) (eval_env env e2)
  | Ge (e1,e2) -> int_int_bool_binop ( >= ) (eval_env env e1) (eval_env env e2)
  | Let (bindings, body) ->
      let env' = Env.begin_scope env in
      let env'' = List.fold_left (fun acc_env (id, expr) ->
        let value = eval_env env' expr in
        Env.bind acc_env id value
      ) env' bindings in
      eval_env env'' body

let eval e = eval_env Env.empty_env e