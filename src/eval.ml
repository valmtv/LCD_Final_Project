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

let rec eval e =
  match e with
  | Num n -> IntV n
  | Bool b -> BoolV b
  | Add (e1,e2) -> int_int_binop ( + ) (eval e1) (eval e2)
  | Sub (e1,e2) -> int_int_binop ( - ) (eval e1) (eval e2)
  | Mul (e1,e2) -> int_int_binop ( * ) (eval e1) (eval e2)
  | Div (e1,e2) -> int_int_binop ( / ) (eval e1) (eval e2)
  | Neg e1 ->  int_int_binop (-) (IntV 0) (eval e1)
  (* Short-circuit evaluation for And *)
  | And (e1,e2) -> begin match eval e1 with
                     | BoolV true -> eval e2
                     | BoolV false -> BoolV false
                     | _ -> failwith "Runtime typing error"
                   end
  (* Short-circuit evaluation for Or *)
  | Or (e1,e2) -> begin match eval e1 with
                    | BoolV true -> BoolV true
                    | BoolV false -> eval e2
                    | _ -> failwith "Runtime typing error"
                  end
  | Not e1 -> begin match eval e1 with
                | BoolV b -> BoolV (not b)
                | _ -> failwith "Runtime typing error"
              end
  | Eq (e1,e2) -> a_a_bool_eq (eval e1) (eval e2)
  | Neq (e1,e2) -> a_a_bool_neq (eval e1) (eval e2)
  | Lt (e1,e2) -> int_int_bool_binop ( < ) (eval e1) (eval e2)
  | Le (e1,e2) -> int_int_bool_binop ( <= ) (eval e1) (eval e2)
  | Gt (e1,e2) -> int_int_bool_binop ( > ) (eval e1) (eval e2)
  | Ge (e1,e2) -> int_int_bool_binop ( >= ) (eval e1) (eval e2)