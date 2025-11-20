(* This file contains the definitional interpreter for the calc language *)

open Ast

type result =
  | IntV of int
  | BoolV of bool
  | UnitV
  | RefV of result ref
  | ClosureV of string * Ast.ast * result Env.env
  | TupleV of result list
  | RecordV of (string * result) list
  | ListV of result list

let rec unparse_result = function
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | UnitV -> "()"
  | RefV r -> "<ref " ^ unparse_result !r ^ ">"
  | ClosureV _ -> "<function>"
  | TupleV vs -> "(" ^ String.concat ", " (List.map unparse_result vs) ^ ")"
  | RecordV fs -> "{" ^ String.concat "; " (List.map (fun (id,v) -> id ^ "=" ^ unparse_result v) fs) ^ "}"
  | ListV vs -> "[" ^ String.concat ", " (List.map unparse_result vs) ^ "]"

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
  | Unit -> UnitV

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

  | New e1 ->
      let v = eval_env env e1 in
      RefV (Mem.new_ref v)

  | Deref e1 ->
      (match eval_env env e1 with
       | RefV r -> Mem.deref r
       | _ -> failwith "Runtime error: dereferencing non-reference")

  | Assign (e1, e2) ->
      let v2 = eval_env env e2 in
      (match eval_env env e1 with
       | RefV r -> Mem.assign r v2; v2
       | _ -> failwith "Runtime error: assigning to non-reference")

  | Free e1 ->
      (match eval_env env e1 with
       | RefV r -> Mem.free r; UnitV
       | _ -> failwith "Runtime error: freeing non-reference")

  | If (e1, e2, e3) ->
      (match eval_env env e1 with
       | BoolV true -> eval_env env e2
       | BoolV false -> eval_env env e3
       | _ -> failwith "Runtime error: condition must be boolean")

  | While (e1, e2) ->
      let rec loop () =
        match eval_env env e1 with
        | BoolV true -> let _ = eval_env env e2 in loop ()
        | BoolV false -> UnitV
        | _ -> failwith "Runtime error: condition must be boolean"
      in loop ()

  | Seq (e1, e2) ->
      let _ = eval_env env e1 in
      eval_env env e2

  | PrintInt e1 ->
      (match eval_env env e1 with
       | IntV n -> print_int n; UnitV
       | _ -> failwith "Runtime error: printInt expects integer")

  | PrintBool e1 ->
      (match eval_env env e1 with
       | BoolV b -> print_string (string_of_bool b); UnitV
       | _ -> failwith "Runtime error: printBool expects boolean")

  | PrintEndLine -> print_newline (); UnitV

  | Fun (param, _param_type, body) ->
      ClosureV (param, body, env)

  | App (e1, e2) ->
      let v1 = eval_env env e1 in
      let v2 = eval_env env e2 in
      (match v1 with
       | ClosureV (param, body, closure_env) ->
           let env' = Env.begin_scope closure_env in
           let env'' = Env.bind env' param v2 in
           eval_env env'' body
       | _ -> failwith "Runtime error: applying non-function")
    
  | Tuple es -> 
      TupleV (List.map (eval_env env) es)

  | TupleAccess (e, i) ->
      (match eval_env env e with
       | TupleV vs ->
           if i > 0 && i <= List.length vs then
             List.nth vs (i - 1)
           else
             failwith "Runtime error: Tuple index out of bounds"
       | _ -> failwith "Runtime error: Accessing non-tuple")

  | Record fields ->
      let v_fields = List.map (fun (id, e) -> (id, eval_env env e)) fields in
      RecordV v_fields

  | RecordAccess (e, id) ->
      (match eval_env env e with
       | RecordV fields ->
           (match List.assoc_opt id fields with
            | Some v -> v
            | None -> failwith ("Runtime error: Field " ^ id ^ " not found"))
       | _ -> failwith "Runtime error: Accessing non-record")

  | List (ann, es) ->
      let vs = List.map (eval_env env) es in
      ListV vs

  | ListAccess (ann, e, i_expr) ->
      let v_list = eval_env env e in
      let v_index = eval_env env i_expr in
      (match v_list, v_index with
       | ListV vs, IntV i ->
           if i >= 0 && i < List.length vs then
             List.nth vs i
           else
             failwith ("Runtime error: List index out of bounds: " ^ string_of_int i)
       | ListV _, _ -> failwith "Runtime error: List index must be an integer"
       | _, _ -> failwith "Runtime error: Accessing non-list")

let eval e = eval_env Env.empty_env e