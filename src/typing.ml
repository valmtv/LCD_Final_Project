(* This file contains the type system for the calcb language *)

type calc_type =
  | IntT
  | BoolT
  | StringT
  | UnitT
  | RefT of calc_type
  | FunT of calc_type * calc_type
  | TupleT of calc_type list
  | RecordT of (string * calc_type) list
  | ListT of calc_type
  | None of string

type ann = calc_type

type ast =
    Num of int
  | Bool of bool
  | Str of string
  | Unit

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
  | Id of ann * string

  | New of ann * ast
  | Deref of ann * ast
  | Assign of ann * ast * ast
  | Free of ann * ast

  | If of ann * ast * ast * ast
  | While of ann * ast * ast
  | Seq of ann * ast * ast

  | PrintInt of ann * ast
  | PrintBool of ann * ast
  | PrintString of ann * ast
  | PrintEndLine of ann

  | Fun of ann * string * calc_type * ast
  | App of ann * ast * ast

  | Tuple of ann * ast list
  | TupleAccess of ann * ast * int
  | Record of ann * (string * ast) list
  | RecordAccess of ann * ast * string
  | List of ann * ast list
  | ListAccess of ann * ast * ast

let type_of = function
  | Num _ -> IntT
  | Bool _ -> BoolT
  | Str _ -> StringT
  | Unit -> UnitT

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
  | Id (ann,_) -> ann

  | New (ann,_) -> ann
  | Deref (ann,_) -> ann
  | Assign (ann,_,_) -> ann
  | Free (ann,_) -> ann

  | If (ann,_,_,_) -> ann
  | While (ann,_,_) -> ann
  | Seq (ann,_,_) -> ann

  | PrintInt (ann,_) -> ann
  | PrintBool (ann,_) -> ann
  | PrintString (ann,_) -> ann
  | PrintEndLine ann -> ann

  | Fun (ann,_,_,_) -> ann
  | App (ann,_,_) -> ann

  | Tuple (ann, _) -> ann
  | TupleAccess (ann, _, _) -> ann
  | Record (ann, _) -> ann
  | RecordAccess (ann, _, _) -> ann
  | List (ann, _) -> ann
  | ListAccess (ann, _, _) -> ann

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

let rec unparse_type = function
  | IntT -> "int"
  | BoolT -> "boolean"
  | UnitT -> "unit"
  | StringT -> "string"
  | RefT t -> "ref " ^ unparse_type t
  | FunT (t1, t2) -> "(" ^ unparse_type t1 ^ " -> " ^ unparse_type t2 ^ ")"
  | TupleT ts -> "(" ^ String.concat " * " (List.map unparse_type ts) ^ ")"
  | RecordT fs -> "{" ^ String.concat "; " (List.map (fun (id,t) -> id ^ ":" ^ unparse_type t) fs) ^ "}"
  | ListT t -> "[" ^ unparse_type t ^ "]"
  
  | None m -> "typing error: "^m

let rec type_annotation_to_calc_type = function
  | Ast.TInt -> IntT
  | Ast.TBool -> BoolT
  | Ast.TString -> StringT
  | Ast.TUnit -> UnitT
  | Ast.TRef t -> RefT (type_annotation_to_calc_type t)
  | Ast.TFun (t1, t2) -> FunT (type_annotation_to_calc_type t1, type_annotation_to_calc_type t2)
  | Ast.TTuple ts -> TupleT (List.map type_annotation_to_calc_type ts)
  | Ast.TRecord fs -> 
      let sorted = List.sort (fun (id1,_) (id2,_) -> String.compare id1 id2) fs in
      RecordT (List.map (fun (id, t) -> (id, type_annotation_to_calc_type t)) sorted)
  | Ast.TList t -> ListT (type_annotation_to_calc_type t)

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
  | Ast.Str s -> Str s
  | Ast.Unit -> Unit

  | Ast.Id x ->
      (match Env.lookup env x with
       | Some t -> Id (t, x)
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

  | Ast.New e1 ->
      let e1' = typecheck_env env e1 in
      let t1 = type_of e1' in
      New (RefT t1, e1')

  | Ast.Deref e1 ->
      let e1' = typecheck_env env e1 in
      (match type_of e1' with
       | RefT t -> Deref (t, e1')
       | _ -> Deref (None "Expecting reference type", e1'))

  | Ast.Assign (e1, e2) ->
      let e1' = typecheck_env env e1 in
      let e2' = typecheck_env env e2 in
      (match type_of e1' with
       | RefT t when t = type_of e2' -> Assign (type_of e2', e1', e2')
       | RefT _ -> Assign (None "Type mismatch in assignment", e1', e2')
       | _ -> Assign (None "Expecting reference type on left side", e1', e2'))

  | Ast.Free e1 ->
      let e1' = typecheck_env env e1 in
      (match type_of e1' with
       | RefT _ -> Free (UnitT, e1')
       | _ -> Free (None "Expecting reference type", e1'))

  | Ast.If (e1, e2, e3) ->
      let e1' = typecheck_env env e1 in
      let e2' = typecheck_env env e2 in
      let e3' = typecheck_env env e3 in
      (match type_of e1' with
       | BoolT ->
           if type_of e2' = type_of e3' then
             If (type_of e2', e1', e2', e3')
           else
             If (None "Branches must have same type", e1', e2', e3')
       | _ -> If (None "Condition must be boolean", e1', e2', e3'))

  | Ast.While (e1, e2) ->
      let e1' = typecheck_env env e1 in
      let e2' = typecheck_env env e2 in
      (match type_of e1' with
       | BoolT -> While (UnitT, e1', e2')
       | _ -> While (None "Condition must be boolean", e1', e2'))

  | Ast.Seq (e1, e2) ->
      let e1' = typecheck_env env e1 in
      let e2' = typecheck_env env e2 in
      Seq (type_of e2', e1', e2')

  | Ast.PrintInt e1 ->
      let e1' = typecheck_env env e1 in
      (match type_of e1' with
       | IntT -> PrintInt (UnitT, e1')
       | _ -> PrintInt (None "Expecting integer", e1'))

  | Ast.PrintBool e1 ->
      let e1' = typecheck_env env e1 in
      (match type_of e1' with
       | BoolT -> PrintBool (UnitT, e1')
       | _ -> PrintBool (None "Expecting boolean", e1'))

  | Ast.PrintString e1 ->
      let e1' = typecheck_env env e1 in
      (match type_of e1' with
       | StringT -> PrintString (UnitT, e1')
       | _ -> PrintString (None "Expecting string", e1'))

  | Ast.PrintEndLine -> PrintEndLine UnitT

  | Ast.Fun (param, param_type, body) ->
      let param_calc_type = type_annotation_to_calc_type param_type in
      let env' = Env.begin_scope env in
      let env'' = Env.bind env' param param_calc_type in
      let typed_body = typecheck_env env'' body in
      let body_type = type_of typed_body in
      Fun (FunT (param_calc_type, body_type), param, param_calc_type, typed_body)

  | Ast.App (e1, e2) ->
      let e1' = typecheck_env env e1 in
      let e2' = typecheck_env env e2 in
      (match type_of e1' with
       | FunT (param_type, return_type) ->
           if param_type = type_of e2' then
             App (return_type, e1', e2')
           else
             App (None "Function argument type mismatch", e1', e2')
       | _ -> App (None "Expecting function type", e1', e2'))

  | Ast.Tuple es ->
      let typed_es = List.map (typecheck_env env) es in
      let types = List.map type_of typed_es in
      Tuple (TupleT types, typed_es)

  | Ast.TupleAccess (e, i) ->
      let typed_e = typecheck_env env e in
      (match type_of typed_e with
       | TupleT ts ->
           if i > 0 && i <= List.length ts then
             let t = List.nth ts (i - 1) in
             TupleAccess (t, typed_e, i)
           else
             TupleAccess (None ("Tuple index out of bounds: " ^ string_of_int i), typed_e, i)
       | _ -> TupleAccess (None "Expected tuple type for access", typed_e, i))
    
  | Ast.Record fields ->
      let typed_fields = List.map (fun (id, e) -> (id, typecheck_env env e)) fields in
      let sorted_fields = List.sort (fun (id1, _) (id2, _) -> String.compare id1 id2) typed_fields in
      let field_types = List.map (fun (id, e) -> (id, type_of e)) sorted_fields in
      Record (RecordT field_types, sorted_fields)

  | Ast.RecordAccess (e, id) ->
      let typed_e = typecheck_env env e in
      (match type_of typed_e with
       | RecordT fields ->
           (match List.assoc_opt id fields with
            | Some t -> RecordAccess (t, typed_e, id)
            | None -> RecordAccess (None ("Field not found: " ^ id), typed_e, id))
       | _ -> RecordAccess (None "Expected record type for access", typed_e, id))
    
  | Ast.List es ->
      if es = [] then
        (* Without type inference, we cant determine the type of an empty list
           Fail safely or return a generic/error type. *)
        List (None "Cannot determine type of empty list", [])
      else
        let typed_es = List.map (typecheck_env env) es in
        let t_head = type_of (List.hd typed_es) in
        
        (* Check that all elements have the same type as the first element *)
        if List.for_all (fun e -> type_of e = t_head) typed_es then
          List (ListT t_head, typed_es)
        else
          List (None "All list elements must have the same type", typed_es)

  | Ast.ListAccess (e, i) ->
      let typed_e = typecheck_env env e in
      let typed_i = typecheck_env env i in
      (match type_of typed_e with
       | ListT t ->
           if type_of typed_i = IntT then
             ListAccess (t, typed_e, typed_i)
           else
             ListAccess (None "List index must be an integer", typed_e, typed_i)
       | _ -> ListAccess (None "Expected list type for index access", typed_e, typed_i))

let typecheck e = typecheck_env Env.empty_env e