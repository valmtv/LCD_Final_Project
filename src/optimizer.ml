open Ast

type env = (string * int) list

let remove_var id env = List.filter (fun (k, _) -> k <> id) env

let rec optimize env e =
  match e with
  | Id x -> 
      (match List.assoc_opt x env with
      | Some n -> Num n
      | None -> Id x)
  | Num _ | Bool _ | Unit -> e
  | Add (e1, e2) ->
      let o1 = optimize env e1 in
      let o2 = optimize env e2 in
      (match o1, o2 with
       | Num n1, Num n2 -> Num (n1 + n2)
       | _ -> Add (o1, o2))
  | Sub (e1, e2) ->
      let o1 = optimize env e1 in
      let o2 = optimize env e2 in
      (match o1, o2 with
       | Num n1, Num n2 -> Num (n1 - n2)
       | _ -> Sub (o1, o2))
  | Mul (e1, e2) ->
      let o1 = optimize env e1 in
      let o2 = optimize env e2 in
      (match o1, o2 with
       | Num n1, Num n2 -> Num (n1 * n2)
       | _ -> Mul (o1, o2))
  | Div (e1, e2) ->
      let o1 = optimize env e1 in
      let o2 = optimize env e2 in
      (match o1, o2 with
       | Num _, Num 0 -> Div (o1, o2)
       | Num n1, Num n2 -> Num (n1 / n2)
       | _ -> Div (o1, o2))
  | Neg e1 ->
      (match optimize env e1 with
       | Num n -> Num (-n)
       | o1 -> Neg o1)
  | And (e1, e2) ->
      (match optimize env e1 with
       | Bool true -> optimize env e2
       | Bool false -> Bool false
       | o1 -> And (o1, optimize env e2))
  | Or (e1, e2) ->
      (match optimize env e1 with
       | Bool true -> Bool true
       | Bool false -> optimize env e2
       | o1 -> Or (o1, optimize env e2))
  | Not e1 ->
      (match optimize env e1 with
       | Bool b -> Bool (not b)
       | o1 -> Not o1)
  | If (e1, e2, e3) ->
      (match optimize env e1 with
       | Bool true -> optimize env e2
       | Bool false -> optimize env e3
       | o1 -> If (o1, optimize env e2, optimize env e3))
  | Let (bindings, body) ->
      let opt_bindings = List.map (fun (id, ex) -> (id, optimize env ex)) bindings in
      let new_env = List.fold_left (fun acc (id, ex) ->
          match ex with
          | Num n -> (id, n) :: acc
          | _ -> acc
      ) env opt_bindings in
      let opt_body = optimize new_env body in
      (match opt_body with
       | Num n -> Num n 
       | Bool b -> Bool b
       | _ -> Let (opt_bindings, opt_body))
  | Fun (id, typ, body) ->
      let clean_env = remove_var id env in
      Fun (id, typ, optimize clean_env body)
  | While (e1, e2) -> While (optimize env e1, optimize env e2)
  | Seq (e1, e2) -> Seq (optimize env e1, optimize env e2)
  | App (e1, e2) -> App (optimize env e1, optimize env e2)
  | New e1 -> New (optimize env e1)
  | Deref e1 -> Deref (optimize env e1)
  | Assign (e1, e2) -> Assign (optimize env e1, optimize env e2)
  | PrintInt e1 -> PrintInt (optimize env e1)
  | PrintBool e1 -> PrintBool (optimize env e1)
  | Free e1 -> Free (optimize env e1)
  | _ -> e