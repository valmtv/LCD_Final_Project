open Ast

let rec optimize e =
  match e with
  | Num _ | Bool _ | Unit | Id _ -> e
  | Add (e1, e2) ->
      let o1 = optimize e1 in
      let o2 = optimize e2 in
      (match o1, o2 with
       | Num n1, Num n2 -> Num (n1 + n2)
       | _ -> Add (o1, o2))

  | Sub (e1, e2) ->
      let o1 = optimize e1 in
      let o2 = optimize e2 in
      (match o1, o2 with
       | Num n1, Num n2 -> Num (n1 - n2)
       | _ -> Sub (o1, o2))

  | Mul (e1, e2) ->
      let o1 = optimize e1 in
      let o2 = optimize e2 in
      (match o1, o2 with
       | Num n1, Num n2 -> Num (n1 * n2)
       | _ -> Mul (o1, o2))

  | Div (e1, e2) ->
      let o1 = optimize e1 in
      let o2 = optimize e2 in
      (match o1, o2 with
       | Num _, Num 0 -> Div (o1, o2)
       | Num n1, Num n2 -> Num (n1 / n2)
       | _ -> Div (o1, o2))

  | Neg e1 ->
      (match optimize e1 with
       | Num n -> Num (-n)
       | o1 -> Neg o1)

   | And (e1, e2) ->
      (match optimize e1, optimize e2 with
       | Bool true, o2 -> o2
       | Bool false, _ -> Bool false
       | o1, o2 -> And (o1, o2))
       
  | Or (e1, e2) ->
      (match optimize e1, optimize e2 with
       | Bool true, _ -> Bool true
       | Bool false, o2 -> o2
       | o1, o2 -> Or (o1, o2))

   | If (e1, e2, e3) ->
      (match optimize e1 with
       | Bool true -> optimize e2
       | Bool false -> optimize e3
       | o1 -> If (o1, optimize e2, optimize e3))

   | Let (bindings, body) ->
      let opt_bindings = List.map (fun (id, ex) -> (id, optimize ex)) bindings in
      Let (opt_bindings, optimize body)
  
  | While (e1, e2) -> While (optimize e1, optimize e2)
  | Seq (e1, e2) -> Seq (optimize e1, optimize e2)
  | PrintInt e1 -> PrintInt (optimize e1)
  | PrintBool e1 -> PrintBool (optimize e1)
  
  (* Non Optimized *)
  | _ -> e
