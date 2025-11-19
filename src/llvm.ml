open Typing

type register = int

type label = int

type result =
  | Const of int
  | Register of register

type llvm =
  | Addi32 of register * result * result
  | Subi32 of register * result * result
  | Muli32 of register * result * result
  | Divi32 of register * result * result
  | CmpEq of Typing.calc_type * register * result * result
  | CmpNeq of Typing.calc_type * register * result * result
  | CmpLt of register * result * result
  | CmpLe of register * result * result
  | CmpGt of register * result * result
  | CmpGe of register * result * result
  | Xor of register * result
  | BrI1 of result * label * label
  | BrLabel of label
  | PhiI1 of register * (result * label) list
  | PhiI32 of register * (result * label) list
  | PhiPtr of register * (result * label) list
  | Call of register * string * (Typing.calc_type * result) list
  | CallVoid of string * (Typing.calc_type * result) list

let count = ref 0
let new_reg = fun () -> count := !count + 1; !count
let new_label = new_reg

(* Function counter for generating unique function names *)
let fun_count = ref 0
let new_fun_id () = fun_count := !fun_count + 1; !fun_count

(* Store generated function definitions *)
let function_defs = ref []

(* Code generation function *)

let rec compile_llvm env e label block =
  match e with
  | Num x -> Const x, env, label, block, []
  | Bool b when b = true -> Const 1, env, label, block, []
  | Bool b when b = false -> Const 0, env, label, block, []
  | Bool _ -> failwith "Invalid boolean value"
  | Unit -> Const 0, env, label, block, []

  | Id (_,x) ->
      begin match Env.lookup env x with
      | None -> failwith ("Unbound identifier: "^x)
      | Some r -> (r, env, label, block, [])
      end

  (* Arithmetic operations *)
  | Add (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[Addi32 (ret,r1,r2)], bs1@bs2)

  | Sub (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[Subi32 (ret,r1,r2)], bs1@bs2)

  | Mul (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[Muli32 (ret,r1,r2)], bs1@bs2)

  | Div (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[Divi32 (ret,r1,r2)], bs1@bs2)

  | Neg (_,e1) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let ret = new_reg() in
    (Register ret, env1, l1, b1@[Subi32 (ret, Const 0, r1)], bs1)

  (* Short-circuit AND: if e1 is false, skip e2 *)
  | And (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let label_b = new_label () in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 label_b [] in
    let label_phi = new_label () in
    let bs = bs1@[(l1,b1@[BrI1 (r1, label_b, label_phi)])]@bs2@[(l2,b2@[BrLabel label_phi])] in
    let ret = new_reg () in
    (Register ret, env2, label_phi, [PhiI1 (ret,[(Const 0, l1);(r2,l2)])], bs)

  (* Short-circuit OR: if e1 is true, skip e2 *)
  | Or (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let label_b = new_label () in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 label_b [] in
    let label_phi = new_label () in
    let bs = bs1@[(l1,b1@[BrI1 (r1, label_phi, label_b)])]@bs2@[(l2,b2@[BrLabel label_phi])] in
    let ret = new_reg () in
    (Register ret, env2, label_phi, [PhiI1 (ret,[(Const 1, l1);(r2,l2)])], bs)

  (* NOT operation *)
  | Not (_,e1) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let ret = new_reg() in
    (Register ret, env1, l1, b1@[Xor (ret, r1)], bs1)

  (* Equality comparison - works for both int and bool *)
  | Eq (t,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpEq (t,ret,r1,r2)], bs1@bs2)

  (* Inequality comparison - works for both int and bool *)
  | Neq (t,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpNeq (t,ret,r1,r2)], bs1@bs2)

  (* Integer comparison operations *)
  | Lt (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpLt (ret,r1,r2)], bs1@bs2)

  | Le (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpLe (ret,r1,r2)], bs1@bs2)

  | Gt (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpGt (ret,r1,r2)], bs1@bs2)

  | Ge (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpGe (ret,r1,r2)], bs1@bs2)

  | Let(_, bindings, body) ->
    let env' = Env.begin_scope env in
    let env'', l', b', bs' = List.fold_left (fun (acc_env, acc_label, acc_block, acc_blocks) (id, expr) ->
      let r, new_env, new_label, new_block, new_blocks = compile_llvm acc_env expr acc_label acc_block in
      let bound_env = Env.bind new_env id r in
      (bound_env, new_label, new_block, acc_blocks @ new_blocks)
    ) (env', label, block, []) bindings in
    let r_body, env_final, l_final, b_final, bs_body = compile_llvm env'' body l' b' in
    (r_body, env_final, l_final, b_final, bs' @ bs_body)

  (* Reference operations *)
  | New (t, e1) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let ret = new_reg() in
    let call_instr = match t with
      | RefT IntT -> Call (ret, "new_ref_int", [(IntT, r1)])
      | RefT BoolT -> Call (ret, "new_ref_bool", [(BoolT, r1)])
      | RefT (RefT _) -> Call (ret, "new_ref_ref", [(RefT UnitT, r1)])
      | RefT (FunT _) -> Call (ret, "new_ref_ref", [(RefT UnitT, r1)])
      | _ -> failwith "Internal error: new expects ref type"
    in
    (Register ret, env1, l1, b1@[call_instr], bs1)

  | Deref (_, e1) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let ret = new_reg() in
    let call_instr = match type_of e1 with
      | RefT IntT -> Call (ret, "deref_int", [(RefT IntT, r1)])
      | RefT BoolT -> Call (ret, "deref_bool", [(RefT BoolT, r1)])
      | RefT (RefT _) -> Call (ret, "deref_ref", [(RefT (RefT UnitT), r1)])
      | RefT (FunT _) -> Call (ret, "deref_ref", [(RefT UnitT, r1)])
      | _ -> failwith "Internal error: deref expects ref type"
    in
    (Register ret, env1, l1, b1@[call_instr], bs1)

  | Assign (_, e1, e2) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let r2, env2, l2, b2, bs2 = compile_llvm env1 e2 l1 b1 in
    let call_instr = match type_of e1 with
      | RefT IntT -> CallVoid ("assign_int", [(RefT IntT, r1); (IntT, r2)])
      | RefT BoolT -> CallVoid ("assign_bool", [(RefT BoolT, r1); (BoolT, r2)])
      | RefT (RefT _) -> CallVoid ("assign_ref", [(RefT (RefT UnitT), r1); (RefT UnitT, r2)])
      | RefT (FunT _) -> CallVoid ("assign_ref", [(RefT UnitT, r1); (RefT UnitT, r2)])
      | _ -> failwith "Internal error: assign expects ref type"
    in
    (r2, env2, l2, b2@[call_instr], bs1@bs2)

  | Free (_, e1) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let call_instr = CallVoid ("free_ref", [(RefT UnitT, r1)]) in
    (Const 0, env1, l1, b1@[call_instr], bs1)

  (* Control flow *)
  | If (t, e1, e2, e3) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let label_then = new_label () in
    let label_else = new_label () in
    let label_end = new_label () in

    let r2, _env2, l2, b2, bs2 = compile_llvm env1 e2 label_then [] in
    let r3, env3, l3, b3, bs3 = compile_llvm env1 e3 label_else [] in

    let bs = bs1 @ [(l1, b1 @ [BrI1 (r1, label_then, label_else)])] @
             bs2 @ [(l2, b2 @ [BrLabel label_end])] @
             bs3 @ [(l3, b3 @ [BrLabel label_end])] in

    let ret = new_reg() in
    let phi_instr = match t with
      | IntT -> PhiI32 (ret, [(r2, l2); (r3, l3)])
      | BoolT -> PhiI1 (ret, [(r2, l2); (r3, l3)])
      | RefT _ -> PhiPtr (ret, [(r2, l2); (r3, l3)])
      | FunT _ -> PhiPtr (ret, [(r2, l2); (r3, l3)])
      | UnitT -> PhiI32 (ret, [(r2, l2); (r3, l3)])
      | _ -> failwith "Unsupported type in if expression"
    in
    (Register ret, env3, label_end, [phi_instr], bs)

  | While (_, e1, e2) ->
    let label_cond = new_label () in
    let label_body = new_label () in
    let label_end = new_label () in

    let bs_pre = [(label, block @ [BrLabel label_cond])] in

    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label_cond [] in
    let _r2, env2, l2, b2, bs2 = compile_llvm env1 e2 label_body [] in

    let bs = bs_pre @ bs1 @ [(l1, b1 @ [BrI1 (r1, label_body, label_end)])] @
             bs2 @ [(l2, b2 @ [BrLabel label_cond])] in

    (Const 0, env2, label_end, [], bs)

  | Seq (_, e1, e2) ->
    let _r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let r2, env2, l2, b2, bs2 = compile_llvm env1 e2 l1 b1 in
    (r2, env2, l2, b2, bs1@bs2)

  (* Print operations *)
  | PrintInt (_, e1) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let call_instr = CallVoid ("print_int", [(IntT, r1)]) in
    (Const 0, env1, l1, b1@[call_instr], bs1)

  | PrintBool (_, e1) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let call_instr = CallVoid ("print_bool", [(BoolT, r1)]) in
    (Const 0, env1, l1, b1@[call_instr], bs1)

  | PrintEndLine _ ->
    let call_instr = CallVoid ("print_endline", []) in
    (Const 0, env, label, block@[call_instr], [])

  (* Function operations *)
  | Fun (t, param, param_type, body) ->
    let fun_id = new_fun_id () in
    let fun_name = "lambda_" ^ string_of_int fun_id in

    (* Compile the function body in a new environment with the parameter *)
    let param_env = Env.begin_scope Env.empty_env in
    let param_reg = Register (new_reg()) in
    let body_env = Env.bind param_env param param_reg in

    let body_result, _body_env, body_label, body_block, body_blocks =
      compile_llvm body_env body 0 [] in

    (* Store the function definition *)
    let return_type = match t with
      | FunT (_, ret_t) -> ret_t
      | _ -> failwith "Internal error: Fun must have FunT type"
    in

    function_defs := (fun_name, param_type, return_type, param_reg,
                      body_result, body_label, body_block, body_blocks) :: !function_defs;

    (* Create a closure - for now just return function pointer *)
    let ret = new_reg() in
    let call_instr = Call (ret, "create_closure", [(FunT (param_type, return_type), Const fun_id)]) in
    (Register ret, env, label, block@[call_instr], [])

  | App (_, e1, e2) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let r2, env2, l2, b2, bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    let call_instr = Call (ret, "apply_closure", [(FunT (UnitT, UnitT), r1); (UnitT, r2)]) in
    (Register ret, env2, l2, b2@[call_instr], bs1@bs2)

(* Unparse LLVM functions *)

let prologue =
  ["declare ptr @new_ref_int(i32)";
   "declare ptr @new_ref_bool(i1)";
   "declare ptr @new_ref_ref(ptr)";
   "declare i32 @deref_int(ptr)";
   "declare i1 @deref_bool(ptr)";
   "declare ptr @deref_ref(ptr)";
   "declare void @assign_int(ptr, i32)";
   "declare void @assign_bool(ptr, i1)";
   "declare void @assign_ref(ptr, ptr)";
   "declare void @free_ref(ptr)";
   "declare void @print_int(i32)";
   "declare void @print_bool(i1)";
   "declare void @print_endline()";
   "declare ptr @create_closure(i32)";
   "declare i32 @apply_closure(ptr, i32)";
   ""]

let epilogue =
   ["  ret i32 0";
    "}"]

let unparse_register n = "%"^string_of_int n

let unparse_label_use n = "%"^string_of_int n

let unparse_label_declaration l = (string_of_int l)^":"

let unparse_result = function
  | Const x -> string_of_int x
  | Register x -> unparse_register x

let unparse_type = function
  | IntT -> "i32"
  | BoolT -> "i1"
  | UnitT -> "i32"
  | RefT _ -> "ptr"
  | FunT _ -> "ptr"
  | _ -> failwith "Unknown type"

let unparse_llvm_i = function
  | Addi32 (r,l1,l2) ->
      "  "^unparse_register r^" = add nsw i32 "^unparse_result l1^", "^unparse_result l2
  | Subi32 (r,l1,l2) ->
      "  "^unparse_register r^" = sub nsw i32 "^unparse_result l1^", "^unparse_result l2
  | Muli32 (r,l1,l2) ->
      "  "^unparse_register r^" = mul nsw i32 "^unparse_result l1^", "^unparse_result l2
  | Divi32 (r,l1,l2) ->
      "  "^unparse_register r^" = sdiv i32 "^unparse_result l1^", "^unparse_result l2
  | BrI1 (r, l1, l2) ->
      "  br i1 "^unparse_result r^", label "^unparse_label_use l1^", label "^unparse_label_use l2
  | BrLabel label ->
      "  br label "^unparse_label_use label
  | PhiI1 (r, l) ->
      "  "^unparse_register r^" = phi i1 "^String.concat ", " (List.map (fun (r,l) -> "["^unparse_result r^", "^unparse_label_use l^"]") l)
  | PhiI32 (r, l) ->
      "  "^unparse_register r^" = phi i32 "^String.concat ", " (List.map (fun (r,l) -> "["^unparse_result r^", "^unparse_label_use l^"]") l)
  | PhiPtr (r, l) ->
      "  "^unparse_register r^" = phi ptr "^String.concat ", " (List.map (fun (r,l) -> "["^unparse_result r^", "^unparse_label_use l^"]") l)
  | Xor (r, l1) ->
      "  "^unparse_register r^" = xor i1 "^unparse_result l1^", 1"
  | CmpEq (IntT, r, l1, l2) ->
      "  "^unparse_register r^" = icmp eq i32 "^unparse_result l1^", "^unparse_result l2
  | CmpEq (BoolT, r, l1, l2) ->
      "  "^unparse_register r^" = icmp eq i1 "^unparse_result l1^", "^unparse_result l2
  | CmpEq (t, _, _, _) -> failwith ("Internal error: Cannot compare "^(unparse_type t))
  | CmpNeq (IntT, r, l1, l2) ->
      "  "^unparse_register r^" = icmp ne i32 "^unparse_result l1^", "^unparse_result l2
  | CmpNeq (BoolT, r, l1, l2) ->
      "  "^unparse_register r^" = icmp ne i1 "^unparse_result l1^", "^unparse_result l2
  | CmpNeq (t, _, _, _) -> failwith ("Internal error: Cannot compare "^(unparse_type t))
  | CmpLt (r, l1, l2) ->
      "  "^unparse_register r^" = icmp slt i32 "^unparse_result l1^", "^unparse_result l2
  | CmpLe (r, l1, l2) ->
      "  "^unparse_register r^" = icmp sle i32 "^unparse_result l1^", "^unparse_result l2
  | CmpGt (r, l1, l2) ->
      "  "^unparse_register r^" = icmp sgt i32 "^unparse_result l1^", "^unparse_result l2
  | CmpGe (r, l1, l2) ->
      "  "^unparse_register r^" = icmp sge i32 "^unparse_result l1^", "^unparse_result l2
  | Call (r, fname, args) ->
      let arg_strs = List.map (fun (t, v) -> unparse_type t ^ " " ^ unparse_result v) args in
      "  "^unparse_register r^" = call ptr @"^fname^"("^String.concat ", " arg_strs^")"
  | CallVoid (fname, args) ->
      let arg_strs = List.map (fun (t, v) -> unparse_type t ^ " " ^ unparse_result v) args in
      "  call void @"^fname^"("^String.concat ", " arg_strs^")"

let print_block (label, instructions) =
    print_endline (unparse_label_declaration label);
    List.iter (fun x -> x |> unparse_llvm_i |> print_endline) instructions

let print_blocks bs = List.iter print_block bs

let print_function_def (_fun_name, _param_type, _return_type, _param_reg,
                        _body_result, _body_label, _body_block, _body_blocks) =
  (* For now, we'll skip printing individual function definitions *)
  (* In a full implementation, each lambda would be a separate LLVM function *)
  ()

let print_llvm (_ret,_env,label,instructions,blocks) _t =
    (* Print the prologue *)
    List.iter print_endline prologue;

    (* Print function definitions *)
    List.iter print_function_def !function_defs;

    (* Print main function *)
    print_endline "define i32 @main() #0 {";
    (* Print the blocks *)
    print_blocks (blocks@[(label,instructions)]);
    (* Print the epilogue *)
    List.iter print_endline epilogue

let compile e =
  function_defs := [];
  compile_llvm Env.empty_env e 0 []