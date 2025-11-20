open Typing

type register = int

type label = string

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
  | GetElementPtr of register * Typing.calc_type * result * result list
  | Load of register * Typing.calc_type * result
  | Store of Typing.calc_type * result * result
  | PtrToInt of register * result
  | Call of register * string * (Typing.calc_type * result) list
  | CallVoid of string * (Typing.calc_type * result) list
  | Bitcast of register * string * result * string
  | ZExt of register * Typing.calc_type * result * Typing.calc_type

let reg_count = ref 0
let label_count = ref 0

let new_reg = fun () -> reg_count := !reg_count + 1; !reg_count
let new_label = fun () -> label_count := !label_count + 1; "L" ^ string_of_int !label_count

(* Function counter for generating unique function names *)
let fun_count = ref 0
let new_fun_id () = fun_count := !fun_count + 1; !fun_count

(* Store generated function definitions *)
type func_def = {
  name: string;
  param_type: calc_type;
  return_type: calc_type;
  body_result: result;
  body_label: label;
  body_block: llvm list;
  body_blocks: (label * llvm list) list;
}

let function_defs = ref []

(* Helper to check if a type is represented as a pointer *)
let is_ptr_type = function
  | FunT _ | TupleT _ | RefT _ -> true
  | _ -> false

(* Helper to determine apply_closure function name based on types *)
let get_apply_closure_name param_t ret_t =
  match param_t, ret_t with
  | IntT, IntT -> "apply_closure_i32_i32"
  | IntT, BoolT -> "apply_closure_i32_i1"
  | BoolT, IntT -> "apply_closure_i1_i32"
  | BoolT, BoolT -> "apply_closure_i1_i1"
  (* Use generic pointer implementation for any pointer types (Functions, Tuples, Refs) *)
  | t1, t2 when is_ptr_type t1 && is_ptr_type t2 -> "apply_closure_closure_closure"
  | _ -> "apply_closure_i32_i32" (* default fallback *)

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
  | Eq (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpEq (type_of e1,ret,r1,r2)], bs1@bs2)

  (* Inequality comparison - works for both int and bool *)
  | Neq (_,e1,e2) ->
    let r1,env1,l1,b1,bs1 = compile_llvm env e1 label block in
    let r2,env2,l2,b2,bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in
    (Register ret, env2, l2, b2@[CmpNeq (type_of e1,ret,r1,r2)], bs1@bs2)

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
    
    (* Zero-extend the i1 boolean to an i32 integer *)
    let ret_reg = new_reg () in
    let ext_instr = ZExt(ret_reg, BoolT, r1, IntT) in
    
    (* Pass the extended register (ret_reg) instead of r1 *)
    let call_instr = CallVoid ("print_bool", [(IntT, Register ret_reg)]) in
    (Const 0, env1, l1, b1@[ext_instr; call_instr], bs1)
  | PrintEndLine _ ->
    let call_instr = CallVoid ("print_endline", []) in
    (Const 0, env, label, block@[call_instr], [])

  (* Function operations *)
  | Fun (t, param, param_type, body) ->
    let fun_id = new_fun_id () in
    let fun_name = "lambda_" ^ string_of_int fun_id in

    (* Compile the function body - it will be a separate LLVM function *)
    let return_type = match t with
      | FunT (_, ret_t) -> ret_t
      | _ -> failwith "Internal error: Fun must have FunT type"
    in

    (* Compile function body with fresh counter *)
    (* We'll fix the numbering when we print the function *)
    let param_reg = Register 1 in (* %1 in the function *)
    let body_env = Env.begin_scope Env.empty_env in
    let body_env' = Env.bind body_env param param_reg in

  (* Use a temporary counter for function body *)
    let saved_reg_count = !reg_count in
    let saved_label_count = !label_count in
    reg_count := 1; (* Start at 1 for function body registers, next will be %2 *)
    label_count := 0; (* Start at 0 for function body labels *)
    let saved_fun_count = !fun_count in
    fun_count := !fun_count - 1; (* Restore fun_count since we incremented it *)

    let body_result, _body_env_final, body_label, body_block, body_blocks =
      compile_llvm body_env' body "entry" [] in

    (* Restore counter for main function *)
    fun_count := saved_fun_count;
    reg_count := saved_reg_count;
    label_count := saved_label_count;

    function_defs := {
      name = fun_name;
      param_type = param_type;
      return_type = return_type;
      body_result = body_result;
      body_label = body_label;
      body_block = body_block;
      body_blocks = body_blocks;
    } :: !function_defs;

    (* Create a closure - pass function pointer and null environment *)
    let func_bitcast = new_reg() in
    let ret = new_reg() in
    let call_instr = [
      Bitcast (func_bitcast, "ptr", Const 0, "@" ^ fun_name);
      Call (ret, "create_closure", [(FunT (param_type, return_type), Register func_bitcast); (RefT UnitT, Const (-1))]) (* Use RefT UnitT for ptr type *)
    ] in
    (Register ret, env, label, block@call_instr, [])

  | App (ret_t, e1, e2) ->
    let r1, env1, l1, b1, bs1 = compile_llvm env e1 label block in
    let r2, env2, l2, b2, bs2 = compile_llvm env1 e2 l1 b1 in
    let ret = new_reg() in

    (* Determine the correct apply_closure function based on types *)
    let func_name = match type_of e1 with
      | FunT (param_t, return_t) -> get_apply_closure_name param_t return_t
      | _ -> "apply_closure_i32_i32" (* fallback *)
    in

    let param_t = match type_of e1 with
      | FunT (pt, _) -> pt
      | _ -> IntT
    in

    let call_instr = Call (ret, func_name, [(FunT (param_t, ret_t), r1); (param_t, r2)]) in
    (Register ret, env2, l2, b2@[call_instr], bs1@bs2)


  | Tuple (ann, es) ->
     let struct_type = type_of (Tuple(ann, es)) in 
     
     (* Calculate size & Malloc - allocate these registers FIRST *)
     let size_ptr_reg = new_reg () in
     let size_reg = new_reg () in
     let mem_reg = new_reg () in
     
     (* Define instructions for size & malloc *)
     (* Use Const (-1) for null to calculate size *)
     let init_instrs = [
        GetElementPtr(size_ptr_reg, struct_type, Const (-1), [Const 1]);
        PtrToInt(size_reg, Register size_ptr_reg);
        Call(mem_reg, "malloc", [(IntT, Register size_reg)])
     ] in
     
     (* Compile & Store elements *)
     (* Start the fold with 'block @ init_instrs'.
         ensures registers were allocated:
        1. Malloc (regs %2, %3, %4)
        2. Element 1 (regs %5...)
        3. Element 2 (regs %6...) 
     *)
     let (final_env, final_label, final_block, final_bs) = 
       List.fold_left (fun (env, l, b, bs) (expr, idx) ->
         (* Compile expr. 'b' already contains history + init_instrs + previous exprs *)
         let r_val, env', l', b', bs' = compile_llvm env expr l b in
         
         (* gep/store registers *)
         let gep_reg = new_reg () in
         let gep_instr = GetElementPtr(gep_reg, struct_type, Register mem_reg, [Const 0; Const idx]) in
         let store_instr = Store(Typing.type_of expr, r_val, Register gep_reg) in
         
         (* Add to block *)
         (env', l', b' @ [gep_instr; store_instr], bs @ bs')
       ) (env, label, block @ init_instrs, []) (List.mapi (fun i e -> (e,i)) es) 
     in
     
     (Register mem_reg, final_env, final_label, final_block, final_bs)

  | TupleAccess (ann, e, idx) ->
      let r_tuple, env1, l1, b1, bs1 = compile_llvm env e label block in
      let tuple_type = Typing.type_of e in (* This is the TupleT {...} *)
      
      let gep_reg = new_reg () in
      let gep_instr = GetElementPtr(gep_reg, tuple_type, r_tuple, [Const 0; Const (idx - 1)]) in
      
      let ret_reg = new_reg () in
      let load_instr = Load(ret_reg, ann, Register gep_reg) in
      
      (Register ret_reg, env1, l1, b1 @ [gep_instr; load_instr], bs1)

(* Unparse LLVM functions *)

let prologue =
  [ "declare ptr @malloc(i32)";
   "declare ptr @new_ref_int(i32)";
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
   "declare void @print_bool(i32)";
   "declare void @print_endline()";
   "declare ptr @create_closure(ptr, ptr)";
   "declare i32 @apply_closure_i32_i32(ptr, i32)";
   "declare i32 @apply_closure_i32_i1(ptr, i32)";
   "declare i32 @apply_closure_i1_i32(ptr, i32)";
   "declare i32 @apply_closure_i1_i1(ptr, i32)";
   "declare ptr @apply_closure_closure_closure(ptr, ptr)";
   ""]

let epilogue =
   ["  ret i32 0";
    "}"]

let unparse_register n = "%"^string_of_int n

let unparse_label_use l = "%"^l

let unparse_label_declaration l = l^":"

let unparse_result = function
  | Const (-1) -> "null"  (* Special case for null pointers *)
  | Const x -> string_of_int x
  | Register x -> unparse_register x

(* Helper to generate the structural type string (e.g. "{i32, i32}") *)
let rec unparse_structural_type = function
  | IntT -> "i32"
  | BoolT -> "i1"
  | UnitT -> "i32"
  | RefT _ -> "ptr"
  | FunT _ -> "ptr"
  | TupleT ts -> "{" ^ String.concat ", " (List.map unparse_type ts) ^ "}"
  | _ -> failwith "Unknown type"

(* Main function for register types - Tuples are pointers in registers *)
and unparse_type = function
  | TupleT _ -> "ptr"
  | t -> unparse_structural_type t

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
      (* Determine return type based on function name *)
      let ret_type =
        if String.sub fname 0 (min 13 (String.length fname)) = "apply_closure" &&
           not (fname = "apply_closure_closure_closure") then
          "i32"  (* Most apply_closure functions return i32 *)
        else if fname = "apply_closure_closure_closure" then
          "ptr"  (* closure -> closure returns ptr *)
        else if fname = "create_closure" then
          "ptr"
        else if List.mem fname ["new_ref_int"; "new_ref_bool"; "new_ref_ref"] then
          "ptr"
        else if fname = "deref_int" then
          "i32"
        else if fname = "deref_bool" then
          "i32"
        else if List.mem fname ["deref_ref"] then
          "ptr"
        else
          "ptr"  (* default to ptr *)
      in
      "  "^unparse_register r^" = call "^ret_type^" @"^fname^"("^String.concat ", " arg_strs^")"
  | CallVoid (fname, args) ->
      let arg_strs = List.map (fun (t, v) -> unparse_type t ^ " " ^ unparse_result v) args in
      "  call void @"^fname^"("^String.concat ", " arg_strs^")"
  | Bitcast (r, target_type, _src, fname) ->
      "  "^unparse_register r^" = bitcast ptr "^fname^" to "^target_type
  | GetElementPtr (r, t, base, indices) ->
      let type_str = unparse_structural_type t in
      let indices_str = String.concat ", " (List.map (fun res -> "i32 " ^ unparse_result res) indices) in
      "  " ^ unparse_register r ^ " = getelementptr " ^ type_str ^ ", ptr " ^ unparse_result base ^ ", " ^ indices_str
  | Load (r, t, ptr) ->
      "  " ^ unparse_register r ^ " = load " ^ unparse_type t ^ ", ptr " ^ unparse_result ptr
  | Store (t, v, ptr) ->
      "  store " ^ unparse_type t ^ " " ^ unparse_result v ^ ", ptr " ^ unparse_result ptr
  | PtrToInt (r, v) ->
      "  " ^ unparse_register r ^ " = ptrtoint ptr " ^ unparse_result v ^ " to i32"
  | ZExt (r, src_t, v, dst_t) ->
      "  " ^ unparse_register r ^ " = zext " ^ unparse_type src_t ^ " " ^ unparse_result v ^ " to " ^ unparse_type dst_t

let print_block (label, instructions) =
    print_endline (unparse_label_declaration label);
    List.iter (fun x -> x |> unparse_llvm_i |> print_endline) instructions

let print_blocks bs = List.iter print_block bs

let print_function_def func_def =
  let param_type_str = unparse_type func_def.param_type in
  let return_type_str = unparse_type func_def.return_type in

  print_endline ("\ndefine " ^ return_type_str ^ " @" ^ func_def.name ^ "(ptr %0, " ^ param_type_str ^ " %1) {");
  print_blocks (func_def.body_blocks @ [(func_def.body_label, func_def.body_block)]);
  print_endline ("  ret " ^ return_type_str ^ " " ^ unparse_result func_def.body_result);
  print_endline "}"

let print_llvm (_ret,_env,label,instructions,blocks) _t =
    (* Print the prologue *)
    List.iter print_endline prologue;

    (* Print function definitions *)
    List.iter print_function_def (List.rev !function_defs);

    (* Print main function *)
    print_endline "define i32 @main() #0 {";
    (* Print the blocks *)
    print_blocks (blocks@[(label,instructions)]);
    (* Print the epilogue *)
    List.iter print_endline epilogue

let compile e =
  function_defs := [];
  reg_count := -1;
  label_count := 0;
  fun_count := 0;
  compile_llvm Env.empty_env e "L0" []