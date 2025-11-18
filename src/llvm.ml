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

let count = ref 0
let new_reg = fun () -> count := !count + 1; !count
let new_label = new_reg

(* Code generation function *)

let rec compile_llvm env e label block =
  match e with
  | Num x -> Const x, env, label, block, []
  | Bool b when b = true -> Const 1, env, label, block, []
  | Bool b when b = false -> Const 0, env, label, block, []
  | Bool _ -> failwith "Invalid boolean value"
  | Id x ->
      (match Env.lookup env x with
       | Some r -> r, env, label, block, []
       | None -> failwith ("Unbound variable in codegen: " ^ x))

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

  | Let (_,bindings,body) ->
    let env' = Env.begin_scope env in
    let env'', l', b', bs' = List.fold_left (fun (acc_env, acc_label, acc_block, acc_blocks) (id, expr) ->
      let r, new_env, new_label, new_block, new_blocks = compile_llvm acc_env expr acc_label acc_block in
      let bound_env = Env.bind new_env id r in
      (bound_env, new_label, new_block, acc_blocks @ new_blocks)
    ) (env', label, block, []) bindings in
    compile_llvm env'' body l' b' bs'

(* Unparse LLVM functions *)

let prologue =
  ["@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1";
   "define i32 @main() #0 {"]

let epilogue =
   ["  ret i32 0";
    "}";
    "declare i32 @printf(ptr noundef, ...) #1"]

let unparse_register n = "%"^string_of_int n

let unparse_label_use n = "%"^string_of_int n

let unparse_label_declaration l = (string_of_int l)^":"

let unparse_result = function
  | Const x -> string_of_int x
  | Register x -> unparse_register x

let unparse_type = function
  | IntT -> "i32"
  | BoolT -> "i1"
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

let print_block (label, instructions) =
    print_endline (unparse_label_declaration label);
    List.iter (fun x -> x |> unparse_llvm_i |> print_endline) instructions

let print_blocks bs = List.iter print_block bs

let emit_printf ret t =
  let llvm_type = unparse_type t
  in
    "  "^unparse_register (new_reg())^" = call i32 (ptr, ...) @printf(ptr noundef @.str, "^llvm_type^" noundef "^unparse_result ret^")"

let print_llvm (ret,_env,label,instructions,blocks) t =
    (* Print the prologue *)
    List.iter print_endline prologue;
    (* Print the blocks *)
    print_blocks (blocks@[(label,instructions)]);
    (* Print the instruction printing the result *)
    print_endline (emit_printf ret t);
    (* Print the epilogue *)
    List.iter print_endline epilogue

let compile e = compile_llvm Env.empty_env e 0 []