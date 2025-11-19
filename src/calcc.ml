(* This is the main entry point for the calc language compiler *)

open Calc_lib

let optimize_enabled = ref false
let input_file = ref ""

let spec = [
  ("-O", Arg.Set optimize_enabled, "Enable optimizations (Constant Folding & Propagation)")
]

let usage_msg = "Usage: calcc [-O] < file"

let parse_lexbuf lb =
  try Parser.main Lexer.read lb with
  | Parsing.Parse_error ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum col)

let parse_channel chan =
  Lexing.from_channel chan |> parse_lexbuf

let () =
  Arg.parse spec (fun _ -> ()) usage_msg;

  try
    (* Read the entire input from stdin *)
    let e = parse_channel stdin in
    
    let e_processed = 
      if !optimize_enabled then 
        Optimizer.optimize [] e
      else 
        e 
    in
    
    let e_typed = Typing.typecheck e_processed in
    let t = Typing.type_of e_typed in
  
    begin match t with
     | None m ->
         Printf.eprintf "Typing error: %s\n" m;
         exit 1
     | _ ->
         (* Print the resulting LLVM program to stdout *)
         let result = Llvm.compile e_typed in
         Llvm.print_llvm result t
     end
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "System error: %s\n" msg;
      exit 1