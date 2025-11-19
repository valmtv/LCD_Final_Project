(* This is the main entry point for the calc language compiler *)

open Calc_lib

let parse_lexbuf lb =
  try Parser.main Lexer.read lb with
  | Parsing.Parse_error ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum col)

let parse_channel chan =
  Lexing.from_channel chan |> parse_lexbuf

let () =
  try
    (* Read the entire input from stdin *)
    let e = parse_channel stdin in
    let e_opt = Optimizer.optimize e in
    let e' = Typing.typecheck e_opt in
    let t = Typing.type_of e' in
    begin match t with
     | None m ->
         Printf.eprintf "Typing error: %s\n" m;
         exit 1
     | _ ->
         (* Call the compiler and receive the instructions *)
         let result = Llvm.compile e' in
         (* Print the resulting LLVM program to stdout *)
         Llvm.print_llvm result t
     end
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "System error: %s\n" msg;
      exit 1