(* This is the main entry point for the calc language interpreter *)

open Calc_lib

let parse_lexbuf lb =
  try Parser.main Lexer.read lb with
  | Parsing.Parse_error ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum col)

let parse_string s =
  Lexing.from_string s |> parse_lexbuf

let () =
  print_endline "Insert an expression. Ctrl+D to exit.";
  let rec loop () =
    print_string "> "; flush stdout;
    match read_line () with
    | s ->
        (try
           let e = parse_string s in 
           print_string (Ast.unparse_ast 100 e^" "); flush stdout;
           let e' = Typing.typecheck e in
           let t = Typing.type_of e' in
           begin match t with 
           | None m -> failwith ("Typing error: " ^ m)
           | _ -> let v = Eval.eval e in
                  print_endline (" = "^(Eval.unparse_result v^ " : "^ Typing.unparse_type t))
           end
         with Failure msg ->
           Printf.eprintf "Error: %s\n%!" msg);
        loop ()
    | exception End_of_file -> print_endline "\nGoodbye!"
  in
  loop ()
 
