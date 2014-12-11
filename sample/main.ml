open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try (let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
       let (id, newenv, v) = eval_decl env decl in
       print_type_vals id v;
       read_eval_print newenv) with
  | Error s -> print_endline s; read_eval_print env
  | Failure s -> print_endline s; read_eval_print env
(*  | _ -> print_endline "a error occurred"; read_eval_print env*)

let initial_env = 
  Environment.extend "ii" (IntV 2)
    (Environment.extend "iii" (IntV 3) 
       (Environment.extend "iv" (IntV 4) Environment.empty))

let _ = read_eval_print initial_env
