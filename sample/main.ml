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


let env_add str env =
  let decl = Parser.toplevel Lexer.main (Lexing.from_string str)
  in let (_, newenv, _) = eval_decl env decl
     in newenv

let rec env_add_list l env =
  match l with
  | [] -> env
  | h::t -> env_add_list t (env_add h env)

let initial_env =
(*  Environment.empty*)
  env_add_list
    ["let (+) = fun x -> fun y -> x + y;;";
     "let (*) = fun x -> fun y -> x * y;;"]
    Environment.empty
(*
  Environment.extend "ii" (IntV 2)
    (Environment.extend "iii" (IntV 3) 
       (Environment.extend "iv" (IntV 4) Environment.empty))
*)

let _ = read_eval_print initial_env
