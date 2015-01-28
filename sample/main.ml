open Syntax
open Eval
open Typing


let print_type_val id v t =
  Printf.printf "val %s : " id;
  pp_ty t;
  print_string " = ";
  pp_val v;
  print_newline();
  ()

let rec print_type_vals id v t =
  match (id, v, t) with
  | ([], _, _) -> ()
  | (_, [], _) -> ()
  | (_, _, []) -> ()
  | ((idh::idt), (vh::vt), (th::tt))
    -> print_type_val idh vh th;
      print_type_vals idt vt tt

let print_val id v =
  Printf.printf "val %s : " id;
  print_string " = ";
  pp_val v;
  print_newline();
  ()

let rec print_vals id v =
  match (id, v) with
  | ([], _) -> ()
  | (_, []) -> ()
  | ((idh::idt), (vh::vt))
    -> print_val idh vh;
      print_vals idt vt

(*
(* 型推論無い方 *)
let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try (let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
       let (id, newenv, v) = eval_decl env decl in
       print_vals id v;
       read_eval_print newenv) with
  | Error s -> print_endline s; read_eval_print env
  | Failure s -> print_endline s; read_eval_print env
  | _ -> print_endline "a error occurred"; read_eval_print env
 *)
(* 型推論有る方 *)
let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try (let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
       let (newtyenv, ty) = ty_decl tyenv decl in
       let (id, newenv, v) = eval_decl env decl in
       print_type_vals id v ty;
       read_eval_print newenv newtyenv) with
  | Error s -> print_endline s; read_eval_print env tyenv
  | Failure s -> print_endline s; read_eval_print env tyenv
  | Parsing.Parse_error -> print_endline "parse error"; read_eval_print env tyenv
  | _ -> print_endline "a error occurred"; read_eval_print env tyenv 


let env_add str env =
  let decl = Parser.toplevel Lexer.main (Lexing.from_string str)
  in let (_, newenv, _) = eval_decl env decl
     in newenv

let rec env_add_list l env =
  match l with
  | [] -> env
  | h::t -> env_add_list t (env_add h env)

let tyenv_add str tyenv =
  let decl = Parser.toplevel Lexer.main (Lexing.from_string str)
  in let (newtyenv, _) = ty_decl tyenv decl
     in newtyenv

let rec tyenv_add_list l tyenv =
  match l with
  | [] -> tyenv
  | h::t -> tyenv_add_list t (tyenv_add h tyenv)


let initialiser =
  ["let (+) = fun x -> fun y -> x + y;;";
   "let (*) = fun x -> fun y -> x * y;;";
   "let i = 1;;";
   "let ii = 2;;";
   "let iii = 3;;";
   "let iv = 4;;";
   "let v = 5;;"]
			 
let initial_env =
  env_add_list
    initialiser
    Environment.empty

let initial_tyenv =
  tyenv_add_list
    initialiser
    Environment.empty


let _ = read_eval_print initial_env initial_tyenv

