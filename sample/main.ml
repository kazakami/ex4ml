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

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try (let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
       let ty = ty_decl tyenv decl in
       let (id, newenv, v) = eval_decl env decl in
       print_type_vals id v ty;
       read_eval_print newenv tyenv) with
  | Error s -> print_endline s; read_eval_print env tyenv
  | Failure s -> print_endline s; read_eval_print env tyenv
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
  env_add_list
    ["let (+) = fun x -> fun y -> x + y;;";
     "let (*) = fun x -> fun y -> x * y;;";
     "let i = 1;;";
     "let v = 5;;"]
    Environment.empty

let initial_tyenv =
  List.fold_right (fun (id, ty) env -> (Environment.extend id ty env)) 
    [("i", TyInt); ("v", TyInt)]
    Environment.empty

let _ =
  let alpha = fresh_tyvar () in
  let beta = fresh_tyvar () in
  print_string (string_of_ty (subst_type [(beta, (TyFun (TyVar alpha, TyInt))); (alpha, TyBool)] (TyVar beta)))


(*let _ = read_eval_print initial_env initial_tyenv*)
