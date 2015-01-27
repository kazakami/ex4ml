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
  | ((idh::idt), (vh::vt), ((_, th)::tt))
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
       let ty = ty_decl tyenv decl in
       let (id, newenv, v) = eval_decl env decl in
       print_type_vals id v ty;
       read_eval_print newenv tyenv) with
  | Error s -> print_endline s; read_eval_print env tyenv
  | Failure s -> print_endline s; read_eval_print env tyenv
  | e -> raise e
  | _ -> print_endline "a error occurred"; read_eval_print env tyenv 


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
     "let ii = 2;;";
     "let iii = 3;;";
     "let iv = 4;;";
     "let v = 5;;"]
    Environment.empty

let initial_tyenv =
  List.fold_right (fun (id, ty) env -> (Environment.extend id ty env)) 
    [("i", TyInt); ("v", TyInt)]
    Environment.empty

(*
let _ =
  let alpha = fresh_tyvar () in
  let beta = fresh_tyvar () in
  print_string (string_of_ty (subst_type [(beta, (TyFun (TyVar alpha, TyInt))); (alpha, TyBool)] (TyVar beta)))
 *)
(*
let _ =
  let a = fresh_tyvar () in
  let b = fresh_tyvar () in
  let c = fresh_tyvar () in
  let d = fresh_tyvar () in
  List.map (fun i -> print_string (Char.escaped (char_of_int (i+97))))
	   (MySet.to_list (freevar_ty (subst_type [(b, (TyFun ((TyVar a),(TyVar d))))]
						  (TyFun ((TyVar b), (TyVar c))))));
  if MySet.member a (freevar_ty (subst_type [(b, (TyFun ((TyVar a),(TyVar d))))]
					    (TyFun ((TyVar b), (TyVar c)))))
  then print_string "y"
  else print_string "n"
 *)
    

let _ = read_eval_print initial_env initial_tyenv

