open Syntax 

type 'a maybe =
  Nothing
| Just of 'a

type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
  | DProcV of id * exp
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"
  | DProcV _ -> "<fun>"

let pp_val v = print_string (string_of_exval v)

let print_type_val id v =
  Printf.printf "val %s = " id;
  pp_val v;
  print_newline();
  ()

let rec print_type_vals id v =
  match (id, v) with
  | ([], _) -> ()
  | (_, []) -> ()
  | ((idh::idt), (vh::vt))
    -> print_type_val idh vh;
      print_type_vals idt vt

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Minus, _, _ -> err ("Both arguments must be integer: -")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Div, IntV i1, IntV i2 -> IntV (i1 / i2)
  | Div, _, _ -> err ("Both arguments must be integer: /")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | LAnd, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | LAnd, _, _ -> err ("Both arguments must be bool: &&")
  | LOr, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | LOr, _, _ -> err ("Both arguments must be bool: ||")

let rec env_extend env =
  function
  | [] -> env
  | (id, v)::xs -> (Environment.extend id v (env_extend env xs))

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (Declare (id, exp1), exp2) ->
    let value = eval_exp env exp1 in
    eval_exp (Environment.extend id value env) exp2
  | LetandExp (andexp, exp) -> 
    let rec andList env = function
      | AndExp (Declare (ident, e), nextExp)
	  -> (ident, (eval_exp env e)) :: (andList env nextExp)
      | AndEnd -> []
      | _ -> raise (Error "error in let and")
    in eval_exp (env_extend env (andList env andexp)) exp
  | AndExp _ | AndEnd -> raise (Error "error in and")
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  | DFunExp (id, exp) -> DProcV (id, exp)
  | AppExp (exp1, exp2) ->
    let funval = eval_exp env exp1 in
    let arg = eval_exp env exp2 in
    (match funval with
    | DProcV (id, body)
      -> let newenv = Environment.extend id arg env
	 in eval_exp newenv body
    | ProcV (id, body, env') ->
      let newenv = Environment.extend id arg !env' in
      eval_exp newenv body
    | _ -> err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) ->
    let dummyenv = ref Environment.empty in
    let newenv =
      Environment.extend id (ProcV (para, exp1, dummyenv)) env in
    dummyenv := newenv;
    eval_exp newenv exp2
  | _ -> err "err in pattern match in eval_exp"



let rec unzip = function
  | [] -> ([],[])
  | (a, b) :: xs -> 
    let (at, bt) = unzip xs
    in (a::at, b::bt)

let rec unzip3 = function
  | [] -> ([],[],[])
  | (a, b, c) :: xs -> 
    let (at, bt, ct) = unzip3 xs
    in (a::at, b::bt, c::ct)

let rec zip a =
  function [] -> []
    | x::xs -> match a with
	| [] -> []
	| hd::tl -> (hd, x) :: (zip tl xs);;


let rec eval_decl env = function
    Exp e -> let v = eval_exp env e in (["-"], env, [v])
  | Decl (id, e) ->
    let v = eval_exp env e in ([id], Environment.extend id v env, [v])
  | (ManyDecl _) as md ->
    let rec evlAndDecl env = function
      | AndDecl (Decl (i, e), decl2) -> 
	(i, eval_exp env e, Nothing) :: (evlAndDecl env decl2)
      | AndDecl (RecDecl (id, FunExp (fid, e)), decl2)
	  -> let dummyenv = ref Environment.empty
	     in let proc = (ProcV (fid, e, dummyenv))
		in let newenv = Environment.extend id proc env
		   in (id, proc, Just dummyenv) :: (evlAndDecl env decl2)
(*
	(match eval_decl env decl1 with
	  | (id::_, _, e::_) -> (id, e) :: (evlAndDecl env decl2)
	  | _ -> raise (Error "error in rec decl"))
*)
      | NoneDecl -> []
      | _ -> raise (Error "Error in \"let and\" declare")
    in let rec evlManyDecl ids vs env = function
      | ManyDecl (decl1, decl2) ->
	let addingList = evlAndDecl env decl1
	in let (id, v, prcList) = unzip3 addingList
	   in let newenv = env_extend env (zip id v)
	      in let rec dummyWrite = function [] -> ()
	                                     | Nothing::xs -> dummyWrite xs
					     | (Just d)::xs -> d := newenv; dummyWrite xs
		 in dummyWrite prcList;
		 evlManyDecl (ids@id) (vs@v) newenv decl2
      | NoneDecl -> (ids, env, vs)
      | _ -> raise (Error "Error in \"let and\" declare")
       in evlManyDecl [] [] env md
  | RecDecl (id, FunExp (fid, e))
      -> let dummyenv = ref Environment.empty
	 in let proc = (ProcV (fid, e, dummyenv))
	    in let newenv = Environment.extend id proc env
	       in dummyenv := newenv;
	       ([id], newenv, [proc])
  | NoneDecl -> ([], env, [])
  | _ -> raise (Error "erro in pattern match in eval decl")
(*
  | (ManyDecl (_, _)) as md ->
    let rec evlDcl ids vs env = function
      | ManyDecl (decl1, decl2) ->
	let (id, newenv, v) = eval_decl env decl1
	in evlDcl (ids@id) (vs@v) newenv decl2
      | NoneDecl -> (ids, env, vs)
      | _ -> raise (Error "error in many declare")
    in evlDcl [] [] env md
*)
(*  | ManyDecl (decl1, decl2) ->
    let (id, newenv, v) = eval_decl env decl1
    in eval_decl newenv decl2*)

