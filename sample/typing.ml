open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let rec unzip = function
  | [] -> ([],[])
  | (a, b) :: xs -> 
    let (at, bt) = unzip xs
    in (a::at, b::bt)

			  
let rec string_of_subst l =
  match l with
    [] -> ""
  | (id, ty)::xs -> "[" ^ string_of_int id ^ ":" ^ string_of_ty ty ^ "]" ^ string_of_subst xs
			  
let rec subst_type l = function
  | (TyVar i) as t ->
    (match l with
      [] -> t
    | (id, y)::xs
      -> if id = i
	then subst_type xs y
	else subst_type xs t)
  | TyFun (t1, t2) -> TyFun ((subst_type l t1), (subst_type l t2))
  | x -> x

let eqs_of_subst s =
  List.map (fun (tv, t) -> (TyVar tv, t)) s

let rec substitute ((a, t) as sbst) = function
    TyVar b -> if a = b
	       then t
	       else TyVar b
  | TyList typ -> TyList (substitute sbst typ)
  | TyFun (t1, t2) -> TyFun ((substitute sbst t1),
			     (substitute sbst t2))
  | typ -> typ
	   
let rec subst_eqs s eqs =
  let rec sbst s = function
      [] -> []
    | (t1, t2) :: xs -> (substitute s t1, substitute s t2) :: (sbst s xs) in
  match s with
    [] -> eqs
  | x :: xs -> subst_eqs xs (sbst x eqs)
	   
let ty_prim op ty1 ty2 =
  match op with
    Plus | Minus | Mult | Div -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | LAnd | LOr -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | _ -> err "Not Implemented!"

let rec string_of_eqs =
  function
    [] -> ""
  | (TyInt, TyInt) :: xs -> "[int, int]" ^ string_of_eqs xs
  | (TyBool, TyBool) :: xs -> "[bool, bool]" ^ string_of_eqs xs
  | (TyVar a, t) :: xs -> "[" ^ (Char.escaped (char_of_int (a+97))) ^ ", " ^ string_of_ty t ^ "]" ^ string_of_eqs xs
  | (t, TyVar a) :: xs -> string_of_eqs ((TyVar a, t) :: xs)
  | (t1, t2) :: xs -> "[" ^ string_of_ty t1 ^ ", " ^ string_of_ty t2 ^ "]" ^ string_of_eqs xs
  | _ -> err "err in eqs"

let print_eqs_line eqs = print_string (string_of_eqs eqs ^ "\n")
	     
let rec unify =
  function
    [] -> []
  | (TyInt, TyInt) :: xs -> unify xs
  | (TyBool, TyBool) :: xs -> unify xs
  | (TyList t1, TyList t2) :: xs
    -> if string_of_ty t1 = string_of_ty t2
       then unify xs
       else err "err in unify : unsame list"
  | (TyVar a, TyVar b) :: xs -> print_int a;print_int b;if a = b
				then unify xs
				else (a, TyVar b) :: (unify (subst_eqs [(a, TyVar b)] xs))
  | (TyVar a, t) :: xs -> print_string ("{" ^ (Char.escaped (char_of_int (a+97)))); print_string (string_of_ty t);print_string "}";
			  if MySet.member a (freevar_ty t)
			  then err ("err in unify: " ^ (Char.escaped (char_of_int (a+97))) ^ " is in " ^ string_of_ty t)
			  else (print_string "\n**";print_eqs_line (subst_eqs [(a, t)] xs);
				(a, t) :: (unify (subst_eqs [(a, t)] xs)))
  | (t, TyVar a) :: xs -> unify ((TyVar a, t) :: xs)
  | (((TyFun (t11, t12)) as t1), ((TyFun (t21, t22)) as t2)) :: xs
    -> if t1 = t2
       then unify xs
       else (print_string (string_of_ty t12);print_string (string_of_ty t22);print_string "\n";
	     unify ((t12, t22) :: (t11, t21) :: xs))
  | _ -> err "err in unify not : not match"

	     
let rec ty_exp tyenv = function
    Var x ->
      (try ([], Environment.lookup x tyenv) with
	Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
     (match op with
      | Cons ->
	 ty_exp tyenv (ListLit (exp1, exp2))
       |_ ->
	 let (s1, ty1) = ty_exp tyenv exp1 in
	 let (s2, ty2) = ty_exp tyenv exp2 in
	 let (eqs3, ty) = ty_prim op ty1 ty2 in
	 let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
	 let s3 = (*print_string (string_of_eqs eqs);*)unify eqs
	 in (*print_string (string_of_subst s3);*)(s3, subst_type s3 ty))
  | IfExp (exp1, exp2, exp3) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let (s3, ty3) = ty_exp tyenv exp3 in
     let eqs = (eqs_of_subst s3) @ (eqs_of_subst s2) @ (eqs_of_subst s1) @ [(ty1, TyBool); (ty2, ty3)] in
     let s4 = print_eqs_line eqs;unify eqs
     in (s4, subst_type s4 ty2)
  | MatchExp (p, c) ->
     let rec eqsList_of_subsetList = function
	 [] -> []
       | s :: ss -> eqs_of_subst s @ eqsList_of_subsetList ss in
     let rec extend tyenv ids ts =
       match (ids, ts) with
	 ([], []) -> tyenv
       | ([], _) -> err "err in match"
       | (_, []) -> err "err in match"
       | (i::is, t::ts) -> (Environment.extend i t (extend tyenv is ts))
     in let rec trial = function
	  | MatchCondAndExp (cond, exp, nextCond) ->
	     let rec subtrial pat = function
	       (* パターンと条件式を受け取り、
	          マッチしたかどうかと環境に追加すべき識別子のリストと
                  その式の型と型代入のタプルのリストのタプルを返す。 *)
	       | ILit i -> ((match pat with ILit i_ -> i = i_ | _ -> false), [], [])
	       | BLit b -> ((match pat with BLit b_ -> b = b_ | _ -> false), [], [])
	       | EmpList -> print_string "@";((match pat with EmpList -> true | _ -> false), [], [])
	       | Underscore -> (true, [], [])
	       | Var id -> (true, [id], [ty_exp tyenv pat])
	       | ListLit (head, tail) ->
		  (match pat with
		     ListLit (patH, patT) ->
		     let (resultH, idsH, esH) = subtrial patH head
		     and (resultT, idsT, esT) = subtrial patT tail
		     in if resultH && resultT
			then (true, idsH@idsT, esH@esT)
			else (false, [], [])
		   | _ -> (false, [], [])
		  )
	       | _ -> (false, [], [])
	     in let (result, ids, tys) = subtrial p cond
		in if result
		   then (ids, tys, exp)
		   else trial nextCond 
	  | MatchCondEnd -> err "Pattern was not match"
	  | _ -> err ("Type Pattern Match failure")
	in let (ids, s_and_tys, exp) = trial c in
	   let (ss, tys) = unzip s_and_tys in
	   let new_env = extend tyenv ids tys in
	   let (se, tye) = ty_exp new_env exp in
	   let eqs = eqsList_of_subsetList ss @ eqs_of_subst se in
	   let s = unify eqs
	   in (s, subst_type s tye)
  | LetExp (Declare (id, exp1), exp2) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
     let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
     let s3 = unify eqs
     in (s3, subst_type s3 ty2)
  | LetRecExp (id, FunExp(fid, fexp), exp) ->
     let new_ty1 = TyVar (fresh_tyvar ()) in
     let new_ty2 = TyVar (fresh_tyvar ()) in
     let (sf, tyf) = ty_exp (Environment.extend fid new_ty1
					       (Environment.extend id (TyFun (new_ty1, new_ty2)) tyenv)) fexp in
     let (se, tye) = ty_exp (Environment.extend id (TyFun (new_ty1, new_ty2)) tyenv) exp in
     let eqs = (eqs_of_subst se) @ (eqs_of_subst sf) @ [(tyf, new_ty2)] in
     let s = print_eqs_line eqs;unify eqs
     in (s, subst_type s tye)
  | FunExp (id, exp) ->
     let new_ty = TyVar (fresh_tyvar ()) in
     let (se, tye) = print_string id;print_string (string_of_ty new_ty);
		     ty_exp (Environment.extend id new_ty tyenv) exp in
     let eqs = eqs_of_subst se in
     let s = print_eqs_line eqs;unify eqs
     in (s, TyFun (subst_type s new_ty, subst_type s tye))
  | AppExp (exp1, exp2) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2
     in (match ty1 with
	   TyFun (tyf1, tyf2) -> let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(tyf1, ty2)] in
				 let s3 = print_eqs_line eqs;unify eqs
				 in (s3, subst_type s3 tyf2)
	 | TyVar ty -> let new_ty = TyVar (fresh_tyvar ()) in
		       let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(TyVar ty, TyFun (ty2, new_ty))] in
		       let s = print_eqs_line eqs;unify eqs
		       in (s, subst_type s new_ty)
	 | _ -> err ("not a function"))
  | ListLit (head, tail) ->
     let (s1, ty1) = ty_exp tyenv head in
     let (s2, ty2) = ty_exp tyenv tail
     in (match ty2 with
	  TyList t_tail -> let eqs = [(ty1, t_tail)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
			   let s = unify eqs
			   in (s, subst_type s (TyList ty1))
	| t -> err ("cons tail " ^ string_of_ty_MkII t ^ " is not a list"))
  | EmpList -> ([], TyList (TyVar (fresh_tyvar ())))
  | Underscore -> ([], TyVar (fresh_tyvar ()))
  | _ -> err ("Not Implemented!")
	     
let ty_decl tyenv = function
    Exp e -> [ty_exp tyenv e]
  | _ -> err ("Not Implemented in decl!")



	     
