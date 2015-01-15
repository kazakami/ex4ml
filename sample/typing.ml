open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

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

let rec subst_eqs s eqs =
  let rec subst (a, t) =
    function
      [] -> []
    | (TyVar t1, TyVar t2) :: xs -> if t1 = a && t2 = a
				    then (t, t) :: (subst (a, t) xs)
				    else if t1 == a
				    then (t, TyVar t2) :: (subst (a, t) xs)
				    else if t2 == a
				    then (TyVar t2, t) :: (subst (a, t) xs)
				    else (TyVar t1, TyVar t2) :: (subst (a, t) xs)
    | (TyVar t1, tt) :: xs -> if t1 = a
			      then (t, tt) :: (subst (a, t) xs)
			      else (TyVar t1, tt) :: (subst (a, t) xs)
    | (tt, TyVar t2) :: xs -> if t2 = a
			      then (tt, t) :: (subst (a, t) xs)
			      else (tt, TyVar t2) :: (subst (a, t) xs)
    | ttt :: xs -> ttt :: (subst (a, t) xs)
  in match s with
       [] -> []
     | (a, t) :: xs -> subst_eqs xs (subst (a, t) eqs)
	   
let ty_prim op ty1 ty2 =
  match op with
    Plus | Minus | Mult | Div -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | LAnd | LOr -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | _ -> err "Not Implemented!"

	     
let rec unify =
  function
    [] -> []
  | (TyInt, TyInt) :: xs -> unify xs
  | (TyBool, TyBool) :: xs -> unify xs
  | (TyVar a, t) :: xs -> if MySet.member a (freevar_ty t)
			  then err ("err in unify")
			  else (a, t) :: (unify (subst_eqs [(a, t)] xs))
  | (t, TyVar a) :: xs -> unify ((TyVar a, t) :: xs)
  | _ -> err "err in unify"


	     
let rec ty_exp tyenv = function
    Var x ->
      (try ([], Environment.lookup x tyenv) with
	Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
    let s3 = unify eqs
    in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let (s3, ty3) = ty_exp tyenv exp3 in
     let eqs =  [(ty2, ty3); (ty1, TyBool)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
     let s4 = unify eqs
     in (s4, subst_type s4 ty2)
     (*
    if ty_exp tyenv exp1 = TyBool
    then
      (let t2 = ty_exp tyenv exp2
       and t3= ty_exp tyenv exp3
       in if t2 = t3
	 then t2
	 else err ("then and else expression must be same type! "
		   ^ string_of_ty t2 ^ " " ^ string_of_ty t3))
    else err ("Condition must be bool!")*)
  | LetExp (Declare (id, exp1), exp2) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
     let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
     let s3 = unify eqs
     in (s3, subst_type s3 ty2)
     (*
     ty_exp (Environment.extend id (ty_exp tyenv exp1) tyenv) exp2*)
  | FunExp (id, exp) ->
     let domty = TyVar (fresh_tyvar ()) in
     let s, ranty =
       ty_exp (Environment.extend id domty tyenv) exp in
     (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2
     in (match ty1 with
	   TyFun (tyf1, tyf2) -> let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(tyf1, ty2)] in
				 let s3 = unify eqs
				 in (s3, subst_type s3 tyf2)
	 | _ -> err ("not a function"))
  | _ -> err ("Not Implemented!")
	     
let ty_decl tyenv = function
    Exp e -> [ty_exp tyenv e]
  | _ -> err ("Not Implemented in decl!")



	     
