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
    Plus | Minus | Mult | Div ->
      (match ty1, ty2 with
	TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: +-*/"))
  | Lt ->
      (match ty1, ty2 with
	TyInt, TyInt -> TyBool
      | _ -> err ("Argument must be of integer: <"))
  | LAnd | LOr ->
      (match ty1, ty2 with
	TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of integer: +"))
  | _ -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x ->
      (try Environment.lookup x tyenv with
	Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
    if ty_exp tyenv exp1 = TyBool
    then
      (let t2 = ty_exp tyenv exp2
       and t3= ty_exp tyenv exp3
       in if t2 = t3
	 then t2
	 else err ("then and else expression must be same type! "
		   ^ string_of_ty t2 ^ " " ^ string_of_ty t3))
    else err ("Condition must be bool!")
  | LetExp (Declare (id, exp1), exp2) ->
    ty_exp (Environment.extend id (ty_exp tyenv exp1) tyenv) exp2
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> [ty_exp tyenv e]
  | _ -> err ("Not Implemented in decl!")


let rec unify =
  function
    [] -> []
  | (TyInt, TyInt) :: xs -> xs
  | (TyBool, TyBool) :: xs -> xs
  | (TyVar a, t) :: xs -> if MySet.member a (freevar_ty t)
			  then err ("err in unify")
			  else (TyVar a, t) :: (unify (subst_eqs [(a, t)] xs))
  | (t, TyVar a) :: xs -> unify ((TyVar a, t) :: xs)
  | _ -> err "err in unify"




	     
