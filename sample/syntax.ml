(* ML interpreter / type reconstruction *)
type id = string
type tyvar = int

exception Error of string

let err s = raise (Error s)
	       
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let rec freevar_ty t =
  match t with
    TyVar tv -> MySet.singleton tv
  | TyFun (tv1, tv2) -> MySet.union (freevar_ty tv1) (freevar_ty tv2)
  | _ -> MySet.empty

(* type scheme *)
type tysc = TyScheme of tyvar list * ty
let tysc_of_ty ty = TyScheme ([], ty)
let freevar_tysc tysc =
  freevar_ty

let rec upToN n = if n = 0 then [0] else upToN (n-1) @ [n]
let rec zip a =
  function [] -> []
	 | x::xs -> match a with
		    | [] -> []
		    | hd::tl -> (hd, x) :: (zip tl xs)
let rec search n =
  function [] -> err "err in search"
	 | (key, data)::xs -> if n = key
			      then data
			      else search n xs
  
let string_of_ty_MkII t =
  let tyList = List.sort (fun x y -> if x > y then 1 else 0) (MySet.to_list (freevar_ty t)) in
  let tyNum = List.length tyList in
  let appList = zip tyList (upToN tyNum) in
  let rec str_of_ty t flag =
    match t with
      TyInt -> "int"
    | TyBool -> "bool"
    | TyFun (t1, t2) -> if flag
			then "(" ^ str_of_ty t1 true ^ " -> " ^ str_of_ty t2 false ^ ")"
			else str_of_ty t1 true ^ " -> " ^ str_of_ty t2 false
    | TyVar tv -> "'" ^ Char.escaped (char_of_int ((search tv appList)+97))
  in str_of_ty t false

let string_of_ty t =
  let rec str_of_ty t flag =
    match t with
      TyInt -> "int"
    | TyBool -> "bool"
    | TyFun (t1, t2) -> if flag
			then "(" ^ str_of_ty t1 true ^ " -> " ^ str_of_ty t2 false ^ ")"
			else str_of_ty t1 true ^ " -> " ^ str_of_ty t2 false
    | TyVar tv -> "'" ^ Char.escaped (char_of_int (tv+97))
  in str_of_ty t false

	       
let pp_ty t = print_string (string_of_ty_MkII t)
	   
type binOp = Append | Plus | Minus | Mult | Div | Lt | LAnd | LOr

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | EmpList
  | ListLit of exp * exp
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | Declare of id * exp
  | RecDeclare of id * exp
  | LetExp of exp * exp (* 第一引数はDeclare *)
  | LetandExp of exp * exp
  | AndExp of exp * exp (* 第一引数はDeclare *)
  | AndEnd
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * exp * exp
  | LetRecAndExp of exp * exp
  | Underscore
  | MatchCondAndExp of exp * exp * exp (* 条件式と返す式の組と次の組*)
  | MatchCondEnd
  | MatchExp of exp * exp (* マッチさせる式と条件式リスト *)

type program = 
    Exp of exp
  | ManyDecl of program * program
  | AndDecl of program * program
  | Decl of id * exp
  | NoneDecl
  | RecDecl of id * exp
