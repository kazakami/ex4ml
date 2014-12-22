(* ML interpreter / type reconstruction *)
type id = string
type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let rec string_of_ty = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1, t2) -> string_of_ty t1 ^ " -> " ^ string_of_ty t2
  | TyVar tv -> "'" ^ Char.escaped (char_of_int (tv+97))

let pp_ty t = print_string (string_of_ty t)

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
