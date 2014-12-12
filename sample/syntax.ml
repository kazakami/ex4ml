(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | LAnd | LOr

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | Declare of id * exp
  | LetExp of exp * exp (* 第一引数はDeclare *)
  | LetandExp of exp * exp
  | AndExp of exp * exp (* 第一引数はDeclare *)
  | AndEnd
  | FunExp of id * exp
  | AppExp of exp * exp

type program = 
    Exp of exp
  | ManyDecl of program * program
  | AndDecl of program * program
  | Decl of id * exp
  | NoneDecl
