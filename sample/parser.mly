%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token LOR LAND PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND

%token <int> INTV
%token <bool> BOOLV
%token <Syntax.id> ID
%token <Syntax.id> MID
%token RARROW FUN

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | Lets SEMISEMI{ $1 }
/*
Lets :
    LET Ident EQ Expr { ManyDecl ((Decl ($2, $4)), NoneDecl) } 
  | LET Ident EQ Expr Lets { ManyDecl ((Decl ($2, $4)), $5) }
*/

LetsAnd :
    AND Ident EQ Expr { AndDecl (Decl ($2, $4), NoneDecl) }
  | AND Ident EQ Expr LetsAnd { AndDecl (Decl ($2, $4), $5) }

Lets :
    LET Ident EQ Expr { ManyDecl (AndDecl(Decl ($2, $4), NoneDecl), NoneDecl) }
  | LET Ident EQ Expr LetsAnd  { ManyDecl (AndDecl(Decl ($2, $4), $5), NoneDecl) }
  | LET Ident EQ Expr Lets { ManyDecl (AndDecl(Decl ($2, $4), NoneDecl), $5) }
  | LET Ident EQ Expr LetsAnd Lets { ManyDecl (AndDecl(Decl ($2, $4), $5), $6) }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | LOExpr { $1 }
  | FunExpr { $1 }

AndExpr :
    AND Ident EQ Expr { AndExp ($2, $4, AndEnd) }
  | AND Ident EQ Expr AndExpr { AndExp ($2, $4, $5) }

LetExpr :
    LET Ident EQ Expr IN Expr { LetExp ($2, $4, $6) }
  | LET Ident EQ Expr AndExpr IN Expr { LetandExp (AndExp($2, $4, $5), $7) }

LOExpr :
    LAExpr LOR LAExpr { BinOp (LOr, $1, $3) }
  | LAExpr { $1 }

LAExpr :
    LAExpr LAND LTExpr { BinOp (LAnd, $1, $3) }
  | LTExpr { $1 }

LTExpr : 
    LTExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | MID { Var $1 }
  | LPAREN Expr RPAREN { $2 }


Ident :
    ID { $1 }
  | MID { $1 }


IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

   
FunExpr :
    FUN Ident RARROW Expr { FunExp ($2, $4) }
