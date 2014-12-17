%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token LOR LAND PLUS MINUS MULT DIV LT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND REC

%token <int> INTV
%token <bool> BOOLV
%token <Syntax.id> ID
%token <Syntax.id> PMID
%token RARROW FUN DFUN

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | Lets SEMISEMI{ $1 }

FunDecl :
    Ident EQ Expr { FunExp ($1, $3) }
  | Ident FunDecl { FunExp ($1, $2) }

DeclExpr :
    Ident EQ Expr { Decl ($1, $3) }
  | Ident FunDecl { Decl ($1, $2) }

LetsAnd :
    AND DeclExpr { AndDecl ($2, NoneDecl) }
  | AND DeclExpr LetsAnd { AndDecl ($2, $3) }

//LetsRecAnd :
//AND Ident EQ FunExpr { 

Lets :
    LET DeclExpr { ManyDecl (AndDecl ($2, NoneDecl), NoneDecl) }
  | LET REC Ident EQ FunExpr { ManyDecl (AndDecl (RecDecl ($3, $5), NoneDecl), NoneDecl) }
  | LET REC Ident FunDecl { RecDecl ($3, $4) }

  | LET Ident EQ Expr LetsAnd  { ManyDecl (AndDecl(Decl ($2, $4), $5), NoneDecl) }
  | LET Ident FunDecl LetsAnd { ManyDecl (AndDecl (Decl ($2, $3), $4), NoneDecl) }
  | LET REC Ident EQ FunExpr LetsAnd { ManyDecl (AndDecl (RecDecl ($3, $5), $6), NoneDecl) }      
  | LET REC Ident FunDecl LetsAnd { ManyDecl (AndDecl (RecDecl ($3, $4), $5), NoneDecl) }      

  | LET DeclExpr Lets { ManyDecl (AndDecl($2, NoneDecl), $3) }
  | LET Ident EQ Expr LetsAnd Lets { ManyDecl (AndDecl(Decl ($2, $4), $5), $6) }
  | LET Ident FunDecl LetsAnd Lets { ManyDecl (AndDecl (Decl ($2, $3), $4), $5) }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | LOExpr { $1 }
  | FunExpr { $1 }

AndExpr :
    AND Ident EQ Expr { AndExp (Declare ($2, $4), AndEnd) }
  | AND Ident FunDecl { AndExp (Declare ($2, $3), AndEnd) }
  | AND Ident EQ Expr AndExpr { AndExp (Declare ($2, $4), $5) }
  | AND Ident FunDecl AndExpr { AndExp (Declare ($2, $3), $4) }

LetExpr :
    LET Ident EQ Expr IN Expr { LetExp (Declare ($2, $4), $6) }
  | LET Ident FunDecl IN Expr { LetExp (Declare ($2, $3), $5) }
  | LET REC Ident EQ FUN Ident RARROW Expr IN Expr
      { LetRecExp ($3, $6, $8, $10) }
  | LET Ident EQ Expr AndExpr IN Expr { LetandExp (AndExp(Declare ($2, $4), $5), $7) }
  | LET Ident FunDecl AndExpr IN Expr { LetandExp (AndExp(Declare ($2, $3), $4), $6) }

LOExpr :
    LAExpr LOR LAExpr { BinOp (LOr, $1, $3) }
  | LAExpr { $1 }

LAExpr :
    LAExpr LAND LTExpr { BinOp (LAnd, $1, $3) }
  | LTExpr { $1 }

LTExpr : 
    LTExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

//MidExpr :
//MidExpr MID

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | PExpr MINUS MExpr { BinOp (Minus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | MExpr DIV AppExpr { BinOp (Div, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | PMID { Var $1 }
  | LPAREN Expr RPAREN { $2 }


Ident :
    ID { $1 }
  | PMID { $1 }


IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

Fun_ :
    Ident RARROW Expr { FunExp ($1, $3) }
  | Ident Fun_ { FunExp ($1, $2) }
/*
DFun_ :
    Ident RARROW Expr { DFunExp ($1, $3) }
  | Ident DFun_ { DFunExp ($1, $2) }
*/
FunExpr :
    FUN Ident RARROW Expr { FunExp ($2, $4) }
  | FUN Ident Fun_ { FunExp ($2, $3) }
  | DFUN Ident RARROW Expr { DFunExp ($2, $4) }
//  | DFUN Ident DFun_ { DFunExp ($2, $3) }

