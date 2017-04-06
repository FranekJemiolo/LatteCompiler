-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParLatte where
import AbsLatte
import LexLatte
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  '++' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '--' { PT _ (TS _ 12) }
  '.' { PT _ (TS _ 13) }
  '/' { PT _ (TS _ 14) }
  ':' { PT _ (TS _ 15) }
  ';' { PT _ (TS _ 16) }
  '<' { PT _ (TS _ 17) }
  '<=' { PT _ (TS _ 18) }
  '=' { PT _ (TS _ 19) }
  '==' { PT _ (TS _ 20) }
  '>' { PT _ (TS _ 21) }
  '>=' { PT _ (TS _ 22) }
  '[' { PT _ (TS _ 23) }
  '[]' { PT _ (TS _ 24) }
  ']' { PT _ (TS _ 25) }
  'boolean' { PT _ (TS _ 26) }
  'class' { PT _ (TS _ 27) }
  'else' { PT _ (TS _ 28) }
  'extends' { PT _ (TS _ 29) }
  'false' { PT _ (TS _ 30) }
  'for' { PT _ (TS _ 31) }
  'if' { PT _ (TS _ 32) }
  'int' { PT _ (TS _ 33) }
  'new' { PT _ (TS _ 34) }
  'null' { PT _ (TS _ 35) }
  'return' { PT _ (TS _ 36) }
  'string' { PT _ (TS _ 37) }
  'true' { PT _ (TS _ 38) }
  'void' { PT _ (TS _ 39) }
  'while' { PT _ (TS _ 40) }
  '{' { PT _ (TS _ 41) }
  '||' { PT _ (TS _ 42) }
  '}' { PT _ (TS _ 43) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListTopDef { AbsLatte.Program $1 }
TopDef :: { TopDef }
TopDef : FnDef { AbsLatte.TopFnDef $1 }
       | 'class' Ident 'extends' Ident '{' ListProperty '}' { AbsLatte.ClassExtDef $2 $4 $6 }
       | 'class' Ident '{' ListProperty '}' { AbsLatte.ClassDef $2 $4 }
ListTopDef :: { [TopDef] }
ListTopDef : TopDef { (:[]) $1 } | TopDef ListTopDef { (:) $1 $2 }
Property :: { Property }
Property : FnDef { AbsLatte.FnProp $1 }
         | Type Ident ';' { AbsLatte.AttrProp $1 $2 }
ListProperty :: { [Property] }
ListProperty : Property { (:[]) $1 }
             | Property ListProperty { (:) $1 $2 }
Arg :: { Arg }
Arg : Type Ident { AbsLatte.Arg $1 $2 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
FnDef :: { FnDef }
FnDef : Type Ident '(' ListArg ')' Block { AbsLatte.FnDef $1 $2 $4 $6 }
Block :: { Block }
Block : '{' ListStmt '}' { AbsLatte.Block (reverse $2) }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Stmt :: { Stmt }
Stmt : ';' { AbsLatte.Empty }
     | Block { AbsLatte.BStmt $1 }
     | Type ListItem ';' { AbsLatte.Decl $1 $2 }
     | Expr '=' Expr ';' { AbsLatte.Ass $1 $3 }
     | Expr '++' ';' { AbsLatte.Incr $1 }
     | Expr '--' ';' { AbsLatte.Decr $1 }
     | 'return' Expr ';' { AbsLatte.Ret $2 }
     | 'return' ';' { AbsLatte.VRet }
     | 'if' '(' Expr ')' Stmt { AbsLatte.Cond $3 $5 }
     | 'if' '(' Expr ')' Stmt 'else' Stmt { AbsLatte.CondElse $3 $5 $7 }
     | 'while' '(' Expr ')' Stmt { AbsLatte.While $3 $5 }
     | Expr ';' { AbsLatte.SExp $1 }
     | 'for' '(' Type Ident ':' Expr ')' Stmt { AbsLatte.For $3 $4 $6 $8 }
Item :: { Item }
Item : Ident { AbsLatte.NoInit $1 }
     | Ident '=' Expr { AbsLatte.Init $1 $3 }
ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }
Type :: { Type }
Type : 'int' { AbsLatte.Int }
     | 'string' { AbsLatte.Str }
     | 'boolean' { AbsLatte.Bool }
     | 'void' { AbsLatte.Void }
     | Type '[]' { AbsLatte.ArrType $1 }
     | Ident { AbsLatte.ClsType $1 }
ListType :: { [Type] }
ListType : {- empty -} { [] }
         | Type { (:[]) $1 }
         | Type ',' ListType { (:) $1 $3 }
Expr6 :: { Expr }
Expr6 : Ident { AbsLatte.EVar $1 }
      | Integer { AbsLatte.ELitInt $1 }
      | 'true' { AbsLatte.ELitTrue }
      | 'false' { AbsLatte.ELitFalse }
      | 'new' Type { AbsLatte.ENewCls $2 }
      | 'new' Type '[' Expr ']' { AbsLatte.ENewArr $2 $4 }
      | Ident '(' ListExpr ')' { AbsLatte.EApp $1 $3 }
      | Expr6 '.' Ident '(' ListExpr ')' { AbsLatte.EPropApp $1 $3 $5 }
      | Expr6 '.' Ident { AbsLatte.EProp $1 $3 }
      | Expr6 '[' Expr ']' { AbsLatte.EArrGet $1 $3 }
      | '(' Ident ')' 'null' { AbsLatte.ENullCast $2 }
      | String { AbsLatte.EString $1 }
      | '(' Expr ')' { $2 }
Expr5 :: { Expr }
Expr5 : '-' Expr6 { AbsLatte.Neg $2 }
      | '!' Expr6 { AbsLatte.Not $2 }
      | Expr6 { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { AbsLatte.EMul $1 $2 $3 } | Expr5 { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { AbsLatte.EAdd $1 $2 $3 } | Expr4 { $1 }
Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { AbsLatte.ERel $1 $2 $3 } | Expr3 { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { AbsLatte.EAnd $1 $3 } | Expr2 { $1 }
Expr :: { Expr }
Expr : Expr1 '||' Expr { AbsLatte.EOr $1 $3 } | Expr1 { $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
AddOp :: { AddOp }
AddOp : '+' { AbsLatte.Plus } | '-' { AbsLatte.Minus }
MulOp :: { MulOp }
MulOp : '*' { AbsLatte.Times }
      | '/' { AbsLatte.Div }
      | '%' { AbsLatte.Mod }
RelOp :: { RelOp }
RelOp : '<' { AbsLatte.LTH }
      | '<=' { AbsLatte.LE }
      | '>' { AbsLatte.GTH }
      | '>=' { AbsLatte.GE }
      | '==' { AbsLatte.EQU }
      | '!=' { AbsLatte.NE }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
