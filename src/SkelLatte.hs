module SkelLatte where

-- Haskell module generated by the BNF converter

import AbsLatte
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x
transTopDef :: TopDef -> Result
transTopDef x = case x of
  TopFnDef fndef -> failure x
  ClassExtDef ident1 ident2 propertys -> failure x
  ClassDef ident propertys -> failure x
transProperty :: Property -> Result
transProperty x = case x of
  FnProp fndef -> failure x
  AttrProp type_ ident -> failure x
transArg :: Arg -> Result
transArg x = case x of
  Arg type_ ident -> failure x
transFnDef :: FnDef -> Result
transFnDef x = case x of
  FnDef type_ ident args block -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> failure x
  BStmt block -> failure x
  Decl type_ items -> failure x
  Ass expr1 expr2 -> failure x
  Incr expr -> failure x
  Decr expr -> failure x
  Ret expr -> failure x
  VRet -> failure x
  Cond expr stmt -> failure x
  CondElse expr stmt1 stmt2 -> failure x
  While expr stmt -> failure x
  SExp expr -> failure x
  For type_ ident expr stmt -> failure x
transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> failure x
  Init ident expr -> failure x
transType :: Type -> Result
transType x = case x of
  Int -> failure x
  Str -> failure x
  Bool -> failure x
  Void -> failure x
  ArrType type_ -> failure x
  ClsType ident -> failure x
  Fun type_ types -> failure x
  Arr type_ integer -> failure x
  Pointer type_ -> failure x
  VTable ident -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EVar ident -> failure x
  ELitInt integer -> failure x
  ELitTrue -> failure x
  ELitFalse -> failure x
  ENewCls type_ -> failure x
  ENewArr type_ expr -> failure x
  EApp ident exprs -> failure x
  EPropApp expr ident exprs -> failure x
  EProp expr ident -> failure x
  EArrGet expr1 expr2 -> failure x
  ENullCast ident -> failure x
  EString string -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x

