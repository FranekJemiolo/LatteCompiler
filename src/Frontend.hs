-- This program was written by Franciszek Jemioło, index number 346919
module Frontend where

-- This is the frontend module of the compiler

import AbsLatte
import ErrM
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M
import Prelude
import PrintLatte

------------------------------------------------------------------------------------------------------------------------
----------------------------- INTERNAL TYPES AND VARIABLES -------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
type Loc = Integer

data Variable = Variable {
  depth :: Integer,
  varType :: Type
} deriving Show

data Env = Env {
  variables :: M.Map Ident Loc,
  currentDepth :: Integer,
  funRetType :: Type,
  hasReturn :: Bool,
  currentClassName :: Maybe Ident
} deriving Show

data Class = Class {
  name :: Ident,
  parent :: Maybe Ident,
  fields :: M.Map Ident Type,
  methods :: M.Map Ident Type
} deriving Show

data Store = Store {
  varLocations :: M.Map Loc Variable,
  functions :: M.Map Ident Type,
  classes :: M.Map Ident Class,
  maxLoc :: Loc
} deriving Show

type Result a = ReaderT Env (ErrorT String (StateT Store IO)) a

getInitialEnv :: Env
getInitialEnv = Env {
  variables = M.empty,
  currentDepth = 0,
  funRetType = Void,
  hasReturn = False,
  currentClassName = Nothing
}

getInitialStore :: Store
getInitialStore = Store {
  functions = (M.fromList
    [(Ident "printInt", Fun Void [Int]),
    (Ident "printString", Fun Void [Str]),
    (Ident "error", Fun Void []),
    (Ident "readInt", Fun Int []),
    (Ident "readString", Fun Str []),
    (Ident "_appendString", Fun Str [Str, Str])]),
  classes = M.empty,
  varLocations = M.empty,
  maxLoc = 0
}

------------------------------------------------------------------------------------------------------------------------
----------------------------- ERROR MESSAGES ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
duplicatedFunErr :: Ident -> String
duplicatedFunErr (Ident x) = "Error! Duplicated declaration of function " ++ x

duplicatedClassErr :: Ident -> String
duplicatedClassErr (Ident x) = "Error! Duplicated declaration of class " ++ x

wrongMainFunction :: Type -> String
wrongMainFunction t = "Error! Wrong main function Type! Expected Int [], got: " ++ (show t)

undeclaredMainFunction :: String
undeclaredMainFunction = "Error! Main function is not declared!"

notEnoughReturns :: String
notEnoughReturns = "Error! Function has not enough returns!"

typeError :: Type -> Type -> String
typeError expected got = "Error! Expected type: " ++ (show expected) ++ " , but got: " ++ (show got)

undeclaredVariable :: Ident -> String
undeclaredVariable (Ident x) = "Error! Undeclared variable: " ++ x

undeclaredFunction :: Ident -> String
undeclaredFunction (Ident x) = "Error! Undeclared function: " ++ x

wrongApplication :: Ident -> [Type] -> [Type] -> String
wrongApplication (Ident f) expected got = "Error! Wrong application of function " ++ f ++
  ". Expected arguments' types: " ++ (show expected) ++ ", but got: " ++ (show got)

eitherIntStrTypeError :: Type -> String
eitherIntStrTypeError t = "Error! Expected either Int or Str type, but got " ++ (show t)

wrongReturnType :: Type -> Type -> String
wrongReturnType expected got = "Error! Wrong return type! Expected : " ++ (show expected) ++ " , but got: " ++
  (show got)

internalError :: String -> String
internalError msg = "Internal Error! " ++ msg

invalidDeclarationType :: Type -> Ident -> String
invalidDeclarationType t (Ident i) = "Error! Forbidden declaration of variable " ++ i ++ " with type " ++ (show t)

illegalDeclarationInSameBlock :: Ident -> String
illegalDeclarationInSameBlock (Ident i) = "Error! More than one declaration of variable " ++ i ++ " in the same block"

duplicateArgIdent :: Ident -> String
duplicateArgIdent (Ident i) = "Error! Duplicate function argument with name " ++ i

illegalOperation :: String -> String
illegalOperation msg = "Illegal operation: " ++ msg

undeclaredParentClass :: Ident -> Ident -> String
undeclaredParentClass (Ident clsName) (Ident baseName) = "Undeclared base class: " ++ baseName ++
  " for class: " ++ clsName

duplicatePropIdent :: Ident -> String
duplicatePropIdent (Ident i) = "Error! Duplicate class property with name " ++ i

undeclaredClass :: Ident -> String
undeclaredClass (Ident i) = "Error! Class " ++ i ++ " is not declared!"

methodCallOnPrimitive :: Ident -> String
methodCallOnPrimitive (Ident i) = "Error! Trying to call method: " ++ i ++ " on a non class type"

attributeOnPrimitive :: Ident -> String
attributeOnPrimitive (Ident i) = "Error! Trying to access attribute: " ++ i ++ " on a non class type"

noSuchMethod :: Ident -> Ident -> String
noSuchMethod (Ident clsName) (Ident methodName) = "Error! Class: " ++ clsName ++ " has no such method: " ++ methodName

noSuchAttribute :: Ident -> Ident -> String
noSuchAttribute (Ident clsName) (Ident attrName) = "Error! Class: " ++ clsName ++ " has no such attribute: " ++ attrName

noSuchAttributeArray :: Ident -> String
noSuchAttributeArray (Ident attrName) = "Error! Array type does not contain attribute: " ++ attrName

wrongMethodApplication :: Ident -> Ident -> [Type] -> [Type] -> String
wrongMethodApplication (Ident clsName) (Ident i) argsType vals = "Error! Wrong application of method: " ++ i ++
  " of class: " ++ clsName ++ " . Expected = " ++ (show argsType) ++ " , got = " ++
  (show vals)

arrayTypeError :: Type -> String
arrayTypeError t = "Error! Type error! Expected array of some type but got: " ++ (show t)

expectedMethod :: Ident -> Ident -> String
expectedMethod (Ident clsName) (Ident methodName) = "Error! Expected method: " ++ methodName ++ " in class: " ++
  clsName ++ " , but this class does not have a method with this name! Did you try to call an attribute of class?"

newOnPrimitive :: Type -> String
newOnPrimitive t = "Error! Wrong usage of new! Cannot create a new object that is not a class!" ++
  " Expected class of some type, but got type = " ++ (show t)

notLeftHandSide :: Expr -> String
notLeftHandSide exp =  "Error! Expression = " ++ (printTree exp) ++ " is not a left hand side value"

expectedArray :: Type -> String
expectedArray t = "Error! Expected array of some type but got = " ++ (show t)

selfIsRestrictedName :: String
selfIsRestrictedName = "Error! self is a restricted name for class usage! It is forbidden to declare it inside a class"

illegalClassExtension :: Ident -> String
illegalClassExtension (Ident i) = "Error! Class " ++ i ++ " cannot extend itself!"

------------------------------------------------------------------------------------------------------------------------
----------------------------- RUNNING FRONTEND -------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
runCheck :: Store -> Env -> Result a -> IO (Either String a)
runCheck store env result = do
  (res, _) <- runStateT (runErrorT (runReaderT result env)) store
  return res

checkProgram :: Program -> Result Program
checkProgram (Program topDefs) = do
  --env <- ask
  -- First we should add all the class definitions to know the types
  -- And add all top definitions of functions
  addTopDefs topDefs
  -- Check for main function
  funs <- gets functions
  case M.lookup (Ident "main") funs of
    Just (Fun Int []) -> do
      -- Ok we got the function so we should type check them
      checkTopDefs topDefs
      -- After type check we can optimize them
      optimizedTopDefs <- mapM optimizeTopDef topDefs
      return $ Program optimizedTopDefs
    Just t -> throwError $ wrongMainFunction t
    Nothing -> throwError undeclaredMainFunction

addTopDefs :: [TopDef] -> Result ()
addTopDefs [] = return ()
addTopDefs (x:rest) = case x of
  f@(TopFnDef (FnDef type_ ident args block)) -> do
    funs <- gets functions
    case M.lookup ident funs of
      Just _ -> throwError $ duplicatedFunErr ident
      Nothing -> do
        modify (\state -> state {
          functions = M.insert ident (getTopDefType f) (functions state)
        })
        addTopDefs rest
  cls@(ClassDef name props) -> do
    clss <- gets classes
    case M.lookup name clss of
      Just _ -> throwError $ duplicatedClassErr name
      Nothing -> do
        converted <- convertClassDefToClass cls
        modify (\state -> state {
          classes = M.insert name converted (classes state)
        })
        addTopDefs rest
  cls@(ClassExtDef name baseName props) -> if name == baseName then throwError $ (illegalClassExtension name)
    else do
    clss <- gets classes
    case M.lookup name clss of
      Just _ -> throwError $ duplicatedClassErr name
      Nothing -> do
        converted <- convertClassDefToClass cls
        modify (\state -> state {
          classes = M.insert name converted (classes state)
        })
        addTopDefs rest

------------------------------------------------------------------------------------------------------------------------
----------------------------- TYPE CHECKING ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
----------------------------- CHECK TYPES FOR EQUAL --------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
equalTypes :: [Type] -> [Type] -> Result Bool
equalTypes [] [] = return True
equalTypes _ [] = return False
equalTypes [] _ = return False
equalTypes (x:xs) (y:ys) = do
  sup <- isSuper x y
  if ((x == y) || (sup))
  then
    equalTypes xs ys
  else
    return False

isSuper :: Type -> Type -> Result Bool
isSuper cls1@(ClsType clsName1) (ClsType clsName2) = if (clsName1 == clsName2) then return True
  else do
    clss <- gets classes
    case M.lookup clsName2 clss of
      Just cls -> case (parent cls) of
        Just parentName -> isSuper cls1 (ClsType parentName)
        Nothing -> return False
      Nothing -> return False
isSuper _ _ = return False

------------------------------------------------------------------------------------------------------------------------
----------------------------- TOPDEF -----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
getTopDefType :: TopDef -> Type
getTopDefType (TopFnDef (FnDef type_ _ args _)) = Fun type_ (Prelude.map (\(Arg t _) -> t) args)

checkForSelf :: [(Ident, a)] -> Result [(Ident, a)]
checkForSelf [] = return []
checkForSelf (((Ident x), y):rest) = if x == "self" then throwError $ selfIsRestrictedName
  else do
    r <- (checkForSelf rest)
    return (((Ident x), y):r)


convertClassDefToClass :: TopDef -> Result Class
convertClassDefToClass (ClassDef nm props) = do
  mFields <- (checkForSelf
    (map (\(AttrProp t i) -> (i, t)) (filter (\x -> case x of
      (AttrProp _ _) -> True
      (FnProp _) -> False) props)))
  mMethods <- (checkForSelf (map
    (\(FnProp (FnDef t i args _)) -> (i, (Fun t (map (\(Arg t i) -> t) args)))) (filter (\x -> case x of
      (AttrProp _ _) -> False
      (FnProp _) -> True) props)))
  return (Class {
    name = nm,
    parent = Nothing,
    -- Insert self also
    fields = M.insert (Ident "self") (ClsType nm) (M.fromList mFields),
    methods = M.fromList mMethods
  })
convertClassDefToClass (ClassExtDef nm baseName props) = do
  mFields <- (checkForSelf
    (map (\(AttrProp t i) -> (i, t)) (filter (\x -> case x of
      (AttrProp _ _) -> True
      (FnProp _) -> False) props)))
  mMethods <- (checkForSelf (map
    (\(FnProp (FnDef t i args _)) -> (i, (Fun t (map (\(Arg t i) -> t) args)))) (filter (\x -> case x of
      (AttrProp _ _) -> False
      (FnProp _) -> True) props)))
  return (Class {
    name = nm,
    parent = Just baseName,
    fields = M.insert (Ident "self") (ClsType nm) (M.fromList mFields),
    methods = M.fromList mMethods
  })

checkTopDefs :: [TopDef] -> Result ()
checkTopDefs [] = return ()
checkTopDefs (x:xs) = do
  checkTopDef x
  checkTopDefs xs

getNewLocations :: Loc -> [a] -> (Loc, [(Loc, a)])
getNewLocations loc [] = (loc, [])
getNewLocations loc (x:xs) = let
  (newLoc, locations) = getNewLocations (loc + 1) xs
  in
  (newLoc, ((loc, x):locations))

checkDups :: (Ident -> String) -> [Ident] -> Result ()
checkDups _ [] = return ()
checkDups errFun (x:xs) =
  if x `elem` xs then
    throwError $ errFun x
  else
    (checkDups errFun xs)

checkArgs :: [Arg] -> Result ()
checkArgs [] = return ()
checkArgs ((Arg t i):rest) = if i == (Ident "self") then throwError $ selfIsRestrictedName
  else case t of
    (Void) -> throwError $ invalidDeclarationType t i
    _ -> checkArgs rest

checkTopDef :: TopDef -> Result ()
checkTopDef (TopFnDef fndef) = local (\env -> env { currentClassName = Nothing }) (checkFnDef fndef)
checkTopDef clsDef@(ClassDef clsName props) = (local (\env -> env { currentClassName = Just clsName })
  (checkProps props)) `catchError` (\msg -> throwError $ (msg ++ "\n" ++ "in class: \n" ++ (printTree clsDef)))
checkTopDef clsDef@(ClassExtDef clsName parentName props) = do
  -- Check if parent class exists?
  clss <- gets classes
  case M.lookup parentName clss of
    Just _ -> (local (\env -> env { currentClassName = Just clsName }) (checkProps props)) `catchError`
      (\msg -> throwError $ (msg ++ "\n" ++ "in class: \n" ++ (printTree clsDef)))
    Nothing -> throwError $ (undeclaredParentClass clsName parentName)

------------------------------------------------------------------------------------------------------------------------
----------------------------- PROPERTY ---------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
checkVoidAttr :: [Property] -> Result ()
checkVoidAttr [] = return ()
checkVoidAttr ((AttrProp t i):xs) = case t of
  (Void) -> throwError $ invalidDeclarationType Void i
  _ -> checkVoidAttr xs
checkVoidAttr (_:xs) = checkVoidAttr xs

checkProps :: [Property] -> Result ()
checkProps props = do
  -- Check for duplicates
  checkDups (duplicatePropIdent) (map (\prop -> case prop of
    (FnProp (FnDef _ i _ _)) -> i
    (AttrProp _ i) -> i) props)
  -- Check for void type attr
  checkVoidAttr props
  -- Insert to local environment the attributes
  env <- addAttributesToEnv
  -- Check the props
  local (\_ -> env) (checkPropsList props)

checkPropsList :: [Property] -> Result ()
checkPropsList [] = return ()
checkPropsList ((FnProp fnDef):props) = do
  checkFnDef fnDef
  checkPropsList props
checkPropsList ((AttrProp t i):props) =
  if t == Void then
    throwError $ invalidDeclarationType t i
  else do
    -- Check if t exists
    checkTypeExistence t
    checkPropsList props

------------------------------------------------------------------------------------------------------------------------
----------------------------- ADDING CLASS PROPERTIES ------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
checkTypeExistence :: Type -> Result ()
checkTypeExistence (ClsType clsName) = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just _ -> return ()
    Nothing -> throwError $ undeclaredClass clsName
checkTypeExistence (Fun retType args) = do
  checkTypeExistence retType
  mapM (checkTypeExistence) args
  return ()
checkTypeExistence (ArrType t) = checkTypeExistence t
checkTypeExistence (Arr t i) = checkTypeExistence t
checkTypeExistence _ = return ()

addClassAttributesToEnv :: Ident -> Result Env
addClassAttributesToEnv clsName = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just cls -> case (parent cls) of
      Just parentName -> do
        env <- addClassAttributesToEnv parentName
        newloc <- gets maxLoc
        let (newMaxLoc, attrsWithLoc) = getNewLocations newloc (M.toList $ fields cls) in do
          curDepth <- asks currentDepth
          modify (\store -> store {
            varLocations = M.union (M.fromList (map
              (\(loc, (i, t)) -> (loc, (Variable { varType = t, depth = curDepth }))) attrsWithLoc)) (varLocations store),
              maxLoc = newMaxLoc
          })
          return env {
            variables = M.union (M.fromList (map (\(loc, (i, t)) -> (i, loc)) attrsWithLoc)) (variables env)
          }
      Nothing -> do
        newloc <- gets maxLoc
        env <- ask
        let (newMaxLoc, attrsWithLoc) = getNewLocations newloc (M.toList $ fields cls) in do
          curDepth <- asks currentDepth
          modify (\store -> store {
            varLocations = M.union (M.fromList (map
              (\(loc, (i, t)) -> (loc, (Variable { varType = t, depth = curDepth }))) attrsWithLoc)) (varLocations store),
              maxLoc = newMaxLoc
          })
          return env {
            variables = M.union (M.fromList (map (\(loc, (i, t)) -> (i, loc)) attrsWithLoc)) (variables env)
          }
    Nothing -> throwError $ undeclaredClass clsName

addAttributesToEnv :: Result Env
addAttributesToEnv = do
  -- Get current class
  clsName <- asks currentClassName
  case clsName of
    Just nm -> do
      env' <- local (\env -> env { currentDepth = ((currentDepth env) + 1) }) (addClassAttributesToEnv nm)
      return env'
    Nothing -> ask

------------------------------------------------------------------------------------------------------------------------
----------------------------- CHECK FNDEF ------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
checkFnDef :: FnDef -> Result ()
checkFnDef f@(FnDef type_ ident args block) = do
  -- Setting block depth to +1 and inserting args as variables
  newloc <- gets maxLoc
  checkDups (duplicateArgIdent) (map (\(Arg t i) -> i) args)
  checkArgs args
  let (newMaxLoc, argsWithLoc) = getNewLocations newloc args in do
    curDepth <- asks currentDepth
    modify (\store -> store {
      varLocations = M.union (M.fromList (map
        (\(loc, Arg t i) -> (loc, (Variable { varType = t, depth = curDepth + 1}))) argsWithLoc)) (varLocations store),
      maxLoc = newMaxLoc
    })
    (local (\env -> env { funRetType = type_,
      currentDepth = (currentDepth env) + 1,
      variables = M.union (M.fromList (map (\(loc, Arg t i) -> (i, loc)) argsWithLoc)) (variables env),
      hasReturn = False
    }) (checkFunBlock block)) `catchError` (\msg -> throwError $ (msg ++ "\n" ++ "in Function: \n" ++ (printTree f)))

checkFunBlock :: Block -> Result ()
checkFunBlock x@(Block stmts) = do
  (checkStmts stmts) `catchError` (\msg -> throwError $ (msg ++ "\n" ++ "in Block: \n" ++ (printTree x)))
  return ()

------------------------------------------------------------------------------------------------------------------------
----------------------------- CHECK STMTS ------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
checkStmts :: [Stmt] -> Result Env
checkStmts [] = ask
checkStmts (stmt:stmts) = do
  env' <- (checkStmt stmt) `catchError` (\msg -> throwError $ (msg ++ "\n" ++ "in Statement: \n" ++ (printTree stmt)))
  local (\env -> env' { hasReturn = ((hasReturn env) || (hasReturn env')) }) (checkStmts stmts)

------------------------------------------------------------------------------------------------------------------------
----------------------------- DECLARING ITEMS --------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
declareVariableItem :: Type -> Item -> Result Env
declareVariableItem (Void) (NoInit i) = throwError $ invalidDeclarationType Void i
declareVariableItem (Void) (Init i exp) = throwError $ invalidDeclarationType Void i
declareVariableItem t (NoInit i) = do
  env <- ask
  case (currentClassName env) of
    Just _ -> if i == (Ident "self") then throwError $ selfIsRestrictedName else do
      st <- get
      -- Check if on the same level exists the same var
      case M.lookup i (variables env) of
        Just loc -> case M.lookup loc (varLocations st) of
          Just var ->
            if (depth var) == (currentDepth env) then
              throwError $ illegalDeclarationInSameBlock i
            else do
              -- Get the location for the new variable
              newloc <- gets maxLoc
              -- Insert the variable to store
              modify (\store -> store {
                maxLoc = (maxLoc store) + 1,
                varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
              })
              -- And overwrite the ident in the environment
              return env {
                variables = M.insert i newloc (variables env)
              }
          Nothing -> throwError $ internalError "Location not found"
        Nothing -> do
          -- Get the location for the new variable
          newloc <- gets maxLoc
          -- Insert the variable to store
          modify (\store -> store {
            maxLoc = (maxLoc store) + 1,
            varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
          })
          -- And overwrite the ident in the environment
          return env {
            variables = M.insert i newloc (variables env)
          }
    _ -> do
      st <- get
      -- Check if on the same level exists the same var
      case M.lookup i (variables env) of
        Just loc -> case M.lookup loc (varLocations st) of
          Just var ->
            if (depth var) == (currentDepth env) then
              throwError $ illegalDeclarationInSameBlock i
            else do
              -- Get the location for the new variable
              newloc <- gets maxLoc
              -- Insert the variable to store
              modify (\store -> store {
                maxLoc = (maxLoc store) + 1,
                varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
              })
              -- And overwrite the ident in the environment
              return env {
                variables = M.insert i newloc (variables env)
              }
          Nothing -> throwError $ internalError "Location not found"
        Nothing -> do
          -- Get the location for the new variable
          newloc <- gets maxLoc
          -- Insert the variable to store
          modify (\store -> store {
            maxLoc = (maxLoc store) + 1,
            varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
          })
          -- And overwrite the ident in the environment
          return env {
            variables = M.insert i newloc (variables env)
          }
declareVariableItem t (Init i exp) = do
  expType <- evalType exp
  sup <- isSuper t expType
  if (expType == t) || sup then do
    env <- ask
    case (currentClassName env) of
      Just _ -> if i == (Ident "self") then throwError $ selfIsRestrictedName else do
        st <- get
        -- Check if on the same level exists the same var
        case M.lookup i (variables env) of
          Just loc -> case M.lookup loc (varLocations st) of
            Just var ->
              if (depth var) == (currentDepth env) then
                throwError $ illegalDeclarationInSameBlock i
              else do
                -- Get the location for the new variable
                newloc <- gets maxLoc
                -- Insert the variable to store
                modify (\store -> store {
                  maxLoc = (maxLoc store) + 1,
                  varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
                })
                -- And overwrite the ident in the environment
                return env {
                  variables = M.insert i newloc (variables env)
                }
            Nothing -> throwError $ internalError "Location not found"
          Nothing -> do
            -- Get the location for the new variable
            newloc <- gets maxLoc
            -- Insert the variable to store
            modify (\store -> store {
              maxLoc = (maxLoc store) + 1,
              varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
            })
            -- And overwrite the ident in the environment
            return env {
              variables = M.insert i newloc (variables env)
            }
      _ -> do
        st <- get
        -- Check if on the same level exists the same var
        case M.lookup i (variables env) of
          Just loc -> case M.lookup loc (varLocations st) of
            Just var ->
              if (depth var) == (currentDepth env) then
                throwError $ illegalDeclarationInSameBlock i
              else do
                -- Get the location for the new variable
                newloc <- gets maxLoc
                -- Insert the variable to store
                modify (\store -> store {
                  maxLoc = (maxLoc store) + 1,
                  varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
                })
                -- And overwrite the ident in the environment
                return env {
                  variables = M.insert i newloc (variables env)
                }
            Nothing -> throwError $ internalError "Location not found"
          Nothing -> do
            -- Get the location for the new variable
            newloc <- gets maxLoc
            -- Insert the variable to store
            modify (\store -> store {
              maxLoc = (maxLoc store) + 1,
              varLocations = M.insert newloc (Variable { depth = currentDepth env, varType = t}) (varLocations store)
            })
            -- And overwrite the ident in the environment
            return env {
              variables = M.insert i newloc (variables env)
            }
  else throwError $ typeError t expType

declareItems :: Type -> [Item] -> Result Env
declareItems _ [] = ask
declareItems t (x:xs) = do
  env <- declareVariableItem t x
  env' <- local (\_ -> env) (declareItems t xs)
  return env'

------------------------------------------------------------------------------------------------------------------------
----------------------------- CHECK STMT -------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
getLeftHandValue :: Expr -> Result Type
getLeftHandValue (EVar i) = do
  vars <- asks variables
  case M.lookup i vars of
    Just loc -> do
      locs <- gets varLocations
      case M.lookup loc locs of
        Just var -> return (varType var)
        Nothing -> throwError $ internalError "Variable not found in var locations"
    Nothing -> throwError $ undeclaredVariable i
getLeftHandValue (EArrGet exp1 exp2) = do
  retType <- getLeftHandValue exp1
  case retType of
    (ArrType t) -> do
      indexType <- evalType exp2
      if indexType == Int then
        return t
      else
        throwError $ typeError Int indexType
    (Arr t i) -> do
      indexType <- evalType exp2
      if indexType == Int then
        return t
      else
        throwError $ typeError Int indexType
    _ -> throwError $ expectedArray retType
getLeftHandValue (EProp exp i) = do
  t <- getLeftHandValue exp
  case t of
    (ClsType clsName) -> do
      lhsType <- lookForAttribute clsName i
      return lhsType
    _ -> throwError $ notLeftHandSide exp
getLeftHandValue x = throwError $ notLeftHandSide x

checkStmt :: Stmt -> Result Env
checkStmt (Ret exp) = do
  t <- evalType exp
  retType <- asks funRetType
  sup <- isSuper retType t
  if ((t == retType) || sup) then do
    env <- ask
    return env { hasReturn = True }
  else throwError $ wrongReturnType retType t
checkStmt (VRet) = do
  retType <- asks funRetType
  if retType == Void then do
    env <- ask
    return env { hasReturn = True }
  else throwError $ wrongReturnType retType Void
checkStmt (Empty) = ask
checkStmt (BStmt (Block stmts)) = local (\env -> env {currentDepth = ((currentDepth env) + 1)}) (checkStmts stmts)
checkStmt (Decl t items) = declareItems t items
checkStmt (Ass lhs exp) = do
  -- Check if lhs is a left hand side value
  lhsType <- getLeftHandValue lhs
  t <- evalType exp
  case lhs of
    (EVar x) -> do
      env <- ask
      case (currentClassName env) of
        Just _ -> if x == (Ident "self") then throwError $ selfIsRestrictedName else do
          vars <- asks variables
          case M.lookup x vars of
            Just loc -> do
              locs <- gets varLocations
              case M.lookup loc locs of
                Just var -> do
                  sup <- isSuper (varType var) t
                  if (t == (varType var)) || sup then ask
                  else throwError $ typeError (varType var) t
                Nothing -> throwError $ internalError "Variable not found in var locations"
            Nothing -> throwError $ undeclaredVariable x
        Nothing -> do
          vars <- asks variables
          case M.lookup x vars of
            Just loc -> do
              locs <- gets varLocations
              case M.lookup loc locs of
                Just var -> do
                  sup <- isSuper (varType var) t
                  if (t == (varType var)) || sup then ask
                  else throwError $ typeError (varType var) t
                Nothing -> throwError $ internalError "Variable not found in var locations"
            Nothing -> throwError $ undeclaredVariable x
    _ -> do
      sup <- isSuper lhsType t
      if (lhsType == t) || sup then ask
      else throwError $ typeError lhsType t
checkStmt (Incr lhs) = do
  lhsType <- getLeftHandValue lhs
  case lhsType of
    Int -> ask
    _ -> throwError $ typeError Int lhsType
checkStmt (Decr lhs) = do
  lhsType <- getLeftHandValue lhs
  case lhsType of
    Int -> ask
    _ -> throwError $ typeError Int lhsType
checkStmt (Cond exp stmt) = do
  t <- evalType exp
  if t == Bool then do
    env <- ask
    env' <- checkStmt stmt
    return env
  else throwError $ typeError Bool t
checkStmt (CondElse exp stmt1 stmt2) = do
  t <- evalType exp
  if t == Bool then do
    env <- ask
    env' <- checkStmt stmt1
    env'' <- checkStmt stmt2
    return (env { hasReturn = ((hasReturn env) || ((hasReturn env') && (hasReturn env'')))})
  else throwError $ typeError Bool t
checkStmt (While exp stmt) = do
  t <- evalType exp
  if t == Bool then do
    env <- ask
    env' <- checkStmt stmt
    return (env { hasReturn = ((hasReturn env) || (hasReturn env')) })
  else throwError $ typeError Bool t
checkStmt (SExp exp) = do
  t <- evalType exp
  env <- ask
  return env
checkStmt (For t i exp stmt) =
  if t == Void then
    throwError $ invalidDeclarationType t i
  else do
    val <- evalType exp
    if val == (ArrType t) then do
      -- declare the i variable
      envB <- ask
      env' <- local (\env -> env { currentDepth = ((currentDepth env) + 1)}) (declareVariableItem t (NoInit i))
      -- check stmt
      env'' <- local (\_ -> env') (checkStmt stmt)
      return (envB { hasReturn = ((hasReturn env'') || (hasReturn envB)) })
    else throwError $ typeError (ArrType t) (val)

------------------------------------------------------------------------------------------------------------------------
----------------------------- EVAL TYPE EXP ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
lookForMethod :: Ident -> Ident -> Result Type
lookForMethod clsName methodName = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just cls -> case (parent cls) of
      Just parentName -> do
        -- First search for method in itself, then parent
        case M.lookup methodName (methods cls) of
          Just t -> return t
          Nothing -> lookForMethod parentName methodName
      Nothing ->
        case M.lookup methodName (methods cls) of
          Just t -> return t
          Nothing -> throwError $ noSuchMethod clsName methodName
    Nothing -> throwError $ undeclaredClass clsName

lookForAttribute :: Ident -> Ident -> Result Type
lookForAttribute clsName attrName = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just cls -> case (parent cls) of
      Just parentName -> do
        -- First search for attribute in itself, then parent
        case M.lookup attrName (fields cls) of
          Just t -> return t
          Nothing -> lookForAttribute parentName attrName
      Nothing ->
        case M.lookup attrName (fields cls) of
          Just t -> return t
          Nothing -> throwError $ noSuchAttribute clsName attrName
    Nothing -> throwError $ undeclaredClass clsName

evalType :: Expr -> Result Type
evalType (EVar ident) = do
  vars <- asks variables
  case M.lookup ident vars of
    Just x -> do
      varLocs <- gets varLocations
      case M.lookup x varLocs of
        Just y -> return $ (varType y)
        Nothing -> throwError $ internalError "Variable not found in var locations"
    Nothing -> throwError $ undeclaredVariable ident
evalType (ELitInt _) = return Int
evalType (ELitTrue) = return Bool
evalType (ELitFalse) = return Bool
evalType (EApp ident exprs) = do
  funs <- gets functions
  case M.lookup ident funs of
    Just (Fun retType args) -> do
      evaluatedTypes <- mapM evalType exprs
      eqTypes <- equalTypes args evaluatedTypes
      if eqTypes then
        return retType
      else
        throwError $ wrongApplication ident args evaluatedTypes
    Nothing -> throwError $ undeclaredFunction ident
evalType (EString _) = return Str
evalType (ENewCls t) = case t of
  cls@(ClsType _) -> return cls
  _ -> throwError $ newOnPrimitive t
evalType (ENewArr t exp) = do
  val <- evalType exp
  case val of
    Int -> return (ArrType t)
    _ -> throwError $ typeError Int val
evalType (EPropApp exp i exprs) = do
  val <- evalType exp
  case val of
    (ClsType clsName) -> do
      methodType <- lookForMethod clsName i
      case methodType of
        (Fun retType argsType) -> do
          vals <- mapM (evalType) exprs
          eqTypes <- equalTypes argsType vals
          if eqTypes then
            return retType
          else
            throwError $ wrongMethodApplication clsName i argsType vals
        _ -> throwError $ expectedMethod clsName i
    _ -> throwError $ methodCallOnPrimitive i
evalType (EProp exp i) = do
  val <- evalType exp
  case val of
    (ClsType clsName) -> do
      propType <- lookForAttribute clsName i
      return propType
    (ArrType t) -> case i of
      (Ident "length") -> return Int
      _ -> throwError $ noSuchAttributeArray i
    _ -> throwError $ attributeOnPrimitive i
evalType (EArrGet exp1 exp2) = do
  arrType <- evalType exp1
  case arrType of
    (ArrType t) -> do
      indexType <- evalType exp2
      case indexType of
        Int -> return t
        _ -> throwError $ typeError Int indexType
    _ -> throwError $ arrayTypeError arrType
evalType (ENullCast i) = do
  clss <- gets classes
  case M.lookup i clss of
    Just cls -> return (ClsType i)
    Nothing -> throwError $ undeclaredClass i
evalType (Not expr) = do
  t <- evalType expr
  case t of
    Bool -> return Bool
    _ -> throwError $ typeError Bool t
evalType (Neg expr) = do
  t <- evalType expr
  case t of
    Int -> return Int
    _ -> throwError $ typeError Int t
evalType (EMul exp1 op exp2) = do
  t1 <- evalType exp1
  case t1 of
    Int -> do
      t2 <- evalType exp2
      case t2 of
        Int -> return Int
        _ -> throwError $ typeError Int t2
    _ -> throwError $ typeError Int t1
evalType (EAdd exp1 op exp2) = do
  t1 <- evalType exp1
  case t1 of
    Str -> do
      t2 <- evalType exp2
      case t2 of
        Str -> case op of
          Plus -> return Str
          _ -> throwError $ illegalOperation ((show op) ++ " on string")
        _ -> throwError $ typeError Str t2
    Int -> do
      t2 <- evalType exp2
      case t2 of
        Int -> return Int
        _ -> throwError $ typeError Int t2
    _ -> throwError $ eitherIntStrTypeError t1
evalType (ERel exp1 op exp2) = do
  t1 <- evalType exp1
  case t1 of
    Int -> do
      t2 <- evalType exp2
      case t2 of
        Int -> return Bool
        _ -> throwError $ typeError Int t2
    Bool -> do
      t2 <- evalType exp2
      case t2 of
        Bool -> case op of
          EQU -> return Bool
          NE -> return Bool
          _ -> throwError $ illegalOperation ((show op) ++ " on boolean")
        _ -> throwError $ typeError Bool t2
    (ClsType i) -> do
      t2 <- evalType exp2
      case t2 of
        (ClsType j) -> if i == j
          then case op of
            EQU -> return Bool
            NE -> return Bool
            _ -> throwError $ illegalOperation ((show op) ++ " on classes")
          else throwError $ typeError (ClsType i) t2
        _ -> throwError $ typeError (ClsType i) t2
    _ -> throwError $ typeError Int t1
evalType (EAnd exp1 exp2) = do
  t1 <- evalType exp1
  case t1 of
    Bool -> do
      t2 <- evalType exp2
      case t2 of
        Bool -> return Bool
        _ -> throwError $ typeError Bool t2
    _ -> throwError $ typeError Bool t1
evalType (EOr exp1 exp2) = do
  t1 <- evalType exp1
  case t1 of
    Bool -> do
      t2 <- evalType exp2
      case t2 of
        Bool -> return Bool
        _ -> throwError $ typeError Bool t2
    _ -> throwError $ typeError Bool t1

----------------------------- OPTIMIZATIONS ----------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
----------------------------- OPTIMIZE TOP DEF -------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- Removing dead code plus inserting evaluated const expressions
optimizeTopDef :: TopDef -> Result TopDef
optimizeTopDef (TopFnDef fndef) = do
  optimizedFnDef <- (optimizeFnDef fndef)
  return (TopFnDef optimizedFnDef)
optimizeTopDef clsDef@(ClassDef clsName props) = do
  optimizedProps <- (local (\env -> env { currentClassName = Just clsName }) (optimizeProps props)) `catchError`
    (\msg -> throwError $ (msg ++ "\n" ++ "in Class: \n" ++ (printTree clsDef)))
  return (ClassDef clsName optimizedProps)
optimizeTopDef clsDef@(ClassExtDef clsName parentName props) = do
  optimizedProps <- (local (\env -> env { currentClassName = Just clsName }) (optimizeProps props)) `catchError`
    (\msg -> throwError $ (msg ++ "\n" ++ "in Class: \n" ++ (printTree clsDef)))
  return (ClassExtDef clsName parentName optimizedProps)

------------------------------------------------------------------------------------------------------------------------
----------------------------- OPTIMIZE CLASS PROPERTIES ----------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
optimizeProps :: [Property] -> Result [Property]
optimizeProps [] = return []
optimizeProps ((FnProp fnDef):props) = do
  optimizedFnDef <- optimizeFnDef fnDef
  optimizedProps <- optimizeProps props
  return ((FnProp optimizedFnDef):optimizedProps)
optimizeProps ((AttrProp t i):props) = do
  optimizedProps <- optimizeProps props
  return ((AttrProp t i):optimizedProps)

------------------------------------------------------------------------------------------------------------------------
----------------------------- OPTIMIZE FNDEF ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
optimizeFnDef :: FnDef -> Result FnDef
optimizeFnDef f@(FnDef type_ ident args block) = do
  (optimizedBlock, hasReturn) <- (optimizeBlock block) `catchError` (
    \msg -> throwError $ (msg ++ "\n" ++ "in Function: \n" ++ (printTree f)))
  if (hasReturn || (type_ == Void)) then
    return (FnDef type_ ident args optimizedBlock)
  else throwError $ ((notEnoughReturns) ++ "\n" ++ "in Function: \n" ++ (printTree f))

------------------------------------------------------------------------------------------------------------------------
----------------------------- OPTIMIZE BLOCK ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
optimizeBlock :: Block -> Result (Block, Bool)
optimizeBlock x@(Block stmts) = do
  (optimizedStmts, hasRet) <- (optimizeStmts stmts) `catchError`
    (\msg -> throwError $ (msg ++ "\n" ++ "in Block: \n" ++ (printTree x)))
  return ((Block optimizedStmts), hasRet)

------------------------------------------------------------------------------------------------------------------------
----------------------------- OPTIMIZE STMTS ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
optimizeStmts :: [Stmt] -> Result ([Stmt], Bool)
optimizeStmts [] = return ([], False)
optimizeStmts (stmt:rest) = do
  (optimizedStmt, hasReturn) <- (optimizeStmt stmt) `catchError`
    (\msg -> throwError $ (msg ++ "\n" ++ "in Statement: \n" ++ (printTree stmt)))
  if hasReturn then
    return ([optimizedStmt], hasReturn)
  else do
    (optimizedRest, hasRet) <- (optimizeStmts rest)
    return ((optimizedStmt:optimizedRest), hasRet)

------------------------------------------------------------------------------------------------------------------------
----------------------------- OPTIMIZE DECLARATION OF ITEMS ------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
optimizeItems :: [Item] -> Result [Item]
optimizeItems [] = return []
optimizeItems ((Init i exp):rest) = do
  val <- evalConst exp
  optimizedRest <- optimizeItems rest
  return ((Init i val):optimizedRest)
optimizeItems (x:rest) = do
  optimizedRest <- optimizeItems rest
  return (x:optimizedRest)

------------------------------------------------------------------------------------------------------------------------
----------------------------- OPTIMIZE STMT ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
optimizeStmt :: Stmt -> Result (Stmt, Bool)
optimizeStmt (Ret exp) = do
  val <- evalConst exp
  return ((Ret val), True)
optimizeStmt (VRet) = return (VRet, True)
optimizeStmt (Empty) = return (Empty, False)
optimizeStmt (Decl t items) = do
  optimizedItems <- optimizeItems items
  return ((Decl t optimizedItems), False)
optimizeStmt (Ass i exp) = do
  val <- evalConst exp
  return ((Ass i val), False)
optimizeStmt (Cond exp stmt) = do
  val <- evalConst exp
  case val of
    (ELitFalse) -> return (Empty, False)
    (ELitTrue) -> do
      (optimizedStmt, hasRet) <- optimizeStmt stmt
      return (optimizedStmt, hasRet)
    _ -> do
      (optimizedStmt, hasRet) <- optimizeStmt stmt
      return ((Cond val optimizedStmt), False)
optimizeStmt (CondElse exp stmt1 stmt2) = do
  val <- evalConst exp
  case val of
    (ELitFalse) -> do
      (optimizedStmt, hasRet) <- optimizeStmt stmt2
      return (optimizedStmt, hasRet)
    (ELitTrue) -> do
      (optimizedStmt, hasRet) <- optimizeStmt stmt1
      return (optimizedStmt, hasRet)
    _ -> do
      (optimizedStmt1, hasRet1) <- optimizeStmt stmt1
      (optimizedStmt2, hasRet2) <- optimizeStmt stmt2
      return ((CondElse val optimizedStmt1 optimizedStmt2), hasRet1 && hasRet2)
optimizeStmt (While exp stmt) = do
  val <- evalConst exp
  case val of
    (ELitFalse) -> do
      return (Empty, False)
    _ -> do
      (optimizedStmt, _) <- optimizeStmt stmt
      return ((While val optimizedStmt), False)
optimizeStmt (SExp exp) = do
  val <- evalConst exp
  return ((SExp val), False)
optimizeStmt (BStmt block) = do
  (optimizedBlock, hasRet) <- optimizeBlock block
  return ((BStmt optimizedBlock), hasRet)
optimizeStmt s@(For t i exp stmt) = return (s, False)
optimizeStmt x = return (x, False)

------------------------------------------------------------------------------------------------------------------------
----------------------------- EVAL CONST EXP ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- Evaluating const expressions
evalConst :: Expr -> Result Expr
evalConst x@(EAdd exp1 op exp2) = do
  var1 <- evalConst exp1
  var2 <- evalConst exp2
  case var1 of
    (ELitInt i1) ->
      case var2 of
        (ELitInt i2) -> case op of
          Plus -> return (ELitInt (i1 + i2))
          Minus -> return (ELitInt (i1 - i2))
        _ -> return x
    (EString s1) ->
      case var2 of
        (EString s2) -> case op of
          Plus -> return (EString (s1 ++ s2))
          Minus -> throwError $ illegalOperation ((show Minus) ++ " on string")
        _ -> return x
    _ -> return x
evalConst x@(ERel exp1 op exp2) = do
  var1 <- evalConst exp1
  var2 <- evalConst exp2
  case var1 of
    (ELitInt i1) ->
      case var2 of
        (ELitInt i2) -> case op of
          LTH -> if i1 < i2 then return ELitTrue else return ELitFalse
          LE -> if i1 <= i2 then return ELitTrue else return ELitFalse
          GTH -> if i1 > i2 then return ELitTrue else return ELitFalse
          GE -> if i1 >= i2 then return ELitTrue else return ELitFalse
          EQU -> if i1 == i2 then return ELitTrue else return ELitFalse
          NE -> if (not (i1 == i2)) then return ELitTrue else return ELitFalse
        _ -> return x
    (ELitTrue) ->
      case var2 of
        (ELitFalse) -> case op of
          EQU -> return ELitFalse
          NE -> return ELitTrue
          _ -> throwError $ illegalOperation ((show op) ++ " on boolean")
        (ELitTrue) -> case op of
          EQU -> return ELitTrue
          NE -> return ELitFalse
          _ -> throwError $ illegalOperation ((show op) ++ " on boolean")
        _ -> return x
    (ELitFalse) ->
      case var2 of
        (ELitFalse) -> case op of
          EQU -> return ELitTrue
          NE -> return ELitFalse
          _ -> throwError $ illegalOperation ((show op) ++ " on boolean")
        (ELitTrue) -> case op of
          EQU -> return ELitFalse
          NE -> return ELitTrue
          _ -> throwError $ illegalOperation ((show op) ++ " on boolean")
        _ -> return x
    _ -> return x
evalConst x@(EAnd exp1 exp2) = do
  var1 <- evalConst exp1
  var2 <- evalConst exp2
  case var1 of
    (ELitTrue) ->
      case var2 of
        (ELitTrue) -> return ELitTrue
        (ELitFalse) -> return ELitFalse
        _ -> return exp2
    (ELitFalse) -> return ELitFalse
    _ -> return x
evalConst x@(EOr exp1 exp2) = do
  var1 <- evalConst exp1
  var2 <- evalConst exp2
  case var1 of
    (ELitTrue) -> return ELitTrue
    (ELitFalse) ->
      case var2 of
        (ELitTrue) -> return ELitTrue
        (ELitFalse) -> return ELitFalse
        _ -> return x
    _ -> return x
evalConst x@(Neg exp) = do
  var <- evalConst exp
  case var of
    (ELitInt i) -> return (ELitInt (0 - i))
    _ -> return x
evalConst x@(Not exp) = do
  var <- evalConst exp
  case var of
    (ELitTrue) -> return (ELitFalse)
    (ELitFalse) -> return (ELitTrue)
    _ -> return x
evalConst x = return x