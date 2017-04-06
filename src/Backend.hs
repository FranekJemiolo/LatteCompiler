-- This program was written by Franciszek Jemioło index number 346919
module Backend where

-- This module contains backend for the compiler in x86 assembly language

import AbsLatte
import ErrM
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List
import Prelude

------------------------------------------------------------------------------------------------------------------------
----------------------------- INTERNAL TYPES AND VARIABLES -------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
type Loc = Integer
type Label = String
type Register = String
type CodeBlockNum = Integer

data Val = ValVar Ident | ValReg Register | ValInt Integer | ValBool Bool | ValNull | ValString String

instance Show Val where
  show (ValVar (Ident x)) = "%" ++ x
  show (ValReg reg) = reg
  show (ValInt i) = show i
  show (ValBool True) = "true"
  show (ValBool False) = "false"
  show (ValNull) = "null"
  show (ValString s) = s

data Variable = Variable {
  _type :: Type,
  val :: Val,
  blockNum :: CodeBlockNum
}

instance Show Variable where
  show (Variable t v num) = case v of
    (ValVar (Ident x)) -> show v--"%" ++ x ++ (show num)
    _ -> show v

data CodeBlock = CodeBlock {
  label :: CodeBlockNum,
  code :: [LLVMCode]
} deriving Show

data MethodPtr = MethodPtr {
  methodName :: Ident,
  llvmMethodPtr :: Variable,
  methodType :: Type
} deriving Show

data MethodsVTable = MethodsVTable {
  methods :: [(Integer, MethodPtr)],
  vtablePointer :: Variable
} deriving Show

data Field = Field Ident Type deriving Show

data Class = Class {
  className :: Ident,
  parent :: Maybe Class,
  fields :: [(Integer, Field)],
  vtable :: MethodsVTable
} deriving Show

data Env = Env {
  -- Because there are no global variables (except strings)
  variables :: M.Map Ident Variable,
  funRetType :: Type,
  currentClass :: Maybe Class
} deriving Show

data Store = Store {
  functions :: M.Map Ident Type,
  blocks :: M.Map CodeBlockNum CodeBlock,
  currentBlockNum :: Integer,
  -- Maping String to constant integer to not generate many the same strings
  stringConstants :: M.Map String Integer,
  constCounter :: Integer,
  -- Label used for llvm - because both labels and register have to be unique
  labelCounter :: Integer,
  classes :: M.Map Ident Class,
  -- For temporary variables
  randomCounter :: Integer
} deriving Show


type Result a = ReaderT Env (ErrorT String (StateT Store IO)) a

------------------------------------------------------------------------------------------------------------------------
----------------------------- LLVM DATA TYPES AND SHOW FUNCTIONS -------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data BinOp = AddBinOp AddOp | MulBinOp MulOp | RelBinOp RelOp | AndBinOp | OrBinOp

data UnaryOp = UnaryNot

-- LLVM Code
data LLVMCode = AssignBin Variable BinOp Variable Variable |
  AssignUnary Variable UnaryOp Variable |
  Branch Label |
  CondBranch Variable Label Label |
  Call Variable Ident [Variable] |
  VoidCall Ident [Variable] |
  PtrCall Variable Variable [Variable] |
  PtrVoidCall Variable [Variable] |
  VoidReturn |
  Alloca Variable |
  Return Variable |
  Load Variable Variable |
  Phi Variable [Variable] |
  Bitcast Variable Variable |
  Getelementptr Variable Variable Variable |
  Getelementptr2 Variable Variable Variable |
  MemStore Variable Variable


instance Show BinOp where
  show (AddBinOp op) = show op
  show (MulBinOp op) = show op
  show (RelBinOp op) = show op
  show (AndBinOp) = "and"
  show (OrBinOp) = "or"

instance Show UnaryOp where
  show (UnaryNot) = "not"

------------------------------------------------------------------------------------------------------------------------
----------------------------- HELPER FUNCTIONS FOR PRINTING LLVM TYPES AND FUNCTION ARGS -------------------------------
------------------------------------------------------------------------------------------------------------------------
showType :: Type -> String
showType (Void) = "void"
showType (Int) = "i32"
showType (Str) = "i8*"
showType (Bool) = "i1"
showType (ArrType t) = "%array"
showType (Arr t i) = "[ " ++ (show i) ++ " x " ++ (showType t) ++ " ]"
showType (ClsType (Ident clsName)) = "%class." ++ clsName
showType (Pointer t) = (showType t) ++ "*"
showType (VTable (Ident clsName)) = "%class." ++ clsName ++ ".vtable"
showType (Fun retType args) = (showType retType) ++ " (" ++ (showFunTypeArgs args)

showFunTypeArgs :: [Type] -> String
showFunTypeArgs [] = ")"
showFunTypeArgs (t:t1:rest) = (showType t) ++ ", " ++ (showFunTypeArgs (t1:rest))
showFunTypeArgs (t:rest) = (showType t) ++ (showFunTypeArgs rest)

showFunArgs :: [Arg] -> String
showFunArgs [] = ")"
showFunArgs ((Arg t (Ident x)):y:xs) = (showType (t)) ++ " %" ++ x ++ ", " ++ (showFunArgs (y:xs))
showFunArgs ((Arg t (Ident x)):xs) = (showType (t)) ++ " %" ++ x ++ (showFunArgs xs)

dereferenceType :: Type -> Type
dereferenceType (Pointer t) = t
dereferenceType t = t

makeParamString :: [Variable] -> String
makeParamString [] = ")"
makeParamString (x:y:xs) = (showType (_type x)) ++ " " ++ (show x) ++ ", " ++ (makeParamString (y:xs))
makeParamString (x:xs) = (showType (_type x)) ++ " " ++ (show x) ++ (makeParamString xs)

builtInStringAppend :: Ident
builtInStringAppend = Ident "_appendString"

------------------------------------------------------------------------------------------------------------------------
----------------------------- SHOW LLVM CODE ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

printPhiVars :: [Variable] -> String
printPhiVars [] = ""
printPhiVars (x:y:xs) = "[ " ++ (show x) ++ ", " ++ ("%" ++ (show (blockNum x))) ++ " ], " ++ printPhiVars (y:xs)
printPhiVars (x:xs) = "[ " ++ (show x) ++ ", " ++ ("%" ++ (show (blockNum x))) ++ "] " ++ printPhiVars (xs)

instance Show LLVMCode where
  show (AssignBin resVar op var1 var2) = (show resVar) ++ " = " ++ (show op) ++ " " ++ (showType (_type var1)) ++
    " " ++ (show var1) ++ ", " ++ (show var2)
  show (AssignUnary resVar op var1) = case op of
    UnaryNot -> (show resVar) ++ " = xor " ++ (showType (_type var1)) ++ " true, " ++ (show var1)
  show (Branch lab) = "br label " ++ lab
  show (CondBranch var labTrue labFalse) = "br i1 " ++ (show var) ++ ", label " ++ labTrue ++
    ", label " ++ labFalse
  show (Call resVar (Ident i) vars) = (show resVar) ++ " = call " ++ (showType (_type resVar)) ++
    " @" ++ i ++ "(" ++ (makeParamString vars)
  show (VoidCall (Ident i) vars) = "call void @" ++ i ++ "(" ++ (makeParamString vars)
  show (PtrCall var fVar vars) = (show var) ++ " = call " ++ (showType (_type var)) ++ " " ++ (show fVar) ++ "(" ++
    (makeParamString vars)
  show (PtrVoidCall fVar vars) = "call void " ++ (show fVar) ++ "(" ++ (makeParamString vars)
  show (VoidReturn) = "ret void"
  show (Return var) = "ret " ++ (showType (_type var)) ++ " " ++ (show var)
  show (Alloca var) = (show var) ++ " = alloca " ++ (showType (dereferenceType (_type var)))
  show (Load resVar var) = (show resVar) ++ " = load " ++ (showType (_type resVar)) ++ ", " ++
    (showType (_type var)) ++ " " ++ (show var)
  show (Phi resVar vars) = (show resVar) ++ " = phi " ++ (showType (_type resVar)) ++ " " ++ printPhiVars vars
  show (Bitcast resVar var) = (show resVar) ++ " = bitcast " ++ (showType (_type var)) ++ " " ++ (show var) ++
    " to " ++ (showType (_type resVar))
  show (Getelementptr resVar var elemNum) = (show resVar) ++ " = getelementptr " ++
    (showType (dereferenceType (_type var))) ++ ", " ++ (showType (_type var)) ++ " " ++
    (show var) ++ ", i32 0, i32 " ++ (show elemNum)
  show (Getelementptr2 resVar var elemNum) = (show resVar) ++ " = getelementptr " ++
    (showType (dereferenceType (_type var))) ++ ", " ++ (showType (_type var)) ++ " " ++
    (show var) ++ ", i32 " ++ (show elemNum)
  show (MemStore var memVar) = "store " ++ (showType (_type var)) ++ " " ++ (show var) ++ ", " ++
    (showType (_type memVar)) ++ " " ++ (show memVar)

------------------------------------------------------------------------------------------------------------------------
----------------------------- ERROR MESSAGES ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
internalError :: String -> String
internalError x = "Internal Error: " ++ x

variableNotFound :: Ident -> String
variableNotFound (Ident x) = "Variable " ++ x ++ " not found"

------------------------------------------------------------------------------------------------------------------------
----------------------------- SETUP COMPILER FUNCTIONS -----------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
getInitialEnv :: Env
getInitialEnv = Env {
  variables = M.empty,
  funRetType = Void,
  currentClass = Nothing
}

builtInFunctions :: M.Map Ident Type
builtInFunctions = (M.fromList ([
  (Ident "printInt", (Fun Void [Int])),
  (Ident "printString", (Fun Void [Str])),
  (Ident "error", (Fun Void [])),
  (Ident "readInt", (Fun Int [])),
  (Ident "readString", (Fun Str []))
  ]))

getInitialStore :: Store
getInitialStore = Store {
  functions = builtInFunctions,
  blocks = M.empty,
  currentBlockNum = 0,
  stringConstants = M.insert "" 0 M.empty,
  constCounter = 1,
  labelCounter = 0,
  classes = M.empty,
  randomCounter = 0
}

------------------------------------------------------------------------------------------------------------------------
----------------------------- RUNNING COMPILATION ----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
runCompilation :: Store -> Env -> Result a -> IO (Either String a)
runCompilation store env result = do
  (res, _) <- runStateT (runErrorT (runReaderT result env)) store
  return res

------------------------------------------------------------------------------------------------------------------------
----------------------------- PRINTING LLVM CODE -----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
printLinesOfCode :: [LLVMCode] -> String
printLinesOfCode [] = ""
printLinesOfCode (x:xs) = "\t" ++ (show x) ++ "\n" ++ (printLinesOfCode xs)

-- Remove unnecessary lines of code from blocks after jumps or returns
clearCode :: [LLVMCode] -> Result [LLVMCode]
clearCode [] = return []
clearCode (x:xs) = case x of
  (VoidReturn) -> return [x]
  (Return _) -> return [x]
  (Branch _) -> return [x]
  (CondBranch _ _ _) -> return [x]
  _ -> do
    cleanedCode <- (clearCode xs)
    return (x:cleanedCode)

clearBlock :: CodeBlock -> Result CodeBlock
clearBlock (CodeBlock label code) = do
  cleanedCode <- clearCode code
  return (CodeBlock label cleanedCode)

clearBlocks :: [(Integer, CodeBlock)] -> Result [(Integer, CodeBlock)]
clearBlocks [] = return []
clearBlocks ((lab, b):rest) = do
  cleanedBlock <- clearBlock b
  cleanedRest <- clearBlocks rest
  return ((lab, (cleanedBlock)):(cleanedRest))

removeUnnecessaryJumps :: Result ()
removeUnnecessaryJumps = do
  st <- get
  cleanedBlocks <- clearBlocks (M.toList (blocks st))
  modify (\store -> store { blocks = (M.fromList cleanedBlocks) })
  return ()

------------------------------------------------------------------------------------------------------------------------
----------------------------- STANDARD LLVM PROLOG ---------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
getProlog :: Result String
getProlog = return ("; ModuleID = 'main.c'\n" ++
            "target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"\n " ++
            "target triple = \"x86_64-unknown-linux-gnu\"\n " ++
            "\n")

------------------------------------------------------------------------------------------------------------------------
----------------------------- PRINT LLVM STRING CONSTANTS --------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
printStringConstantList :: [(String, Integer)] -> String
printStringConstantList [] = ""
printStringConstantList ((str, i):rest) = "@.str" ++ (show i) ++ " = " ++
  "private unnamed_addr constant [" ++ (show ((length str) + 1)) ++
  " x i8] c\"" ++ str ++ "\\00\", align 1" ++ "\n" ++ (printStringConstantList rest)

printStringConstants :: Result String
printStringConstants = do
  consts <- gets stringConstants
  return $ printStringConstantList (M.toList consts)

------------------------------------------------------------------------------------------------------------------------
----------------------------- PRINT CLASSES TYPES ----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
printClassStructType :: [Type] -> String
printClassStructType [] = " }"
printClassStructType (t:t1:rest) = (showType t) ++ ", " ++ (printClassStructType (t1:rest))
printClassStructType (t:rest) = (showType t) ++ (printClassStructType rest)

-- Printing the types of the classes
printClassesTypesList :: [Class] -> String
printClassesTypesList [] = ""
printClassesTypesList (cls@(Class clsName@(Ident n) p f@(_:fs) m):rest) = ("%class." ++ n ++ " = type { " ++
  "%class." ++ n ++ ".vtable*, " ++
  (printClassStructType (map (\(_,(Field i t)) -> t) f)) ++ "\n\n" ++ (printClassesTypesList rest))
printClassesTypesList (cls@(Class clsName@(Ident n) p [] m):rest) = ("%class." ++ n ++ " = type { " ++
  "%class." ++ n ++ ".vtable* " ++ "}" ++ "\n\n" ++ (printClassesTypesList rest))

printClassesTypes :: Result String
printClassesTypes = do
  clss <- gets classes
  return $ printClassesTypesList (map (\(_, cls) -> cls) (M.toList clss))

------------------------------------------------------------------------------------------------------------------------
----------------------------- PRINT VTABLES ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
printVtable :: Ident -> MethodsVTable -> String
printVtable (Ident i) mvt = ("%class." ++ i ++ ".vtable" ++ " = type { " ++
  (printClassStructType (map (\(_,(MethodPtr nm mPtr mType)) -> mType) (methods mvt))) ++ "\n\n" ++
  "@class." ++ i ++ ".vtablePtr = global %class." ++ i ++ ".vtable* null")

printVtablesFromList :: [(Ident, MethodsVTable)] -> String
printVtablesFromList [] = ""
printVtablesFromList ((i,mvt):rest) = (printVtable i mvt) ++ "\n\n" ++ (printVtablesFromList rest)

printVtables :: Result String
printVtables = do
  clss <- gets classes
  return $ (printVtablesFromList (map (\(i,cls) -> (i, vtable cls)) (M.toList clss)))

------------------------------------------------------------------------------------------------------------------------
----------------------------- PRINT CLASSES TYPES DEFINITIONS ----------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
{-printClassesTypesDefinitions :: Result String
printClassesTypesDefinitions = do
  clss <- gets classes
  printClassTypeDef (M.toList clss)

printClassTypeDef :: [(Ident, Class)] -> Result String
printClassTypeDef [] = return ""
printClassTypeDef ((i, cls@(Class clsName parent fields vtable)):rest) = do
  -- print the class
  -- print the rest
  return ""-}

------------------------------------------------------------------------------------------------------------------------
----------------------------- PRINT BUILT INS --------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
printBuiltInFunctions :: Result String
printBuiltInFunctions = return ("declare void @printInt(i32)" ++ "\n\n" ++
  "declare void @printString(i8*)" ++ "\n\n" ++
  "declare void @error()" ++ "\n\n" ++
  "declare i32 @readInt()" ++ "\n\n" ++
  "declare i8* @readString()" ++ "\n\n" ++
  "declare i8* @_appendString(i8*, i8*)" ++ "\n\n" ++
  "declare i8* @malloc(i32)" ++ "\n\n")

printBuiltInArrayStruct :: Result String
printBuiltInArrayStruct = return ("%array = type { i8*, i32 }" ++ "\n\n")

------------------------------------------------------------------------------------------------------------------------
----------------------------- STANDARD LLVM EPILOG ---------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
getEpilog :: Result String
getEpilog = return ("\n" ++
                   "\n" ++
                   "attributes #0 = { nounwind uwtable \"disable-tail-calls\"=\"false\" " ++
                   "\"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" " ++
                   "\"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" " ++
                   "\"no-nans-fp-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\"" ++
                   "\"target-cpu\"=\"x86-64\" \"target-features\"=\"+sse,+sse2\" " ++
                   "\"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }\n " ++
                   "attributes #1 = { \"disable-tail-calls\"=\"false\" " ++
                   "\"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" " ++
                   "\"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" " ++
                   "\"no-nans-fp-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" " ++
                   "\"target-cpu\"=\"x86-64\" \"target-features\"=\"+sse,+sse2\" " ++
                   "\"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }\n " ++
                   "\n " ++
                   "!llvm.ident = !{!0}\n " ++
                   "\n " ++
                   "!0 = !{!\"clang version 3.7.0 (tags/RELEASE_370/final)\"}\n")

------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE LATTE PROGRAM TO LLVM ------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
convertToReference :: Type -> Type
convertToReference (ClsType ci) = (Pointer (ClsType ci))
convertToReference (ArrType t) = (Pointer (ArrType (convertToReference t)))
convertToReference t = t

changeToReferences :: TopDef -> Result TopDef
changeToReferences (TopFnDef (FnDef t i args block)) = return (TopFnDef (FnDef (convertToReference t) i
  (map (\(Arg t i) -> (Arg (convertToReference t) i)) args) block))
changeToReferences cls = return cls

compileProgram :: Program -> Result String
compileProgram (Program old_topDefs) = do
  topDefs <- mapM (changeToReferences) old_topDefs
  addTopDefs topDefs
  fillExtClasses topDefs
  compiledClasses <- compileClasses topDefs
  compiledFunctions <- compileFunctions topDefs
  -- TODO: Of course we need the prolog
  prolog <- getProlog
  strConsts <- printStringConstants
  builtInArrayStruct <- printBuiltInArrayStruct
  builtInFuns <- printBuiltInFunctions
  epilog <- getEpilog
  classesTypes <- printClassesTypes
  vtablesTypes <- printVtables
  return (prolog ++ "\n\n" ++ strConsts ++ "\n\n" ++ classesTypes ++ "\n\n" ++ vtablesTypes ++ "\n\n"  ++
    builtInArrayStruct ++ "\n\n" ++ builtInFuns ++ "\n\n" ++ compiledClasses ++ "\n\n" ++ compiledFunctions ++
    "\n\n" ++ epilog ++ "\n\n")

------------------------------------------------------------------------------------------------------------------------
----------------------------- ADDING FORWARD DECLARATIONS TO INTERNAL STATE --------------------------------------------
------------------------------------------------------------------------------------------------------------------------

addTopDef :: TopDef -> Result ()
addTopDef (TopFnDef (FnDef _type i args block)) = modify (\store -> store {
  functions = M.insert i (Fun ( _type)
   (map (\(Arg t i) -> (t)) args)) (functions store)
  })
addTopDef (ClassDef clsName@(Ident clsId) props) = modify (\store -> store {
  classes = M.insert clsName (Class {
    className = clsName,
    parent = Nothing,
    fields = createListWithIds (map (\(AttrProp t i) -> (Field i (convertToReference t))) (filter (\x -> case x of
      (AttrProp _ _) -> True
      (FnProp _) -> False) props)),
    vtable = MethodsVTable {
      methods = createListWithIds (map (\(FnProp (FnDef t i@(Ident nm) args block)) -> MethodPtr {
        methodName = i,
        llvmMethodPtr = (Variable {
          _type = (Pointer (Fun (convertToReference t)
            ((Pointer (ClsType clsName)):(map (\(Arg t i) -> convertToReference t) args)))),
          blockNum = 0,
          val = (ValReg ("@class." ++ clsId ++ ".vtable." ++ nm)) }),
        methodType = Pointer (Fun (convertToReference t)
          ((Pointer (ClsType clsName)):(map (\(Arg t i) -> convertToReference t) args)))
      }) (filter (\x -> case x of
        (AttrProp _ _) -> False
        (FnProp _) -> True) props)),
      vtablePointer = (Variable {
        _type = (Pointer (Pointer (VTable clsName))),
        blockNum = 0,
        val = (ValReg ("@class." ++ clsId ++ ".vtablePtr")) })
    }
  }) (classes store)
})
addTopDef (ClassExtDef clsName parentName props) = return ()

createListWithIds :: [a] -> [(Integer, a)]
createListWithIds xs = createListFrom 0 xs

createListFrom :: Integer -> [a] -> [(Integer, a)]
createListFrom _ [] = []
createListFrom i (x:xs) = ((i, x):(createListFrom (i + 1) xs))

addTopDefs :: [TopDef] -> Result ()
addTopDefs [] = return ()
addTopDefs (x:xs) = do
  addTopDef x
  addTopDefs xs

------------------------------------------------------------------------------------------------------------------------
----------------------------- FILL EXTENDED CLASSES, BUILD VTABLE FOR THEM ---------------------------------------------
------------------------------------------------------------------------------------------------------------------------
fillExtClass :: [TopDef] -> TopDef -> Result ()
fillExtClass [] (ClassExtDef clsName@(Ident clsId) parentName props) = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just _ -> return () -- Already defined
    Nothing ->
      case M.lookup parentName clss of
        Just parentCls -> do
          let parentFields = (fields parentCls)
          let parentVTable = (vtable parentCls)
          modify (\store -> store {
            classes = M.insert clsName (Class {
              className = clsName,
              parent = Just parentCls,
              fields = addFields parentFields (map (\(AttrProp t i) ->
                (Field i (convertToReference t))) (filter (\x -> case x of
                  (AttrProp _ _) -> True
                  (FnProp _) -> False) props)),
              vtable = ((mergeVTables parentVTable (map (\(FnProp (FnDef t i@(Ident nm) args block)) -> MethodPtr {
                methodName = i,
                llvmMethodPtr = (Variable {
                  _type = Pointer (Fun (convertToReference t)
                    ((Pointer (ClsType clsName)):(map (\(Arg t i) -> convertToReference t) args))),
                  blockNum = 0,
                  val = (ValReg ("@class." ++ clsId ++ ".vtable." ++ nm)) }),
                methodType = Pointer (Fun (convertToReference t)
                  ((Pointer (ClsType clsName)):(map (\(Arg t i) -> convertToReference t) args)))
                -- Adding pointer to self to arguments TODO: think whether this should be a pointer or just class type
              }) (filter (\x -> case x of
                (AttrProp _ _) -> False
                (FnProp _) -> True) props))) { vtablePointer = (Variable {
                  _type = (Pointer (Pointer (VTable clsName))),
                  blockNum = 0,
                  val = (ValReg ("@class." ++ clsId ++ ".vtablePtr")) }) })
            }) (classes store)
          })
        Nothing -> throwError $ internalError "Parent class not found"
fillExtClass rest cls@(ClassExtDef clsName parentName props) = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just _ -> return () -- Already defined
    Nothing ->
      case M.lookup parentName clss of
        Just parentCls -> fillExtClass [] cls
        Nothing -> do
          -- Do the rest
          fillExtClasses rest
          -- Check now if the parent classes exist
          fillExtClass [] cls
fillExtClass _ _ = return () -- Otherwise do nothing

fillExtClasses :: [TopDef] -> Result ()
fillExtClasses [] = return ()
fillExtClasses (x:xs) = do
  fillExtClass xs x
  fillExtClasses xs

addFields :: [(Integer, Field)] -> [Field] -> [(Integer, Field)]
addFields [] inserted = createListFrom 0 inserted
addFields fields inserted = fields ++ (createListFrom ((maxIndex fields) + 1) inserted)

maxIndex :: [(Integer, a)] -> Integer
maxIndex [] = 0
maxIndex xs = let (lastId, _) = last xs in lastId

insertOrReplace :: MethodPtr -> Integer -> [(Integer, MethodPtr)] -> [(Integer, MethodPtr)]
insertOrReplace ptr ptrId [] = [(ptrId, ptr)]
insertOrReplace ptr@(MethodPtr nm1 _ _)  _ ((i, xs@(MethodPtr nm2 _ _)):rest) = if nm1 == nm2 then ((i, ptr):rest)
  else ((i,xs):(insertOrReplace ptr (i + 1) (rest)))

mergeVTables :: MethodsVTable -> [MethodPtr] -> MethodsVTable
mergeVTables vtable [] = vtable
mergeVTables vtable (ptr:rest) = mergeVTables (vtable { methods = (insertOrReplace ptr 0 (methods vtable))}) rest

findFieldInList :: Ident -> [(Integer, Field)] -> Maybe (Integer, Field)
findFieldInList i [] = Nothing
findFieldInList i1 ((num, x@(Field i2 _)):rest) = if i1 == i2 then Just (num, x) else findFieldInList i1 rest

findMethodInList :: Ident -> [(Integer, MethodPtr)] -> Maybe (Integer, MethodPtr)
findMethodInList i [] = Nothing
findMethodInList i1 ((num, x@(MethodPtr i2 _ _)):rest) = if i1 == i2 then Just (num, x) else findMethodInList i1 rest

-- Finds field in class, returns it's index also
findField :: Ident -> Ident -> Result (Maybe (Integer, Field))
findField clsName fieldName = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just cls -> do
      case findFieldInList fieldName (fields cls) of
        Just (fieldId, field) -> return (Just (fieldId, field))
        Nothing -> return Nothing
    Nothing -> return Nothing

-- Finds method in class, returns it's index in vector also
findMethod :: Ident -> Ident -> Result (Maybe (Integer, MethodPtr))
findMethod clsName methodName = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just cls -> do
      case findMethodInList methodName (methods (vtable cls)) of
        Just (methodId, methodPtr) -> return (Just (methodId, methodPtr))
        Nothing -> return Nothing
    Nothing -> return Nothing

------------------------------------------------------------------------------------------------------------------------
----------------------------- HELPER FUNCTION FOR GETTING LABEL --------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
getLabel :: Result String
getLabel = do
  lab <- gets labelCounter
  modify (\store -> store { labelCounter = ((labelCounter store) + 1)})
  return ("%" ++ (show lab))

getNextBlockLabel :: Result String
getNextBlockLabel = do
  lab <- gets labelCounter
  modify (\store -> store { labelCounter = ((labelCounter store) + 1), currentBlockNum = lab,
    blocks = M.insert lab (CodeBlock { label = lab, code = [] }) (blocks store) })
  return ("%" ++ (show lab))

getNextBlockNum :: Result CodeBlockNum
getNextBlockNum = do
  lab <- gets labelCounter
  modify (\store -> store { labelCounter = ((labelCounter store) + 1), currentBlockNum = lab,
    blocks = M.insert lab (CodeBlock { label = lab, code = [] }) (blocks store) })
  return lab

------------------------------------------------------------------------------------------------------------------------
----------------------------- EMIT CODE --------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
emitCode :: LLVMCode -> Result ()
emitCode x = do
  store <- get
  let lab = currentBlockNum store in
    case M.lookup lab (blocks store) of
      Just block ->
        let added = ((code block) ++ [x]) in do
          modify (\store -> store {
            blocks = M.insert lab (CodeBlock { label = lab, code = added }) (blocks store)
          })
      Nothing -> modify (\store -> store {
        blocks = M.insert lab (CodeBlock { label = lab, code = [x] }) (blocks store)
      })

emitCodeInBlock :: CodeBlockNum -> LLVMCode -> Result ()
emitCodeInBlock blockNum x = do
  store <- get
  let lab = blockNum in
    case M.lookup lab (blocks store) of
      Just block ->
        let added = ((code block) ++ [x]) in do
          modify (\store -> store {
            blocks = M.insert lab (CodeBlock { label = lab, code = added }) (blocks store)
          })
      Nothing -> modify (\store -> store {
        blocks = M.insert lab (CodeBlock { label = lab, code = [x] }) (blocks store)
      })

------------------------------------------------------------------------------------------------------------------------
----------------------------- DEFAULT VALUES FOR VARIABLES -------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
defaultValue :: Type -> Val
defaultValue (Int) = ValInt 0
defaultValue (Str) = ValReg "getelementptr inbounds ([1 x i8], [1 x i8]* @.str0, i32 0, i32 0)"
defaultValue (Bool) = ValBool False
defaultValue _ = ValNull

------------------------------------------------------------------------------------------------------------------------
----------------------------- COUNTING SIZE OF TYPES -------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
getTypeSize :: Type -> Result Integer
getTypeSize (Int) = return 4
getTypeSize (Bool) = return 1
getTypeSize (Str) = return 8
getTypeSize (Pointer t) = return 8
getTypeSize (ClsType i) = do
  clss <- gets classes
  case M.lookup i clss of
    Just cls -> do
      -- Get fields size
      fieldSizes <- mapM (\(_, (Field i t)) -> getTypeSize t) (fields cls)
      let fieldsSize = sum fieldSizes in
        -- Size of the struct is fields size plus 8 bytes for pointer to vtable
        return (fieldsSize + 8)
    Nothing -> throwError $ internalError ("Class " ++ (show i) ++ " not found!")
getTypeSize (Arr t i) = do
  tSize <- getTypeSize t
  return (tSize * i + 4)
getTypeSize (ArrType t) = return 12
getTypeSize (VTable i) = do
  clss <- gets classes
  case M.lookup i clss of
    Just cls -> do
      let methodsSize = sum (map (\(_,(MethodPtr nm ptr mt)) -> 8) (methods (vtable cls)))
      return methodsSize
    Nothing -> throwError $ internalError ("Class " ++ (show i) ++ " not found!")
getTypeSize _ = undefined


------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE FUNCTIONS ------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
resetLabelCounter :: Result ()
resetLabelCounter = modify (\store -> store { labelCounter = 0, currentBlockNum = 0})

compileFunctions :: [TopDef] -> Result String
compileFunctions [] = return ""
compileFunctions ((TopFnDef fnDef):rest) = do
  -- Global functions is not in any class
  resetLabelCounter
  compiledCode <- local (\env -> env { currentClass = Nothing }) (compileFnDef fnDef)
  compiledRest <- compileFunctions rest
  return (compiledCode ++ "\n" ++ compiledRest)
compileFunctions (_:rest) = compileFunctions rest

------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE CLASSES --------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
compileProps :: TopDef -> [Property] -> Result String
compileProps _ [] = return ""
compileProps clsDef@(ClassDef clsName@(Ident clN) props) ((FnProp (FnDef retT i@(Ident fName) args block)):rest) = do
  clss <- gets classes
  case M.lookup clsName clss of
    Just cls -> do
      -- We will use a hack - we will create a function with added class name -
      -- (just some name mangling hoping nobody hacks us)
      let convertedArgs = (map (\(Arg t i) -> (Arg (convertToReference t) i)) args)
      let argsWithSelf = ((Arg (Pointer (ClsType clsName)) (Ident "self")):convertedArgs)
      let newFName = "class." ++ (clN) ++ ".vtable." ++ (fName)
      resetLabelCounter
      compiledFun <- (local (\env -> env { currentClass = Just cls })
        (compileFnDef (FnDef (convertToReference retT) (Ident newFName) argsWithSelf block)))
      compiledRest <- compileProps clsDef rest
      return (compiledFun ++ "\n\n" ++ compiledRest)
    Nothing -> throwError $ internalError "Class not found!"
compileProps clsDef@(ClassExtDef clsName@(Ident clN) parentName props)
  ((FnProp (FnDef retT i@(Ident fName) args block)):rest) = do
    clss <- gets classes
    case M.lookup clsName clss of
      Just cls -> do
        -- We will use a hack - we will create a function with added class name -
        -- (just some name mangling hoping nobody hacks us)
        let convertedArgs = (map (\(Arg t i) -> (Arg (convertToReference t) i)) args)
        let argsWithSelf = ((Arg (Pointer (ClsType clsName)) (Ident "self")):convertedArgs)
        let newFName = "class." ++ (clN) ++ ".vtable." ++ (fName)
        resetLabelCounter
        compiledFun <- (local (\env -> env { currentClass = Just cls })
          (compileFnDef (FnDef (convertToReference retT) (Ident newFName) argsWithSelf block)))
        compiledRest <- compileProps clsDef rest
        return (compiledFun ++ "\n\n" ++ compiledRest)
      Nothing -> throwError $ internalError "Class not found!"
compileProps clsDef (_:rest) = compileProps clsDef rest

compileClasses :: [TopDef] -> Result String
compileClasses [] = return ""
compileClasses (clsDef@(ClassDef clsName props):rest) = do
  compiledProps <- compileProps clsDef props
  compiledRest <- compileClasses rest
  return (compiledProps ++ "\n\n" ++ compiledRest)
compileClasses (clsDef@(ClassExtDef clsName parentName props):rest) = do
  compiledProps <- compileProps clsDef props
  compiledRest <- compileClasses rest
  return (compiledProps ++ "\n\n" ++ compiledRest)
compileClasses ((TopFnDef fndef):rest) = compileClasses rest

------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE FN DEF ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
removeBlocks :: Result ()
removeBlocks = modify (\store -> store { blocks = M.empty })

insertVTableMethods :: Ident -> Variable -> [(Integer, MethodPtr)] -> Result ()
insertVTableMethods _ _ [] = return ()
insertVTableMethods clsName@(Ident clsNm) vtablePtr ((methodId, methodPtr):rest) = do
  -- Get the pointer to the method ptr
  methodPtrLabel <- getLabel
  num <- gets currentBlockNum
  let mPtr = (Variable { _type = Pointer (methodType methodPtr), blockNum = num, val = (ValReg methodPtrLabel) })
  emitCode (Getelementptr mPtr vtablePtr (Variable { _type = Int, blockNum = num, val = (ValInt methodId) }))
  emitCode (MemStore (llvmMethodPtr methodPtr) mPtr)
  insertVTableMethods clsName vtablePtr rest

fillClassesVTable :: [(Ident, Class)] -> Result ()
fillClassesVTable [] = return ()
fillClassesVTable ((clsName@(Ident clsNm), cls):rest) = do
  -- Malloc the vtable
  tSize <- getTypeSize (VTable clsName)
  memPtrLabel <- getLabel
  num <- gets currentBlockNum
  let memPtr = (Variable { _type = Str, blockNum = num, val = (ValReg memPtrLabel)})
  emitCode (Call memPtr (Ident "malloc") [(Variable { _type = Int, blockNum = num, val = (ValInt tSize) })])
  vtablePtrLabel <- getLabel
  num <- gets currentBlockNum
  let vtablePtr = (Variable { _type = (Pointer (VTable clsName)), blockNum = num,
    val = (ValReg (vtablePtrLabel))})
  -- Cast the allocate memory to the vtable ptr
  emitCode (Bitcast vtablePtr memPtr)
  emitCode (MemStore vtablePtr (vtablePointer (vtable cls)))
  -- Ok now fill the vtablePtr
  insertVTableMethods clsName vtablePtr (methods (vtable cls))
  fillClassesVTable rest

fillVTables :: Result ()
fillVTables = do
  clss <- gets classes
  fillClassesVTable (M.toList clss)

compileFnDef :: FnDef -> Result String
compileFnDef (FnDef _type i@(Ident fName) args block) = do
  if fName == "main" then do
    -- In the main function we need first to setup the vtables for all the objects...
    startingBlockNum <- getNextBlockNum
    env' <- local (\env -> env { funRetType = _type }) (putArgs args)
    local (\_ -> env') (fillVTables)
    env'' <- local (\_ -> env') (compileBlock block)
    endBlockNum <- gets currentBlockNum
    removeUnnecessaryJumps
    -- Get the blocks from the graph
    blockList <- gatherBlocks [] startingBlockNum
    compiledBlocks <- printBlocks (sort blockList)
    removeBlocks
    return ("define " ++ (showType _type) ++ " @" ++ fName ++ "(" ++ (showFunArgs args) ++ " {\n" ++
      "\n" ++ compiledBlocks ++ "\n" ++ "}\n")
  else do
    -- Get label for the entry point block of function
    startingBlockNum <- getNextBlockNum
    env' <- local (\env -> env { funRetType = _type }) (putArgs args)
    env'' <- local (\_ -> env') (compileBlock block)
    endBlockNum <- gets currentBlockNum
    removeUnnecessaryJumps
    -- Get the blocks from the graph
    blockList <- gatherBlocks [] startingBlockNum
    compiledBlocks <- printBlocks (sort blockList)
    removeBlocks
    return ("define " ++ (showType _type) ++ " @" ++ fName ++ "(" ++ (showFunArgs args) ++ " {\n" ++
      "\n" ++ compiledBlocks ++ "\n" ++ "}\n")

putArg :: Arg -> Result Env
putArg (Arg t i) = do
  env <- ask
  -- Alloc space for the vars
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let allocaResult = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg registerLabel) })
  emitCode (Alloca allocaResult)
  emitCode (MemStore (Variable { _type = t, blockNum = num, val = (ValVar i)}) allocaResult)
  -- env' <- local (\_ -> env) (declareItem t (Init i (EVar i)))
  --return env'
  return $ env { variables = M.insert i allocaResult (variables env) }

putArgs :: [Arg] -> Result Env
putArgs [] = ask
putArgs (x:xs) = do
  env <- putArg x
  env' <- local (\_ -> env) (putArgs xs)
  return env'

-- Just simple dfs
gatherBlocksFromCode :: [CodeBlockNum] -> [LLVMCode] -> Result [CodeBlockNum]
gatherBlocksFromCode gathered [] = return gathered
gatherBlocksFromCode gathered ((VoidReturn):_) = return gathered
gatherBlocksFromCode gathered ((Return _):_) = return gathered
gatherBlocksFromCode gathered ((Branch (prefix:lab)):_) = do
  let labNum = read lab
  if labNum `elem` gathered then
    return gathered
  else
    gatherBlocks gathered labNum
gatherBlocksFromCode gathered ((CondBranch _ (p1:lab1) (p2:lab2)):_) = do
  let lab1Num = read lab1
  let lab2Num = read lab2
  if lab1Num `elem` gathered then
    if lab2Num `elem` gathered then
      return gathered
    else
      gatherBlocks gathered lab2Num
  else
    if lab2Num `elem` gathered then
      gatherBlocks gathered lab1Num
    else do
      from1 <- gatherBlocks gathered lab1Num
      if lab2Num `elem` from1 then
        return from1
      else
        gatherBlocks from1 lab2Num
gatherBlocksFromCode gathered (_:xs) = gatherBlocksFromCode gathered xs

gatherBlocks :: [CodeBlockNum] -> CodeBlockNum -> Result [CodeBlockNum]
gatherBlocks gathered num = do
  bls <- gets blocks
  case M.lookup num bls of
    Just cdBl@(CodeBlock lab code) -> gatherBlocksFromCode (num:gathered) code
    Nothing -> return gathered

printBlocks :: [CodeBlockNum] -> Result String
printBlocks [] = return ""
printBlocks (num:rest) = do
  compiledRest <- printBlocks rest
  bls <- gets blocks
  case M.lookup num bls of
    -- Check if every blocks end with voidreturn/return/br/br i1, if not add void return!
    -- Because other return types would be found in frontend.
    Just (CodeBlock lb code) -> case code of
      [] -> return (";<label>:" ++ (show lb) ++ ":\n" ++ (printLinesOfCode [VoidReturn]) ++ "\n\n" ++ compiledRest)
      (x:xs) -> let lastLn = last (x:xs) in case lastLn of
        (VoidReturn) -> return (";<label>:" ++ (show lb) ++ ":\n" ++ (printLinesOfCode code) ++ "\n\n" ++ compiledRest)
        (Return _) -> return (";<label>:" ++ (show lb) ++ ":\n" ++ (printLinesOfCode code) ++ "\n\n" ++ compiledRest)
        (Branch _) -> return (";<label>:" ++ (show lb) ++ ":\n" ++ (printLinesOfCode code) ++ "\n\n" ++ compiledRest)
        (CondBranch _ _ _) ->
          return (";<label>:" ++ (show lb) ++ ":\n" ++ (printLinesOfCode code) ++ "\n\n" ++ compiledRest)
        _ -> return (";<label>:" ++ (show lb) ++ ":\n" ++ (printLinesOfCode (code ++ [VoidReturn])) ++ "\n\n" ++ compiledRest)
    Nothing -> return compiledRest

getBlocks :: CodeBlockNum -> CodeBlockNum -> Result String
getBlocks from to = if from > to then return ""
  else do
    bls <- gets blocks
    case M.lookup from bls of
      Just (CodeBlock lb code) -> do
        compiledRest <- getBlocks (from + 1) to
        return (";<label>:" ++ (show lb) ++ ":\n" ++ (printLinesOfCode code) ++ "\n\n" ++ compiledRest)
      Nothing -> getBlocks (from + 1) to

------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE BLOCK ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
compileBlock :: Block -> Result Env
compileBlock (Block stmts) = do
  -- Jump to block
  num <- gets currentBlockNum
  startBlockLabel <- getNextBlockLabel
  emitCodeInBlock num (Branch startBlockLabel)
  compileStmts stmts
  ask

------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE STMTS ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
compileStmts :: [Stmt] -> Result Env
compileStmts [] = ask
compileStmts (x:xs) = do
  env <- compileStmt x
  local (\_ -> env) (compileStmts xs)

------------------------------------------------------------------------------------------------------------------------
----------------------------- DECLARATION OF ITEMS ---------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
declareItem :: Type -> Item -> Result Env
declareItem old_t (NoInit i) = do
  let t = convertToReference old_t
  -- Get new label for result
  registerLabel <- getLabel
  store <- get
  -- Alloca the result
  let allocaResult = (Variable { _type = (Pointer t), blockNum = (currentBlockNum store),
    val = (ValReg registerLabel) })
  emitCode (Alloca allocaResult)
  -- Store the default value
  emitCode (MemStore (Variable { _type = t, blockNum = (currentBlockNum store),
    val = defaultValue t }) allocaResult)
  env <- ask
  return $ env { variables = M.insert i allocaResult (variables env) }
declareItem old_t (Init i expr) = do
  let t = convertToReference old_t
  -- Calculate the expr
  resultVar <- compileExpr expr
  if t == (_type resultVar) then do
    -- Get new label for result
    registerLabel <- getLabel
    store <- get
    -- Alloca the result
    let allocaResult = (Variable { _type = (Pointer t), blockNum = (currentBlockNum store),
      val = (ValReg registerLabel) })
    emitCode (Alloca allocaResult)
    -- Store the result of the expression
    emitCode (MemStore resultVar allocaResult)
    env <- ask
    return $ env { variables = M.insert i allocaResult (variables env) }
  else do
    -- Cast the value
    castedLabel <- getLabel
    num <- gets currentBlockNum
    let castedPtr = (Variable { _type = t, blockNum = num, val = (ValReg castedLabel) })
    emitCode (Bitcast castedPtr resultVar)
    -- Get new label for result
    registerLabel <- getLabel
    store <- get
    -- Alloca the result
    let allocaResult = (Variable { _type = (Pointer t), blockNum = (currentBlockNum store),
      val = (ValReg registerLabel) })
    emitCode (Alloca allocaResult)
    -- Store the result of the expression
    emitCode (MemStore castedPtr allocaResult)
    env <- ask
    return $ env { variables = M.insert i allocaResult (variables env) }

declareItems :: Type -> [Item] -> Result Env
declareItems t [] = ask
declareItems t (item:rest) = do
  env <- declareItem t item
  env' <- local (\_ -> env) (declareItems t rest)
  return env'

------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE STMT -----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
compileStmt :: Stmt -> Result Env
compileStmt (Empty) = ask
compileStmt (BStmt block) = compileBlock block
compileStmt (Decl t items) = declareItems t items
compileStmt (Ass exp expr) = do
  lhs <- compileLvalue exp
  val <- compileExpr expr
  if (_type lhs) == (_type val) then do
    emitCode (MemStore val lhs)
    ask
  else do
    castedLabel <- getLabel
    num <- gets currentBlockNum
    let castedPtr = (Variable { _type = dereferenceType (_type lhs), blockNum = num, val = (ValReg castedLabel) })
    emitCode (Bitcast castedPtr val)
    emitCode (MemStore castedPtr lhs)
    ask
compileStmt (Incr exp) = compileStmt (Ass exp (EAdd exp Plus (ELitInt 1)))
compileStmt (Decr exp) = compileStmt (Ass exp (EAdd exp Minus (ELitInt 1)))
compileStmt (Ret expr) = do
  var <- compileExpr expr
  retT <- asks funRetType
  [casted] <- castIfNecessary [var] ([retT])
  emitCode (Return casted)
  ask
compileStmt (VRet) = do
  emitCode (VoidReturn)
  ask
compileStmt (Cond expr stmt) = do
  -- First generate the expression
  var <- compileExpr expr
  -- Now get current block num
  beforeBlockNum <- gets currentBlockNum
  -- Jump if true
  -- TODO: rewrite this for
  --emitCode (CondJump EQU var ConstTrue (beforeBlockNum + 1))
  --modify (\store -> store { currentBlockNum = (currentBlockNum store) + 1 })
  stmtBlockLabel <- getNextBlockLabel
  env <- compileStmt stmt
  -- Add the jump to end cond block to the after block
  endStmtBlockNum <- gets currentBlockNum
  afterBlockLabel <- getNextBlockLabel
  -- Add Necessary jump to after block to the before block
  emitCodeInBlock beforeBlockNum (CondBranch var stmtBlockLabel afterBlockLabel)
  emitCodeInBlock endStmtBlockNum (Branch afterBlockLabel)
  ask
compileStmt (CondElse expr stmt1 stmt2) = do
  -- First generate the expression
  var <- compileExpr expr
  -- Now save the current block num
  beforeBlockNum <- gets currentBlockNum
  -- Generating the true block
  ifTrueBlockLabel <- getNextBlockLabel
  env <- compileStmt stmt1
  afterTrueBlockNum <- gets currentBlockNum
  -- Generating the false block
  ifFalseBlockLabel <- getNextBlockLabel
  env' <- compileStmt stmt2
  afterFalseBlockNum <- gets currentBlockNum
  nextBlockLabel <- getNextBlockLabel
  emitCodeInBlock beforeBlockNum (CondBranch var ifTrueBlockLabel ifFalseBlockLabel)
  emitCodeInBlock afterTrueBlockNum (Branch nextBlockLabel)
  emitCodeInBlock afterFalseBlockNum (Branch nextBlockLabel)
  ask
compileStmt (While expr stmt) = do
  -- Get label
  beforeExpBlockNum <- gets currentBlockNum
  expBlockLabel <- getNextBlockLabel
  -- Jump to expr evaluation code
  emitCodeInBlock beforeExpBlockNum (Branch expBlockLabel)
  -- Generate expr evaluate code
  var <- compileExpr expr
  afterExpBlockNum <- gets currentBlockNum
  -- Generate jump if true to stmt code
  ifTrueBlockLabel <- getNextBlockLabel
  -- Generate stmt code
  env <- compileStmt stmt
  -- Generate jump to expr code
  emitCode (Branch expBlockLabel)
  nextBlockLabel <- getNextBlockLabel
  -- Add Jump from expr evaluation code to after while stmt
  emitCodeInBlock afterExpBlockNum (CondBranch var ifTrueBlockLabel nextBlockLabel)
  ask
compileStmt (SExp expr) = do
  var <- compileExpr expr
  ask
compileStmt (For t i exp stmt) = do
  counter <- gets randomCounter
  modify (\store -> store { randomCounter = ((randomCounter store) + 1)})
  compileBlock (Block [
    (Decl t [(NoInit i)]),
    (Decl Int [(Init (Ident ("temporaryVariableFor" ++ (show counter))) (ELitInt 0))]),
    (While (ERel (EVar (Ident ("temporaryVariableFor" ++ (show counter)))) (LTH) (EProp exp (Ident "length")))
      (BStmt (Block [Ass (EVar i) (EArrGet exp (EVar(Ident ("temporaryVariableFor" ++ (show counter))))),
        stmt,
        Incr (EVar (Ident ("temporaryVariableFor" ++ (show counter))))]
        )
       )
      )
    ]) -- This hack changes the for to a while on array all over it's length


------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE LVALUE ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
compileLvalue :: Expr -> Result Variable
compileLvalue (EVar i) = do
  vars <- asks variables
  currentCls <- asks currentClass
  case M.lookup i vars of
    Just var -> return var
    Nothing -> case currentCls of
      Just cls -> compileLvalue (EProp (EVar (Ident "self")) i)
      Nothing -> throwError $ internalError (variableNotFound i)
compileLvalue (EProp exp i) = do
  var <- compileLvalue exp
  case (_type var) of
    (Pointer (ClsType clsName)) -> do
      maybeField <- findField clsName i
      case maybeField of
        Just (fieldId, (Field _ t)) -> do
          fieldRegisterLabel <- getLabel
          num <- gets currentBlockNum
          let fieldRegister = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg fieldRegisterLabel) })
          emitCode (Getelementptr fieldRegister var (Variable { _type = Int, blockNum = num,
            val = (ValInt (fieldId + 1)) }))
          return fieldRegister
        Nothing -> throwError $ internalError "Field not found!"
    (Pointer (Pointer (ClsType clsName))) -> do
      loadedLabel <- getLabel
      num <- gets currentBlockNum
      let loadedVar = (Variable { _type = (Pointer (ClsType clsName)), blockNum = num, val = (ValReg loadedLabel) })
      emitCode (Load loadedVar var)
      maybeField <- findField clsName i
      case maybeField of
        Just (fieldId, (Field _ t)) -> do
          fieldRegisterLabel <- getLabel
          num <- gets currentBlockNum
          let fieldRegister = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg fieldRegisterLabel) })
          emitCode (Getelementptr fieldRegister loadedVar
            (Variable { _type = Int, blockNum = num, val = (ValInt (fieldId + 1)) }))
          return fieldRegister
        Nothing -> throwError $ internalError "Field not found!"
    _ -> throwError $ internalError ("In compile lvalue prop: " ++ (showType (_type var)) ++ " " ++ (show var))
compileLvalue (EArrGet exp1 exp2) = do
-- Get the expression as array
  arrStructPtr@(Variable (Pointer (ArrType t)) _ _) <- compileExpr exp1
  -- Load the array ptr from array struct ptr
  structRegisterLabel <- getLabel
  num <- gets currentBlockNum
  let arrPtr = (Variable { _type = (Pointer Str), blockNum = num, val = (ValReg structRegisterLabel)})
  emitCode (Getelementptr arrPtr (arrStructPtr)
    (Variable { _type = Int, blockNum = num, val = (ValInt 0) }))
  -- Load the pointer
  loadedLabel <- getLabel
  num <- gets currentBlockNum
  let loadedPtr = (Variable { _type = Str, blockNum = num, val = (ValReg loadedLabel) })
  emitCode (Load loadedPtr arrPtr)
  -- Now cast the pointer to the first element of array structure to array type of the t
  castedLabel <- getLabel
  num <- gets currentBlockNum
  let castedArrPtr = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg castedLabel) })
  emitCode (Bitcast castedArrPtr loadedPtr)
  -- Now get the pointer to the value from the array pointer
  indexVar <- compileExpr exp2
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let resultVar = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg registerLabel) })
  emitCode (Getelementptr2 resultVar (castedArrPtr) indexVar)
  return resultVar
compileLvalue x = throwError $ internalError ("Not an lvalue! " ++ (show x) )

------------------------------------------------------------------------------------------------------------------------
----------------------------- COMPILE EXPR -----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
setupClassInitialValues :: Variable -> Result ()
setupClassInitialValues clsPtr@(Variable (Pointer (ClsType i)) blNum val) = do
  clss <- gets classes
  case M.lookup i clss of
    Just cls ->
      -- Go through every field of class an init it's field
      setupClassFieldList clsPtr (fields cls)
    Nothing -> throwError $ internalError "Class not found!"
setupClassInitialValues _ = undefined

setupClassFieldList :: Variable -> [(Integer, Field)] -> Result ()
setupClassFieldList _ [] = return ()
setupClassFieldList clsPtr@(Variable (Pointer (ClsType i)) blNum val) ((fieldId, (Field fieldNm fieldT)):rest) = do
  -- Get element ptr to the field
  fieldPtrLabel <- getLabel
  num <- gets currentBlockNum
  let fieldPtr = (Variable { _type = Pointer fieldT, blockNum = num, val = (ValReg fieldPtrLabel) })
  emitCode (Getelementptr fieldPtr clsPtr (Variable { _type = Int, blockNum = num, val = (ValInt (fieldId + 1)) }))
  -- Store default value based on type
  emitCode (MemStore (Variable { _type = fieldT, blockNum = num, val = (defaultValue fieldT) }) fieldPtr)
  setupClassFieldList clsPtr rest

castIfNecessary :: [Variable] -> [Type] -> Result [Variable]
castIfNecessary [] [] = return []
castIfNecessary [] _ = throwError $ internalError "Wrong cast!"
castIfNecessary _ [] = throwError $ internalError "Wrong cast!"
castIfNecessary (var:rest) (t:tRest) =
  if (_type var) == t then do
    castedRest <- castIfNecessary rest tRest
    return (var:castedRest)
  else do
    castedLabel <- getLabel
    num <- gets currentBlockNum
    let castedPtr = (Variable { _type = t, blockNum = num, val = (ValReg castedLabel) })
    emitCode (Bitcast castedPtr var)
    castedRest <- castIfNecessary rest tRest
    return (castedPtr:castedRest)

initializeArray :: Variable -> Type -> Result ()
initializeArray = undefined


compileExpr :: Expr -> Result Variable
compileExpr (EVar i) = do
  vars <- asks variables
  currentCls <- asks currentClass
  case M.lookup i vars of
    Just var -> do
      registerLabel <- getLabel
      num <- gets currentBlockNum
      let loadedVar = (Variable { _type = (dereferenceType (_type var)), blockNum = num,
        val = (ValReg (registerLabel))})
      emitCode (Load loadedVar var)
      return loadedVar
    Nothing -> case currentCls of
      Just cls -> compileExpr (EProp (EVar (Ident "self")) i)
      Nothing -> throwError $ internalError (variableNotFound i)
compileExpr (ELitInt i) = do
  num <- gets currentBlockNum
  return (Variable { _type = Int, blockNum = num, val = (ValInt i)})
compileExpr (ELitTrue) =  do
  num <- gets currentBlockNum
  return (Variable { _type = Bool, blockNum = num, val = (ValBool True)})
compileExpr (ELitFalse) = do
  num <- gets currentBlockNum
  return (Variable { _type = Bool, blockNum = num, val = (ValBool False)})
compileExpr (EApp i exprs) = do
  funs <- gets functions
  case M.lookup i funs of
    Just (Fun retType params) -> do
      compiledVars <- mapM compileExpr exprs
      vars <- castIfNecessary compiledVars (params)
      case retType of
        (Void) -> do
          emitCode (VoidCall i vars)
          num <- gets currentBlockNum
          -- Return whatever - because no operation should be allowed after void call
          return Variable { _type = Void, blockNum = num, val = (ValNull)}
        _ -> do
          num <- gets currentBlockNum
          registerLabel <- getLabel
          let resultVar = (Variable { _type = retType, blockNum = num, val = (ValReg registerLabel) }) in do
            emitCode (Call resultVar i vars)
            return resultVar
    Nothing -> undefined
compileExpr (EString string) = do
  consts <- gets stringConstants
  case M.lookup string consts of
    Just reg -> do
      num <- gets currentBlockNum
      return (Variable { _type = Str, blockNum = num, val = (ValReg
        ("getelementptr inbounds ([" ++ (show ((length string) + 1)) ++ " x i8], [" ++ (show ((length string) + 1)) ++
        " x i8]* @.str" ++ (show reg) ++ ", i32 0, i32 0)"))})
    Nothing -> do
      constC <- gets constCounter
      modify (\store -> store {
        stringConstants = M.insert string constC (stringConstants store),
        constCounter = constC + 1
      })
      num <- gets currentBlockNum
      return (Variable { _type = Str, blockNum = num, val = (ValReg (
        ("getelementptr inbounds ([" ++ (show ((length string) + 1)) ++ " x i8], [" ++ (show ((length string) + 1)) ++
        " x i8]* @.str" ++ (show constC) ++ ", i32 0, i32 0)")))})
compileExpr (ENewCls t@(ClsType i@(Ident clsName))) = do
  clss <- gets classes
  case M.lookup i clss of
    Just cls -> do
      tSize <- getTypeSize t
      num <- gets currentBlockNum
      let typeSize = (Variable { _type = Int, blockNum = num, val = (ValInt tSize) })
      -- Alloc the size
      memPtrLabel <- getLabel
      num <- gets currentBlockNum
      let memPtr = (Variable { _type = Str, blockNum = num, val = (ValReg memPtrLabel) })
      emitCode (Call memPtr (Ident "malloc") [typeSize])
      -- Cast the ptr
      clsPtrLabel <- getLabel
      num <- gets currentBlockNum
      let clsPtr = (Variable { _type = Pointer (ClsType i), blockNum = num, val = (ValReg clsPtrLabel) })
      emitCode (Bitcast clsPtr memPtr)
      -- OK now should we setup the fields to the initial values
      setupClassInitialValues clsPtr
      -- And setup the vtable
      vtablePtrLabel <- getLabel
      num <- gets currentBlockNum
      let vtablePtr = (Variable { _type = Pointer (Pointer (VTable i)),
        blockNum = num, val = (ValReg vtablePtrLabel) })
      -- First element of every class is the vtable ptr
      emitCode (Getelementptr vtablePtr clsPtr (Variable { _type = Int, blockNum = num, val = (ValInt 0) }))
      loadedVtableLabel <- getLabel
      num <- gets currentBlockNum
      let loadedVtable = (Variable { _type = Pointer (VTable i), blockNum = num, val = (ValReg loadedVtableLabel) })
      emitCode (Load loadedVtable (vtablePointer (vtable cls)))
      -- Store the pointer
      emitCode (MemStore loadedVtable vtablePtr)
      return clsPtr
    Nothing -> throwError $ internalError "Class not found!"
compileExpr (ENewArr old_t exp) = do
  let t = convertToReference old_t
  -- First calculate the type size, and the total array size
  sizeVar <- compileExpr exp
  num <- gets currentBlockNum
  tSize <- getTypeSize t
  let typeSize = (Variable { _type = Int, blockNum = num, val = (ValInt tSize) })
  sizeLabel <- getLabel
  num <- gets currentBlockNum
  let memSize = (Variable { _type = Int, blockNum = num, val = (ValReg sizeLabel) })
  emitCode (AssignBin memSize (MulBinOp (Times)) sizeVar typeSize)
  -- Now we need to allocate a pointer to the array struct type of llvm CANNOT USE ALLOCA!
  memPtrLabel <- getLabel
  num <- gets currentBlockNum
  let memPtr = (Variable { _type = Str, blockNum = num, val = (ValReg memPtrLabel) })
  -- Size of ptr to struct of array is 8 + 4 = 12 bytes
  emitCode (Call memPtr (Ident "malloc") [(Variable { _type = Int, blockNum = num, val = (ValInt 12)})])
  arrayStructPtrLabel <- getLabel
  num <- gets currentBlockNum
  let arrayStructPtr = (Variable { _type = Pointer (ArrType t), blockNum = num, val = (ValReg arrayStructPtrLabel) })
  emitCode (Bitcast arrayStructPtr memPtr)
  --emitCode (Alloca arrayStructPtr)
  -- OK now get the memory for the array
  mallocLabel <- getLabel
  num <- gets currentBlockNum
  let mallocResult = (Variable { _type = Str, blockNum = num, val = (ValReg mallocLabel) })
  emitCode (Call mallocResult (Ident "malloc") [memSize])
  -- Great we now have the pointer to the allocated memory and the allocated struct, so now we should getelementptr
  -- of the first element of the struct, and then insert the memory there
  arrayPtrLabel <- getLabel
  num <- gets currentBlockNum
  let arrayPtr = (Variable { _type = Pointer Str, blockNum = num, val = (ValReg arrayPtrLabel) })
  emitCode (Getelementptr arrayPtr arrayStructPtr (Variable { _type = Int, blockNum = num, val = (ValInt 0) }))
  -- Yaaay, now store the malloc result
  emitCode (MemStore mallocResult arrayPtr)
  -- OK NOT SO FAST! WE SHOULD STORE ALSO THE LENGTH
  arraySizePtrLabel <- getLabel
  num <- gets currentBlockNum
  let arraySizePtr = (Variable { _type = Pointer Int, blockNum = num, val = (ValReg arraySizePtrLabel) })
  emitCode (Getelementptr arraySizePtr arrayStructPtr (Variable { _type = Int, blockNum = num, val = (ValInt 1) }))
  emitCode (MemStore sizeVar arraySizePtr)
  -- OK! GREAT! WE MANAGED! NOW RETURN ARRAY
  -- TODO: intialize array
  -- HA! But first we should initialize the array
  arrcounter <- gets randomCounter
  modify (\store -> store { randomCounter = ((randomCounter store) + 1)})
  registerLabel <- getLabel
  store <- get
  -- Alloca the result
  let allocaResult = (Variable { _type = (Pointer (Pointer (ArrType t))), blockNum = (currentBlockNum store),
    val = (ValReg registerLabel) })
  emitCode (Alloca allocaResult)
  -- Store the result of the expression
  emitCode (MemStore arrayStructPtr allocaResult)
  counter <- gets randomCounter
  modify (\store -> store { randomCounter = ((randomCounter store) + 1)})
  local (\env -> env { variables = M.insert (Ident
    ("tempArrayStructPtr" ++ (show arrcounter))) allocaResult (variables env) })
    (compileBlock (Block [
      (Decl Int [(Init (Ident ("tempCounterForLoop" ++ (show counter))) (ELitInt 0))]),
      (While (ERel (EVar (Ident ("tempCounterForLoop" ++ (show counter)))) (LTH)
        (EProp (EVar (Ident ("tempArrayStructPtr" ++ (show arrcounter)))) (Ident "length")))
        (BStmt (Block [
          (Decl t [(NoInit (Ident ("tempVar" ++ (show counter))))]),
          (Ass (EArrGet (EVar (Ident ("tempArrayStructPtr" ++ (show arrcounter))))
            (EVar(Ident ("tempCounterForLoop" ++ (show counter))))) (EVar (Ident ("tempVar" ++ (show counter))))),
          (Incr (EVar (Ident ("tempCounterForLoop" ++ (show counter)))))
          ]))
        )
      ]))
  return arrayStructPtr
compileExpr (EPropApp exp i exprs) = do
  var <- compileExpr exp
  case (_type var) of
    (Pointer (ClsType clsName)) -> do
      maybeMethod <- findMethod clsName i
      case maybeMethod of
        Just (methodId, method@(MethodPtr nm mPtr mType@(Pointer (Fun retT argsT)))) -> do
          compiledArgs <- mapM compileExpr exprs
          argVars <- castIfNecessary (var:compiledArgs) argsT
          -- Get the pointer to the vtable
          vtablePtrLabel <- getLabel
          num <- gets currentBlockNum
          let vtablePtr = (Variable { _type = (Pointer (Pointer (VTable clsName))), blockNum = num,
            val = (ValReg vtablePtrLabel) })
          emitCode (Getelementptr vtablePtr var (Variable { _type = Int, blockNum = num, val = (ValInt 0) }))
          -- Load the vtable
          loadedVtableLabel <- getLabel
          num <- gets currentBlockNum
          let loadedVtable = (Variable { _type = (Pointer (VTable clsName)), blockNum = num,
            val = (ValReg loadedVtableLabel) })
          emitCode (Load loadedVtable vtablePtr)
          -- Get the pointer to the method
          methodPtrLabel <- getLabel
          num <- gets currentBlockNum
          let methodPtr = (Variable { _type = (Pointer mType),
            blockNum = num, val = (ValReg methodPtrLabel) })
          emitCode (Getelementptr methodPtr loadedVtable (Variable { _type = Int, blockNum = num,
            val = (ValInt methodId) }))
          loadedMethodLabel <- getLabel
          num <- gets currentBlockNum
          let loadedMethod = (Variable { _type = mType, blockNum = num, val = (ValReg loadedMethodLabel) })
          emitCode (Load loadedMethod methodPtr)
          case retT of
            (Void) -> do
              emitCode (PtrVoidCall loadedMethod (argVars))
              return Variable { _type = Void, blockNum = num, val = (ValNull)}
            _ -> do
              resultLabel <- getLabel
              num <- gets currentBlockNum
              let resultVar = (Variable { _type = retT, blockNum = num, val = (ValReg resultLabel) })
              emitCode (PtrCall resultVar loadedMethod (argVars))
              return resultVar
        Nothing -> throwError $ internalError "Method not found"
    _ -> throwError $ internalError ("In compile expr epropapp: " ++ (showType (_type var)) ++ " " ++ (show var))
compileExpr (EProp exp i) = do
  var <- compileExpr exp
  case (_type var) of
    (Pointer (ArrType t)) -> do
      arrRegisterLabel <- getLabel
      num <- gets currentBlockNum
      let arrVar = (Variable { _type = (Pointer Int), blockNum = num, val = (ValReg arrRegisterLabel) })
      emitCode (Getelementptr arrVar (var)
        (Variable { _type = Int, blockNum = num, val = (ValInt 1)}))
      lenRegisterLabel <- getLabel
      num <- gets currentBlockNum
      let lenVar = (Variable { _type = Int, blockNum = num, val = (ValReg lenRegisterLabel) })
      emitCode (Load lenVar arrVar)
      return lenVar
    (Pointer (ClsType clsName)) -> do
      maybeField <- findField clsName i
      case maybeField of
        Just (fieldId, (Field _ t)) -> do
          fieldRegisterLabel <- getLabel
          num <- gets currentBlockNum
          let fieldRegister = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg fieldRegisterLabel) })
          emitCode (Getelementptr fieldRegister var (Variable { _type = Int, blockNum = num,
            val = (ValInt (fieldId + 1)) }))
          loadedLabel <- getLabel
          num <- gets currentBlockNum
          let loadedVar = (Variable { _type = t, blockNum = num, val = (ValReg loadedLabel) })
          emitCode (Load loadedVar fieldRegister)
          return loadedVar
        Nothing -> throwError $ internalError "Field not found!"
    _ -> throwError $ internalError ("In compile expr eprop: " ++ (showType (_type var)) ++ " " ++ (show var))
compileExpr (EArrGet exp1 exp2) = do
  -- Get the expression as array
  arrStructPtr@(Variable (Pointer (ArrType t)) _ _) <- compileExpr exp1
  -- Load the array ptr from array struct ptr
  structRegisterLabel <- getLabel
  num <- gets currentBlockNum
  let arrPtr = (Variable { _type = (Pointer Str), blockNum = num, val = (ValReg structRegisterLabel)})
  emitCode (Getelementptr arrPtr (arrStructPtr)
    (Variable { _type = Int, blockNum = num, val = (ValInt 0) }))
  -- Load the pointer
  loadedLabel <- getLabel
  num <- gets currentBlockNum
  let loadedPtr = (Variable { _type = Str, blockNum = num, val = (ValReg loadedLabel) })
  emitCode (Load loadedPtr arrPtr)
  -- Now cast the pointer to the first element of array structure to array type of the t
  castedLabel <- getLabel
  num <- gets currentBlockNum
  let castedArrPtr = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg castedLabel) })
  emitCode (Bitcast castedArrPtr loadedPtr)
  -- Now get the value from the pointer
  indexVar <- compileExpr exp2
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let resultVar = (Variable { _type = (Pointer t), blockNum = num, val = (ValReg registerLabel) })
  emitCode (Getelementptr2 resultVar (castedArrPtr) indexVar)
  -- OK, now just load the value
  elemRegisterLabel <- getLabel
  num <- gets currentBlockNum
  let elemVar = (Variable { _type = dereferenceType (_type resultVar), blockNum = num,
    val = (ValReg elemRegisterLabel) })
  emitCode (Load elemVar resultVar)
  return elemVar
compileExpr (ENullCast i) = do
  num <- gets currentBlockNum
  return (Variable { _type = Pointer (ClsType i), blockNum = num, val = (ValNull) })
compileExpr (Not expr) = do
  var <- compileExpr expr
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let resultVar = (Variable { _type = Bool, blockNum = num, val = (ValReg registerLabel) }) in do
    emitCode (AssignUnary resultVar UnaryNot var)
    return resultVar
compileExpr (Neg expr) = do
  var <- compileExpr expr
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let resultVar = (Variable { _type = Int, blockNum = num, val = (ValReg registerLabel) }) in do
    emitCode (AssignBin resultVar (AddBinOp Minus) (Variable { _type = Int, blockNum = num, val = (ValInt 0) }) var)
    return resultVar
compileExpr (EMul expr1 op expr2) = do
  var1 <- compileExpr expr1
  var2 <- compileExpr expr2
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let resultVar = (Variable { _type = Int, blockNum = num, val = (ValReg registerLabel) }) in do
    emitCode (AssignBin resultVar (MulBinOp op) var1 var2)
    return resultVar
compileExpr (EAdd expr1 op expr2) = do
  var1 <- compileExpr expr1
  var2 <- compileExpr expr2
  case (_type var1) of
    (Int) -> do
      registerLabel <- getLabel
      num <- gets currentBlockNum
      let resultVar = (Variable { _type = Int, blockNum = num, val = (ValReg registerLabel) }) in do
        emitCode (AssignBin resultVar (AddBinOp op) var1 var2)
        return resultVar
    (Str) -> do
      registerLabel <- getLabel
      num <- gets currentBlockNum
      let resultVar = (Variable { _type = Str, blockNum = num, val = (ValReg registerLabel) }) in do
        emitCode (Call resultVar (Ident "_appendString") [var1, var2])
        return resultVar
    _ -> undefined
compileExpr (ERel expr1 op expr2) = do
  var1 <- compileExpr expr1
  var2 <- compileExpr expr2
  num <- gets currentBlockNum
  registerLabel <- getLabel
  let resultVar = (Variable { _type = Bool, blockNum = num, val = (ValReg registerLabel) }) in do
    emitCode (AssignBin resultVar (RelBinOp op) var1 var2)
    return resultVar
compileExpr (EAnd expr1 expr2) = do
  var1 <- compileExpr expr1
  afterExpr1Num <- gets currentBlockNum
  expr2Label <- getNextBlockLabel
  var2 <- compileExpr expr2
  afterExpr2Num <- gets currentBlockNum
  afterBothLabel <- getNextBlockLabel
  emitCodeInBlock afterExpr1Num (CondBranch var1 expr2Label afterBothLabel)
  emitCodeInBlock afterExpr2Num (Branch afterBothLabel)
  -- We need the phi function for the SSA form
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let resultVar = (Variable { _type = Bool, blockNum = num, val = (ValReg registerLabel) })
  emitCode (Phi resultVar [var1, var2])
  return resultVar
compileExpr (EOr expr1 expr2) = do
  var1 <- compileExpr expr1
  afterExpr1Num <- gets currentBlockNum
  expr2Label <- getNextBlockLabel
  var2 <- compileExpr expr2
  afterExpr2Num <- gets currentBlockNum
  afterBothLabel <- getNextBlockLabel
  emitCodeInBlock afterExpr1Num (CondBranch var1 afterBothLabel expr2Label)
  emitCodeInBlock afterExpr2Num (Branch afterBothLabel)
  registerLabel <- getLabel
  num <- gets currentBlockNum
  let resultVar = (Variable { _type = Bool, blockNum = num, val = (ValReg registerLabel) })
  emitCode (Phi resultVar [var1, var2])
  return resultVar