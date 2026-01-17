{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Semantic Checker Engine
-}

module AST.Semantics.Check (checkAst, checkExpr, checkStmt) where

import Control.Monad (foldM, unless, forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as DT
import Data.List (isPrefixOf)

import AST.Ast (Ast(..))
import AST.Semantics.Type
import AST.Semantics.CheckCall (checkCall)

-- | Main entry point: validates a list of AST nodes.
checkAst :: [Ast] -> Either String ()
checkAst asts = case foldM checkStmt emptyEnv asts of
    Left err -> Left err
    Right _  -> Right ()

-- | Helper to insert a variable into the environment
insertVar :: CheckEnv -> DT.Text -> Type -> Either String CheckEnv
insertVar env name t =
    let newVars = Map.insert name t (envVars env)
    in Right $ env { envVars = newVars }

-- Helper to check a sequence
checkBlock :: CheckEnv -> [Ast] -> Either String Type
checkBlock _ [] = Right TyVoid
checkBlock env [x] = checkExpr env x
checkBlock env (x:xs) = do
    _ <- checkExpr env x
    checkBlock env xs

-- | Check Expression: Verifies an expression and returns its Semantic Type.
checkExpr :: CheckEnv -> Ast -> Either String Type
checkExpr _ (AInteger _) = Right TyInt
checkExpr _ (ABool _)    = Right TyBool
checkExpr _ AVoid        = Right TyVoid
checkExpr _ (ABlock []) = Right TyVoid
checkExpr env (ASymbol name) = checkSymbol env name
checkExpr env (AIf c t e)    = checkIf env c t e
checkExpr env (ASetStruct name fields) =
    checkStructInstantiation env name fields
checkExpr env (ACall func args) = checkCall checkExpr env func args
checkExpr env (AAccessStruct obj field) = checkStructAccess env obj field
checkExpr env (AList list) = checkListExpr env list
checkExpr env (ABlock xs) = checkBlock env xs
checkExpr env (APos line _ ast) =
    case checkExpr env ast of
        Left err -> 
            if "Error line" `isPrefixOf` err 
                then Left err
                else Left $ "Error line " ++ show line ++ ": " ++ err
        Right t  -> Right t
checkExpr _ _ = Left "Error: Expression type not supported"

-- | Symbol lookup
checkSymbol :: CheckEnv -> DT.Text -> Either String Type
checkSymbol env name = case Map.lookup name (envVars env) of
    Just t  -> Right t
    Nothing -> Left $ "Undefined variable '" ++ DT.unpack name ++ "'"

-- | Validates that a specific field exists within a defined structure.
--
-- @args
--   - env: The current checking environment containing struct definitions.
--   - sName: The name of the structure type to check against.
--   - field: The name of the field to look up.
--
-- @return
--   The 'Type' of the field if it exists, or an error message if the 
--   struct is undefined or the field is missing.
validateStructField :: CheckEnv -> DT.Text -> DT.Text -> Either String Type
validateStructField env sName field = do
    def <- getStructDef env sName
    case Map.lookup field (structFields def) of
        Just t -> Right t
        Nothing -> Left $ "Field '" ++ DT.unpack field ++ 
                          "' not found in struct '" ++ 
                          DT.unpack sName ++ "'"

-- | Validates a structure member access expression (e.g., 'obj.field').
--
-- @args
--   - env: The current checking environment.
--   - obj: The AST node representing the object being accessed.
--   - field: The name of the field to access.
--
-- @details
--   First, it verifies that the 'obj' expression evaluates to a structure type (`TyStruct`).
--   Then, it delegates to 'validateStructField' to ensure the field exists 
--   within that specific structure definition.
--
-- @return
--   The semantic 'Type' of the accessed field, or an error if the object 
--   is not a structure or the field is invalid.
checkStructAccess :: CheckEnv -> Ast -> DT.Text -> Either String Type
checkStructAccess env obj field = do
    typeObj <- checkExpr env obj
    case typeObj of
        TyStruct name -> validateStructField env name field
        _ -> Left $ "Cannot access field '" ++ DT.unpack field ++ 
                    "' on non-struct type " ++ typeToString typeObj

-- | IF logic
checkIf :: CheckEnv -> Ast -> Ast -> Ast -> Either String Type
checkIf env cond thenB elseB = do
    tCond <- checkExpr env cond
    case tCond of
        TyBool -> checkBranches env thenB elseB
        TyInt  -> checkBranches env thenB elseB
        _ -> Left "Error: 'if' condition must be boolean"

-- | Loop logic (While)
checkLoop :: CheckEnv -> Ast -> Ast -> Either String CheckEnv
checkLoop env cond body = do
    tCond <- checkExpr env cond
    case tCond of
        TyBool -> checkStmt env body
        TyInt  -> checkStmt env body
        _ -> Left "Error: Loop condition must be boolean or integer"

-- | Loop logic (For)
checkFor :: CheckEnv -> Ast -> Ast -> Ast -> Ast -> Either String CheckEnv
checkFor env initStmt cond updateStmt body = do
    envAfterInit <- checkStmt env initStmt
    tCond <- checkExpr envAfterInit cond
    case tCond of
        TyBool -> Right ()
        TyInt  -> Right ()
        _ -> Left "Error: 'for' condition must be boolean or integer"
    _ <- checkStmt envAfterInit updateStmt
    _ <- checkStmt envAfterInit body
    Right env

checkReturns :: CheckEnv -> Type -> Ast -> Either String ()
checkReturns env expected (AReturn expr) = do
    actual <- checkExpr env expr
    if areTypesCompatible expected actual
        then Right ()
        else Left $ "Return type mismatch: expected " ++
            typeToString expected ++ " but got " ++ typeToString actual
checkReturns env expected (AList list) =
    mapM_ (checkReturns env expected) list
checkReturns env expected (ABlock list) =
    mapM_ (checkReturns env expected) list
checkReturns env expected (AIf _ t e) =
    checkReturns env expected t >> checkReturns env expected e
checkReturns env expected (AWhile _ body) = checkReturns env expected body
checkReturns env expected (AFor _ _ _ body) = checkReturns env expected body
checkReturns env expected (APos _ _ ast) = checkReturns env expected ast
checkReturns _ _ _ = Right ()

-- | Branches comparison
checkBranches :: CheckEnv -> Ast -> Ast -> Either String Type
checkBranches env thenB elseB = do
    tThen <- checkExpr env thenB
    tElse <- checkExpr env elseB
    if areTypesCompatible tThen tElse
        then Right tThen
        else Left $ "Type mismatch in 'if' branches (" ++
                    typeToString tThen ++ " vs " ++
                    typeToString tElse ++ ")"

-- | Check Statement: Verifies a statement and returns the updated environment.
checkStmt :: CheckEnv -> Ast -> Either String CheckEnv
checkStmt env (ADefineStruct name fields) = defineStruct env name fields
checkStmt env (ASetVar name typeStr expr) = checkSetVar env name typeStr expr
checkStmt env (ADefineFunc name args ret body) =
    defineFunc env name args ret body
checkStmt env (AWhile cond body) = checkLoop env cond body
checkStmt env (AFor i c u b) = checkFor env i c u b
checkStmt env (APos line _ ast) = 
    case checkStmt env ast of
        Left err -> 
            if "Error line" `isPrefixOf` err
                then Left err
                else Left $ "Error line " ++ show line ++ ": " ++ err
        Right newEnv -> Right newEnv
checkStmt env _ = Right env

-- | Define a new structure in the environment.
-- Handles parsing of field types and duplicate checks.
defineStruct :: CheckEnv -> DT.Text -> [(DT.Text, DT.Text)] -> Either String CheckEnv
defineStruct env name fields =
    case Map.lookup name (envStructs env) of
        Just _ -> Left $ "Struct '" ++ DT.unpack name ++ "' is already defined"
        Nothing ->
            let parsedFields = map (\(n, t) -> (n, parseType t)) fields
                fieldsMap = Map.fromList parsedFields
                newDef = StructDef name fieldsMap
                newStructs = Map.insert name newDef (envStructs env)
            in Right $ env { envStructs = newStructs }

-- | Variable assignment
checkSetVar :: CheckEnv -> DT.Text -> DT.Text -> Ast -> Either String CheckEnv
checkSetVar env name typeStr expr = do
    let declaredType = parseType typeStr
    actualType <- checkExpr env expr
    applyAssignment env name declaredType actualType

-- | Define a new function in the environment.
--
-- Parses argument types and return type.
-- Adds the function to the environment (enabling recursion).
-- Creates a new scope with arguments bound.
-- Validates the function body within this new scope.
defineFunc :: CheckEnv -> DT.Text -> [(DT.Text, DT.Text)]
           -> DT.Text -> Ast -> Either String CheckEnv
defineFunc env name args retType body = do
    let argTypes = map (parseType . snd) args
    let retTy = parseType retType
    let funcType = TyFunc argTypes retTy
    envWithFunc <- insertVar env name funcType
    envForBody <- foldM (\e (n, t) -> insertVar e n (parseType t))
                        envWithFunc args
    _ <- checkStmt envForBody body
    _ <- checkReturns envForBody retTy body
    Right envWithFunc

-- | Applies the assignment logic based on types compatibility
applyAssignment :: CheckEnv -> DT.Text -> Type -> Type -> Either String CheckEnv
applyAssignment env name TyAuto actual = insertVar env name actual
applyAssignment env name declared actual
    | areTypesCompatible declared actual = insertVar env name declared
    | otherwise = Left $ "Variable '" ++ DT.unpack name ++ 
                         "' declared as " ++ typeToString declared ++ 
                         " but assigned " ++ typeToString actual

-- | Validates the instantiation of a structure (e.g. new Point {x: 1})
checkStructInstantiation :: CheckEnv -> DT.Text -> [(DT.Text, Ast)] -> Either String Type
checkStructInstantiation env name args = do
    def <- getStructDef env name
    let expected = structFields def
    let providedNames = map fst args
    let expectedNames = Map.keys expected

    checkExtraFields name expectedNames providedNames
    checkMissingFields name expectedNames providedNames
    mapM_ (validateField env expected) args
    Right (TyStruct name)

-- | Retrieves the struct definition
getStructDef :: CheckEnv -> DT.Text -> Either String StructDef
getStructDef env name = case Map.lookup name (envStructs env) of
    Just d -> Right d
    Nothing -> Left $ "Error: Undefined struct '" ++ 
                      DT.unpack name ++ "'"

-- | Checks for unknown fields provided in instantiation
checkExtraFields :: DT.Text -> [DT.Text] -> [DT.Text] -> Either String ()
checkExtraFields name expected provided =
    let unknown = filter (`notElem` expected) provided
    in forM_ (take 1 unknown) $ \u ->
        Left $ "Error: Unknown field '" ++ DT.unpack u ++ 
                  "' in struct '" ++ DT.unpack name ++ "'"

-- | Checks for missing mandatory fields
checkMissingFields :: DT.Text -> [DT.Text] -> [DT.Text] -> Either String ()
checkMissingFields name expected provided =
    let missing = filter (`notElem` provided) expected
    in forM_ (take 1 missing) $ \m ->
        Left $ "Error: Missing field '" ++ DT.unpack m ++ 
                  "' in construction of '" ++ DT.unpack name ++ "'"

-- | Validates the type of a single field expression
validateField :: CheckEnv -> Map.Map DT.Text Type -> (DT.Text, Ast) -> Either String ()
validateField env expectedFields (fieldName, expr) = do
    actualType <- checkExpr env expr
    let expectedType = expectedFields Map.! fieldName
    
    unless (areTypesCompatible expectedType actualType) $
        Left $ "Error: Field '" ++ DT.unpack fieldName ++ 
               "' expected " ++ typeToString expectedType ++ 
               " but got " ++ typeToString actualType

-- | Checks for list/strings
checkListExpr :: CheckEnv -> [Ast] -> Either String Type
checkListExpr _ [] = Right (TyList TyVoid)
checkListExpr env (x:_) = do
    elemType <- checkExpr env x
    return (TyList elemType)
