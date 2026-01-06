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

import AST.Ast (Ast(..))
import AST.Semantics.Type

-- | Checks if two types are semantically compatible.
-- Replaces the usage of (==) with explicit domain logic.
areTypesCompatible :: Type -> Type -> Bool
areTypesCompatible TyInt TyInt   = True
areTypesCompatible TyBool TyBool = True
areTypesCompatible TyVoid TyVoid = True
areTypesCompatible TyAuto TyAuto = True
areTypesCompatible (TyList a) (TyList b) = areTypesCompatible a b
areTypesCompatible (TyStruct a) (TyStruct b) = a == b
areTypesCompatible (TyFunc args1 ret1) (TyFunc args2 ret2) = 
    length args1 == length args2 &&
    and (zipWith areTypesCompatible args1 args2) &&
    areTypesCompatible ret1 ret2
areTypesCompatible _ _ = False

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

-- | Check Expression: Verifies an expression and returns its Semantic Type.
checkExpr :: CheckEnv -> Ast -> Either String Type
checkExpr _ (AInteger _) = Right TyInt
checkExpr _ (ABool _)    = Right TyBool
checkExpr _ AVoid        = Right TyVoid
checkExpr env (ASymbol name) = checkSymbol env name
checkExpr env (AIf c t e)    = checkIf env c t e
checkExpr env (ASetStruct name fields) =
    checkStructInstantiation env name fields
checkExpr _ _ = Left "Error: Expression type not supported"

-- | Symbol lookup
checkSymbol :: CheckEnv -> DT.Text -> Either String Type
checkSymbol env name = case Map.lookup name (envVars env) of
    Just t  -> Right t
    Nothing -> Left $ "Undefined variable '" ++ DT.unpack name ++ "'"

-- | IF logic
checkIf :: CheckEnv -> Ast -> Ast -> Ast -> Either String Type
checkIf env cond thenB elseB = do
    tCond <- checkExpr env cond
    case tCond of
        TyBool -> checkBranches env thenB elseB
        _ -> Left "Error: 'if' condition must be boolean"

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
checkExtraFields name expected provided = do
    let unknown = filter (`notElem` expected) provided
    forM_ (take 1 unknown) $ \u ->
        Left $ "Error: Unknown field '" ++ DT.unpack u ++ 
                  "' in struct '" ++ DT.unpack name ++ "'"
-- | Checks for missing mandatory fields
checkMissingFields :: DT.Text -> [DT.Text] -> [DT.Text] -> Either String ()
checkMissingFields name expected provided = do
    let missing = filter (`notElem` provided) expected
    forM_ (take 1 missing) $ \m ->
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
