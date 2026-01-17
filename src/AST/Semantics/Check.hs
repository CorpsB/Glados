{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Semantic Checker Engine with AST Transformation (Type Inference)
-}

module AST.Semantics.Check (checkAst, checkExpr, checkStmt) where

import Control.Monad (foldM, unless, forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as DT
import Data.List (isPrefixOf)

import AST.Ast (Ast(..))
import AST.Semantics.Type
import AST.Semantics.CheckCall (checkCall)

-- | Main entry point: validates AND transforms a list of AST nodes.
--
-- This function iterates over the top-level AST nodes, threading the
-- environment state. It returns a new AST where 'auto' types have been
-- resolved to concrete types.
--
-- @param asts: The list of raw AST nodes from the parser.
-- @return: Either an error message or the transformed AST list.
checkAst :: [Ast] -> Either String [Ast]
checkAst asts = do
    (finalAsts, _) <- foldM processNode ([], emptyEnv) asts
    return (reverse finalAsts)
  where
    -- acc = Accumulator (list of processed nodes)
    -- env = Current Environment state
    processNode (acc, env) node = do
        (newNode, newEnv) <- checkStmt env node
        return (newNode : acc, newEnv)

-- | Helper to insert a variable into the environment.
-- Used to update the scope with new variable definitions.
insertVar :: CheckEnv -> DT.Text -> Type -> Either String CheckEnv
insertVar env name t =
    let newVars = Map.insert name t (envVars env)
    in Right $ env { envVars = newVars }

-- | Check Expression: Verifies an expression and returns its Semantic Type.
--
-- Does not modify the AST, only extracts the type.
-- Handles literals (Int, Bool), variables (Symbol), and complex structures.
checkExpr :: CheckEnv -> Ast -> Either String Type
checkExpr _ (AInteger _) = Right TyInt
checkExpr _ (ABool _)    = Right TyBool
checkExpr _ AVoid        = Right TyVoid
checkExpr _ (ABlock []) = Right TyVoid
checkExpr env (ASymbol name) = checkSymbol env name
checkExpr env (AIf c t e)    = checkIfExpr env c t e
checkExpr env (ASetStruct name fields) =
    checkStructInstantiation env name fields
checkExpr env (ACall func args) = checkCall checkExpr env func args
checkExpr env (AAccessStruct obj field) = checkStructAccess env obj field
checkExpr env (AList list) = checkListExpr env list
checkExpr env (ABlock xs) = checkBlockExpr env xs
checkExpr env (APos line _ ast) =
    case checkExpr env ast of
        Left err -> 
            if "Error line" `isPrefixOf` err 
                then Left err
                else Left $ "Error line " ++ show line ++ ": " ++ err
        Right t  -> Right t
checkExpr _ _ = Left "Error: Expression type not supported"

-- | Helper to check a block used as an expression.
-- Returns the type of the *last* instruction in the block.
checkBlockExpr :: CheckEnv -> [Ast] -> Either String Type
checkBlockExpr _ [] = Right TyVoid
checkBlockExpr env [x] = checkExpr env x
checkBlockExpr env (x:xs) = do
    _ <- checkExpr env x
    checkBlockExpr env xs

-- | Looks up a variable symbol in the environment.
checkSymbol :: CheckEnv -> DT.Text -> Either String Type
checkSymbol env name = case Map.lookup name (envVars env) of
    Just t  -> Right t
    Nothing -> Left $ "Undefined variable '" ++ DT.unpack name ++ "'"

-- | Check Statement: Verifies AND transforms a statement.
--
-- Dispatcher function that routes AST nodes to specific handlers.
-- Returns a tuple containing the transformed AST node (with inferred types)
-- and the updated environment.
--
-- @param env: Current Semantic Environment.
-- @param node: AST node to process.
checkStmt :: CheckEnv -> Ast -> Either String (Ast, CheckEnv)
checkStmt env (ASetVar name typeStr expr) =
    processSetVar env name typeStr expr
checkStmt env (ADefineFunc name args ret body) =
    processFuncDef env name args ret body
checkStmt env (AIf c t e) = processIfStmt env c t e
checkStmt env (AWhile c body) = processWhileStmt env c body
checkStmt env (AFor i c u b) = processForLoop env i c u b
checkStmt env (AList list) = do
    (newList, newEnv) <- foldM processList ([], env) list
    return (AList (reverse newList), newEnv)
  where
    processList (acc, e) node = do
        (newNode, nEnv) <- checkStmt e node
        return (newNode : acc, nEnv)
checkStmt env (ABlock list) = do
    (newList, _) <- foldM processList ([], env) list
    return (ABlock (reverse newList), env)
  where
    processList (acc, e) node = do
        (newNode, nEnv) <- checkStmt e node
        return (newNode : acc, nEnv)
checkStmt env (APos l c ast) = 
    case checkStmt env ast of
        Left err -> 
            if "Error line" `isPrefixOf` err
                then Left err
                else Left $ "Error line " ++ show l ++ ": " ++ err
        Right (newAst, newEnv) -> Right (APos l c newAst, newEnv)
checkStmt env (ADefineStruct name fields) = do
    newEnv <- defineStruct env name fields
    return (ADefineStruct name fields, newEnv)
checkStmt env (AReturn expr) = do
    _ <- checkExpr env expr
    return (AReturn expr, env)
checkStmt env (AImport _) = Right (AVoid, env)
checkStmt env ast = do
    _ <- checkExpr env ast
    return (ast, env)

-- | Helper to validate boolean/int condition for loops/if.
--
-- @param c: The Condition AST to check.
-- @param context: String name of the structure (e.g. "if", "while").
validateCondition :: CheckEnv -> Ast -> String -> Either String ()
validateCondition env c context = do
    tCond <- checkExpr env c
    unless (areTypesCompatible TyBool tCond ||
        areTypesCompatible TyInt tCond) $
        Left $ "Error: '" ++ context ++
               "' condition must be boolean or integer"

-- | Process variable assignment (handles auto).
-- Calculates the actual type of the expression and replaces 'auto'.
processSetVar :: CheckEnv -> DT.Text -> DT.Text -> Ast ->
    Either String (Ast, CheckEnv)
processSetVar env name typeStr expr = do
    actualType <- checkExpr env expr
    let resolvedTypeStr = resolveAuto typeStr actualType
    let declaredType = parseType resolvedTypeStr
    newEnv <- applyAssignment env name declaredType actualType
    return (ASetVar name resolvedTypeStr expr, newEnv)

-- | Resolves the 'auto' keyword to a concrete type string.
resolveAuto :: DT.Text -> Type -> DT.Text
resolveAuto typeStr actualType =
    if typeStr == DT.pack "auto"
    then DT.pack (typeToString actualType)
    else typeStr

-- | Process function definition.
--
-- 1. Prepares environment (adds function name + arguments).
-- 2. Checks function body.
-- 3. Verifies return statements recursively.
processFuncDef :: CheckEnv -> DT.Text -> [(DT.Text, DT.Text)] -> DT.Text ->
    Ast -> Either String (Ast, CheckEnv)
processFuncDef env name args ret body = do
    let retTy = parseType ret
    (envWithFunc, envForBody) <- prepareFuncEnv env name args retTy
    (newBody, _) <- checkStmt envForBody body
    _ <- checkReturns envForBody retTy newBody
    return (ADefineFunc name args ret newBody, envWithFunc)

-- | Helper to setup the environment for a function.
-- Adds the function itself (for recursion) and its arguments.
prepareFuncEnv :: CheckEnv -> DT.Text -> [(DT.Text, DT.Text)] -> Type ->
    Either String (CheckEnv, CheckEnv)
prepareFuncEnv env name args retTy = do
    let argTypes = map (parseType . snd) args
    let funcType = TyFunc argTypes retTy
    envWithFunc <- insertVar env name funcType
    envForBody <- foldM (\e (n, t) -> insertVar e n (parseType t))
                        envWithFunc args
    return (envWithFunc, envForBody)

-- | Process If statement.
--
-- Abbreviations used:
-- c = Condition AST
-- t = Then branch AST
-- e = Else branch AST
processIfStmt :: CheckEnv -> Ast -> Ast -> Ast -> Either String (Ast, CheckEnv)
processIfStmt env c t e = do
    validateCondition env c "if"
    (newT, _) <- checkStmt env t
    (newE, _) <- checkStmt env e
    return (AIf c newT newE, env)

-- | Process While statement.
--
-- Abbreviations used:
-- c = Condition AST
processWhileStmt :: CheckEnv -> Ast -> Ast -> Either String (Ast, CheckEnv)
processWhileStmt env c body = do
    validateCondition env c "while"
    (newBody, _) <- checkStmt env body
    return (AWhile c newBody, env)

-- | Process For loop.
--
-- Abbreviations used:
-- i = Initialization AST (e.g. i = 0)
-- c = Condition AST (e.g. i < 10)
-- u = Update AST (e.g. i = i + 1)
-- b = Body AST
processForLoop :: CheckEnv -> Ast -> Ast -> Ast -> Ast ->
    Either String (Ast, CheckEnv)
processForLoop env i c u b = do
    (newInit, envAfterInit) <- checkStmt env i
    validateCondition envAfterInit c "for"
    (newUpdate, _) <- checkStmt envAfterInit u
    (newBody, _) <- checkStmt envAfterInit b
    return (AFor newInit c newUpdate newBody, env)

-- | Registers a new Structure definition in the environment.
defineStruct :: CheckEnv -> DT.Text -> [(DT.Text, DT.Text)] ->
    Either String CheckEnv
defineStruct env name fields =
    case Map.lookup name (envStructs env) of
        Just _ -> Left $ "Struct '" ++ DT.unpack name ++ "' is already defined"
        Nothing ->
            let parsedFields = map (\(n, t) -> (n, parseType t)) fields
                fieldsMap = Map.fromList parsedFields
                newDef = StructDef name fieldsMap
                newStructs = Map.insert name newDef (envStructs env)
            in Right $ env { envStructs = newStructs }

-- | Applies assignment logic.
-- Verifies that the actual value type matches the declared variable type.
applyAssignment :: CheckEnv -> DT.Text -> Type -> Type ->
    Either String CheckEnv
applyAssignment env name TyAuto actual = insertVar env name actual
applyAssignment env name declared actual
    | areTypesCompatible declared actual = insertVar env name declared
    | otherwise = Left $ "Variable '" ++ DT.unpack name ++ 
                         "' declared as " ++ typeToString declared ++ 
                         " but assigned " ++ typeToString actual

-- | Handles 'if' used as an expression (e.g. x = if c then 1 else 2).
checkIfExpr :: CheckEnv -> Ast -> Ast -> Ast -> Either String Type
checkIfExpr env cond thenB elseB = do
    tCond <- checkExpr env cond
    case tCond of
        TyBool -> checkBranches env thenB elseB
        TyInt  -> checkBranches env thenB elseB
        _ -> Left "Error: 'if' condition must be boolean"

-- | Verifies that both branches of an 'if' expression return compatible types.
checkBranches :: CheckEnv -> Ast -> Ast -> Either String Type
checkBranches env thenB elseB = do
    tThen <- checkExpr env thenB
    tElse <- checkExpr env elseB
    if areTypesCompatible tThen tElse
        then Right tThen
        else Left $ "Type mismatch in 'if' branches (" ++
                    typeToString tThen ++ " vs " ++
                    typeToString tElse ++ ")"

-- | Checks existence of a field within a specific struct definition.
validateStructField :: CheckEnv -> DT.Text -> DT.Text -> Either String Type
validateStructField env sName field = do
    def <- getStructDef env sName
    case Map.lookup field (structFields def) of
        Just t -> Right t
        Nothing -> Left $ "Field '" ++ DT.unpack field ++ 
                          "' not found in struct '" ++ 
                          DT.unpack sName ++ "'"

-- | Validates access to a struct field (e.g. object.field).
checkStructAccess :: CheckEnv -> Ast -> DT.Text -> Either String Type
checkStructAccess env obj field = do
    typeObj <- checkExpr env obj
    case typeObj of
        TyStruct name -> validateStructField env name field
        _ -> Left $ "Cannot access field '" ++ DT.unpack field ++ 
                    "' on non-struct type " ++ typeToString typeObj

-- | Validates a "new Struct {...}" instantiation.
-- Checks for extra fields, missing fields, and field types.
checkStructInstantiation :: CheckEnv -> DT.Text -> [(DT.Text, Ast)] ->
    Either String Type
checkStructInstantiation env name args = do
    def <- getStructDef env name
    let expected = structFields def
    let providedNames = map fst args
    let expectedNames = Map.keys expected

    checkExtraFields name expectedNames providedNames
    checkMissingFields name expectedNames providedNames
    mapM_ (validateField env expected) args
    Right (TyStruct name)

-- | Retrieves a Struct definition from the environment.
getStructDef :: CheckEnv -> DT.Text -> Either String StructDef
getStructDef env name = case Map.lookup name (envStructs env) of
    Just d -> Right d
    Nothing -> Left $ "Error: Undefined struct '" ++ 
                      DT.unpack name ++ "'"

-- | Errors if fields are provided that don't exist in the struct definition.
checkExtraFields :: DT.Text -> [DT.Text] -> [DT.Text] -> Either String ()
checkExtraFields name expected provided =
    let unknown = filter (`notElem` expected) provided
    in forM_ (take 1 unknown) $ \u ->
        Left $ "Error: Unknown field '" ++ DT.unpack u ++ 
                  "' in struct '" ++ DT.unpack name ++ "'"

-- | Errors if mandatory fields are missing from the instantiation.
checkMissingFields :: DT.Text -> [DT.Text] -> [DT.Text] -> Either String ()
checkMissingFields name expected provided =
    let missing = filter (`notElem` provided) expected
    in forM_ (take 1 missing) $ \m ->
        Left $ "Error: Missing field '" ++ DT.unpack m ++ 
                  "' in construction of '" ++ DT.unpack name ++ "'"

-- | Validates the type of a single field assignment in a constructor.
validateField :: CheckEnv -> Map.Map DT.Text Type -> (DT.Text, Ast) ->
    Either String ()
validateField env expectedFields (fieldName, expr) = do
    actualType <- checkExpr env expr
    let expectedType = expectedFields Map.! fieldName

    unless (areTypesCompatible expectedType actualType) $
        Left $ "Error: Field '" ++ DT.unpack fieldName ++ 
               "' expected " ++ typeToString expectedType ++ 
               " but got " ++ typeToString actualType

-- | Recursively verifies that all 'return' statements match expected type.
checkReturns :: CheckEnv -> Type -> Ast -> Either String ()
checkReturns env expected (AReturn expr) = do
    actual <- checkExpr env expr
    unless (areTypesCompatible expected actual) $
        Left $ "Return type mismatch: expected " ++
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

-- | Checks that all elements in a list literal have the same type.
checkListExpr :: CheckEnv -> [Ast] -> Either String Type
checkListExpr _ [] = Right (TyList TyVoid)
checkListExpr env (x:xs) = do
    expectedType <- checkExpr env x
    forM_ xs $ \elemAst -> do
        elemType <- checkExpr env elemAst
        unless (areTypesCompatible expectedType elemType) $
            Left $ "List type mismatch: expected " ++
                   typeToString expectedType ++
                   " but got " ++ typeToString elemType
    return (TyList expectedType)
