{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Semantic Checker for Function Calls & Operators
-}

module AST.Semantics.CheckCall (checkCall, checkEqualityOp,) where

import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import AST.Ast (Ast(..))
import AST.Semantics.Type
import Common.Type.Integer (IntValue(..))
import Data.Char (chr)
import Control.Monad (unless)

-- | Type signature for the checkExpr function passed as argument.
-- Used to break the circular dependency between Check and CheckCall.
type CheckExprFn = CheckEnv -> Ast -> Either String Type

-- | Main entry point for checking calls and operators.
--
-- Dispatches the call to the appropriate handler. It first checks if the
-- function is a built-in operator (Math, Logic, Special). If not, it
-- attempts to resolve it as a user-defined function.
--
-- @param CheckExprFn The callback to verify arguments.
-- @param CheckEnv The current semantic environment.
-- @param Ast The function node (usually ASymbol).
-- @param [Ast] The list of arguments.
-- @return Either String Type The return type of the call or an error message.
checkCall :: CheckExprFn -> CheckEnv -> Ast -> [Ast] -> Either String Type
checkCall checker env (APos _ _ f) args = checkCall checker env f args
checkCall checker env (ASymbol name) args =
    case checkBuiltinOp checker env name args of
        Just result -> result
        Nothing     -> checkUserFunc checker env name args
checkCall _ _ _ _ = Left "Error: Invalid function call (must be a symbol)"

-- | Main dispatcher for built-in operators and special functions.
--
-- Tries to match arithmetic, comparison, logic, or special internal calls
-- like 'set_field'.
--
-- Returns 'Nothing' if the symbol is not a built-in operator.
checkBuiltinOp :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
               -> Maybe (Either String Type)
checkBuiltinOp c e n a =
    case checkMathComp c e n a of
        Just res -> Just res
        Nothing -> checkLogicSpecial c e n a

-- | Sub-dispatcher for Arithmetic and Comparison operators.
--
-- Handles:
-- * Arithmetic: +, -, *, div, mod
-- * Comparison: <, >, <=, >=
--
-- Split from the main function to reduce cyclomatic complexity and line length.
checkMathComp :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
              -> Maybe (Either String Type)
checkMathComp c e n a
    | n `elem` mathOps = Just $ checkBinaryOp c e n a TyInt TyInt
    | n `elem` compOps = Just $ checkBinaryOp c e n a TyInt TyBool
    | otherwise = Nothing
    where
        mathOps = map DT.pack ["+", "-", "*", "div", "mod"]
        compOps = map DT.pack ["<", ">", "<=", ">="]

-- | Validates 'print'. Accepts 1 argument of ANY type. Returns Void.
checkPrint :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkPrint checker env [arg] = do
    _ <- checker env arg
    Right TyVoid
checkPrint _ _ _ = Left "Function 'print' expects exactly 1 argument"

-- | Validates strict operators (===, !==). Always returns Bool.
-- Accepts any types (even incompatible ones).
checkStrictOp :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkStrictOp checker env [left, right] = do
    _ <- checker env left
    _ <- checker env right
    Right TyBool
checkStrictOp _ _ _ = Left "Strict comparison operator expects 2 arguments"

-- | Sub-dispatcher for Logic and Strict Comparison operators.
--
-- This function handles boolean logic (&&, ||) and strict comparison (===, !==).
-- For other special keywords (like 'print' or 'eq?'), it delegates to 'checkKeywordFuncs'
-- to keep the function size small and manageable.
--
-- @param c (checker): The callback function to verify sub-expressions.
-- @param e (env): The current semantic environment.
-- @param n (name): The operator/function name (e.g. "&&", "===").
-- @param a (args): The list of arguments passed to the operator.
checkLogicSpecial :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
                  -> Maybe (Either String Type)
checkLogicSpecial c e n a
    | n `elem` logicOps = Just $ checkBinaryOp c e n a TyBool TyBool
    | n `elem` strictOps = Just $ checkStrictOp c e a
    | otherwise = checkKeywordFuncs c e n a
    where
        logicOps = map DT.pack ["&&", "||"]
        strictOps = map DT.pack ["?teq", "?tneq"]

-- | Helper dispatcher for named keyword functions.
--
-- This function isolates the matching logic for built-in functions that are
-- not symbolic operators, such as 'print', 'eq?', or internal helpers like 'set_field'.
--
-- @param c (checker): The callback function to verify sub-expressions.
-- @param e (env): The current semantic environment.
-- @param n (name): The keyword name (e.g. "print", "eq?").
-- @param a (args): The list of arguments passed to the function.
checkKeywordFuncs :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
                  -> Maybe (Either String Type)
checkKeywordFuncs c e n a
    | n == DT.pack "eq?" = Just $ checkEqualityOp c e a
    | n == DT.pack "!" = Just $ checkUnaryOp c e n a TyBool TyBool
    | n == DT.pack "set_field" = Just $ checkSetField c e a
    | n == DT.pack "print" = Just $ checkPrint c e a
    | n == DT.pack "exit" = Just $ checkExit c e a
    | otherwise = checkDataFuncs c e n a

-- | Dispatcher for Data functions: Casts and List operations.
checkDataFuncs :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
               -> Maybe (Either String Type)
checkDataFuncs c e n a
    | n `elem` castOps = Just $ checkUnaryOp c e n a TyInt TyInt
    | n `elem` listOps = Just $ checkListOps c e n a
    | otherwise = Nothing
    where
        castOps = map DT.pack [ "int8", "uint8", "int16", "uint16"
                              , "int32", "uint32", "int64", "uint64"
                              , "char", "uchar" ]
        listOps = map DT.pack ["cons", "head", "tail", "nth"]

-- | Dispatcher for List specific operations.
checkListOps :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
             -> Either String Type
checkListOps c e n a
    | n == DT.pack "cons" = checkCons c e a
    | n == DT.pack "head" = checkHead c e a
    | n == DT.pack "tail" = checkTail c e a
    | n == DT.pack "nth"  = checkNth c e a
    | otherwise = Left "Unknown list operator"

-- | Validates 'exit'. Expects 1 Int. Returns Void.
checkExit :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkExit checker env [arg] = do
    t <- checker env arg
    unless (areTypesCompatible TyInt t) $ Left "exit expects an integer code"
    Right TyVoid
checkExit _ _ _ = Left "exit expects 1 argument"

-- | Validates 'cons(elem, list)'. Returns 'list' type.
checkCons :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkCons checker env [el, lst] = do
    tEl <- checker env el
    tLst <- checker env lst
    case tLst of
        TyList inner -> if areTypesCompatible inner tEl
                        then Right tLst
                        else Left $ "cons type mismatch: " ++
                             typeToString tEl ++ " vs " ++ typeToString inner
        _ -> Left "cons expects a list as second argument"
checkCons _ _ _ = Left "cons expects 2 arguments"

-- | Validates 'head(list)'. Returns inner type.
checkHead :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkHead checker env [lst] = do
    tLst <- checker env lst
    case tLst of
        TyList inner -> Right inner
        _ -> Left "head expects a list"
checkHead _ _ _ = Left "head expects 1 argument"

-- | Validates 'tail(list)'. Returns 'list' type.
checkTail :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkTail checker env [lst] = do
    tLst <- checker env lst
    case tLst of
        TyList _ -> Right tLst
        _ -> Left "tail expects a list"
checkTail _ _ _ = Left "tail expects 1 argument"

-- | Validates 'nth(list, index)'. Returns inner type.
checkNth :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkNth checker env [lst, idx] = do
    tLst <- checker env lst
    tIdx <- checker env idx
    unless (areTypesCompatible TyInt tIdx) $ Left
        "nth index must be an integer"
    case tLst of
        TyList inner -> Right inner
        _ -> Left "nth expects a list as first argument"
checkNth _ _ _ = Left "nth expects 2 arguments"

-- | Validates the special internal function 'set_field'.
--
-- This function is generated by the Parser when assigning a value to a
-- structure field (e.g. `p.x = 10` becomes `set_field(p, "x", 10)`).
--
-- It verifies that:
-- 1. The object is a structure.
-- 2. The field exists in the structure definition.
-- 3. The assigned value type matches the field type.
checkSetField :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkSetField checker env [obj, fieldNameAst, val] = do
    objType <- checker env obj
    case objType of
        TyStruct sName -> do
            fieldName <- extractStringFromAst fieldNameAst
            checkFieldInStruct checker env sName fieldName val
        _ -> Left "set_field expects a structure as first argument"
checkSetField _ _ _ = Left "set_field expects 3 arguments"

-- | Validate that a specific field exists within a structure.
--
-- If found, delegates to 'validateAssignment' for type checking.
checkFieldInStruct :: CheckExprFn -> CheckEnv -> DT.Text -> DT.Text -> Ast
                   -> Either String Type
checkFieldInStruct checker env sName fieldName val = do
    def <- getStructDef env sName
    case Map.lookup fieldName (structFields def) of
        Just expectedType ->
            validateAssignment checker env val expectedType fieldName sName
        Nothing -> Left $ "Field '" ++ DT.unpack fieldName ++
                          "' not found in struct '" ++
                          DT.unpack sName ++ "'"

-- | Final validation step for field assignment.
--
-- Checks if the value type is compatible with the field's expected type.
-- Returns the structure type itself on success (allowing expression chaining).
validateAssignment :: CheckExprFn -> CheckEnv -> Ast -> Type ->
    DT.Text -> DT.Text -> Either String Type
validateAssignment checker env val expectedType fieldName sName = do
    valType <- checker env val
    if areTypesCompatible expectedType valType
        then Right (TyStruct sName)
        else Left $ "Type mismatch in field assignment '" ++
                    DT.unpack fieldName ++ "'. Expected " ++
                    typeToString expectedType ++ ", got " ++
                    typeToString valType

-- | Extract a string (Text) from an AST node.
--
-- Expects an AList of AInteger(IChar), which is the format used by the parser
-- to transmit the field name string to the 'set_field' function.
extractStringFromAst :: Ast -> Either String DT.Text
extractStringFromAst (AList chars) =
    let extractChar (AInteger (IChar c)) = Just (chr (fromIntegral c))
        extractChar _ = Nothing
    in case mapM extractChar chars of
        Just str -> Right (DT.pack str)
        Nothing -> Left "Invalid string format in AST"
extractStringFromAst _ = Left "Invalid field name format in set_field"

-- | Retrieve a structure definition from the environment.
getStructDef :: CheckEnv -> DT.Text -> Either String StructDef
getStructDef env name = case Map.lookup name (envStructs env) of
    Just d -> Right d
    Nothing -> Left $ "Error: Undefined struct '" ++ DT.unpack name ++ "'"

-- | Validates standard binary operators.
--
-- Checks that both arguments match the expected input type.
-- Returns the specified output type.
checkBinaryOp :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
              -> Type -> Type -> Either String Type
checkBinaryOp checker env op args inType outType =
    case args of
        [left, right] ->
                validateBinaryOp checker env op left right inType outType
        _ -> Left $ "Operator '" ++ DT.unpack op ++ "' expects 2 arguments"

-- | Validate binary operands types.
validateBinaryOp :: CheckExprFn -> CheckEnv -> DT.Text -> Ast -> Ast
                 -> Type -> Type -> Either String Type
validateBinaryOp checker env op left right inType outType = do
    tLeft <- checker env left
    tRight <- checker env right
    if areTypesCompatible tLeft inType && areTypesCompatible tRight inType
        then Right outType
        else Left $ binaryOpError op inType tLeft tRight

-- | Formats the error message for binary operators.
binaryOpError :: DT.Text -> Type -> Type -> Type -> String
binaryOpError op expected actualLeft actualRight =
    "Operator '" ++ DT.unpack op ++ "' expects (" ++
    typeToString expected ++ ", " ++ typeToString expected ++
    ") but got (" ++ typeToString actualLeft ++ ", " ++
    typeToString actualRight ++ ")"

-- | Validates equality checks (==).
--
-- Unlike other binary ops, equality allows any type as long as
-- both operands have compatible types (e.g. int==int, bool==bool).
checkEqualityOp :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkEqualityOp checker env args =
    case args of
        [left, right] -> do
            _ <- checker env left
            _ <- checker env right
            Right TyBool
        _ -> Left "Equality operator expects 2 arguments"

-- | Validates unary operators (like !).
checkUnaryOp :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
             -> Type -> Type -> Either String Type
checkUnaryOp checker env op args inType outType =
    case args of
        [operand] -> do
            tOp <- checker env operand
            if areTypesCompatible tOp inType
                then Right outType
                else Left $ "Operator '" ++ DT.unpack op ++ "' expects " ++
                            typeToString inType ++" but got "
                            ++ typeToString tOp
        _ -> Left $ "Operator '" ++ DT.unpack op ++ "' expects 1 argument"

-- | Validates user-defined function calls.
--
-- Looks up the function in the environment and verifies:
-- The symbol exists and is a function.
-- The argument count matches.
-- Each argument type matches the function definition.
checkUserFunc :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast] -> Either String Type
checkUserFunc checker env name args =
    case Map.lookup name (envVars env) of
        Just (TyFunc argTypes retType) ->
            verifyFuncCall checker env name args argTypes retType
        Just _ -> Left $ "'" ++ DT.unpack name ++ "' is not a function"
        Nothing -> Left $ "Undefined function '" ++ DT.unpack name ++ "'"

-- | Dispatches verification between argument count and argument types.
verifyFuncCall :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
               -> [Type] -> Type -> Either String Type
verifyFuncCall checker env name args expected ret
    | length args /= length expected = verifyArgCount name expected args
    | otherwise = verifyArgTypes checker env name args expected ret

-- | Checks if the number of arguments matches the definition.
verifyArgCount :: DT.Text -> [Type] -> [Ast] -> Either String Type
verifyArgCount name expected args =
    Left $ "Function '" ++ DT.unpack name ++ "' expects " ++
           show (length expected) ++ " arguments but got "++
           show (length args)

-- | Checks if the types of arguments match the definition.
verifyArgTypes :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
               -> [Type] -> Type -> Either String Type
verifyArgTypes checker env name args expected ret = do
    checked <- mapM (checker env) args
    let errors = filter (\(expect, act) -> not (areTypesCompatible expect act))
                        (zip expected checked)
    if null errors
        then Right ret
        else Left $ "Argument type mismatch in call to '" ++
            DT.unpack name ++ "'"
