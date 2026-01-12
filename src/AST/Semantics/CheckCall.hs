{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Semantic Checker for Function Calls & Operators
-}

module AST.Semantics.CheckCall (checkCall) where

import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import AST.Ast (Ast(..))
import AST.Semantics.Type

-- | Type signature for the checkExpr function passed as argument.
-- Used to break the circular dependency between Check and CheckCall.
type CheckExprFn = CheckEnv -> Ast -> Either String Type

-- | Main entry point for checking calls and operators.
--
-- Dispatches the call to the appropriate handler based on the function symbol:
-- Arithmetic: +, -, *, div, mod
-- Comparison: <, >, <=, >=
-- Equality: eq? (==)
-- Logic: &&, ||
-- Unary: !
-- User functions: defined in the environment
--
-- @param CheckExprFn The callback to verify arguments.
-- @param CheckEnv The current semantic environment.
-- @param Ast The function node (usually ASymbol).
-- @param [Ast] The list of arguments.
-- @return Either String Type The return type of the call or an error message.
checkCall :: CheckExprFn -> CheckEnv -> Ast -> [Ast] -> Either String Type
checkCall checker env (APos _ _ f) args = checkCall checker env f args
checkCall checker env (ASymbol name) args
    | name `elem` map DT.pack ["+", "-", "*", "div", "mod"] =
        checkBinaryOp checker env name args TyInt TyInt
    | name `elem` map DT.pack ["<", ">", "<=", ">="] =
        checkBinaryOp checker env name args TyInt TyBool
    | name == DT.pack "eq?" = checkEqualityOp checker env args
    | name `elem` map DT.pack ["&&", "||"] =
        checkBinaryOp checker env name args TyBool TyBool
    | name == DT.pack "!" = checkUnaryOp checker env name args TyBool TyBool
    | otherwise = checkUserFunc checker env name args
checkCall _ _ _ _ = Left "Error: Invalid function call (must be a symbol)"

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
            tLeft <- checker env left
            tRight <- checker env right
            if areTypesCompatible tLeft tRight
                then Right TyBool
                else Left $ "Equality requires compatible types, got " ++
                        typeToString tLeft ++ " and " ++ typeToString tRight
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
