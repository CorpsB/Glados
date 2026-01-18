{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Semantic Checker for Function Calls & Operators
-}

module AST.Semantics.CheckCall (checkCall, checkEqualityOp,checkTypeof,
    checkFFRead,
    checkFFWrite,
    checkOpen,
    checkClose,
    checkRead,
    checkInput) where

import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import AST.Ast (Ast(..))
import AST.Semantics.Type
import Common.Type.Integer (IntValue(..))
import Data.Char (chr)
import Control.Monad (unless)
import Control.Applicative ((<|>))

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
checkCall checker env expr args = do
    funcType <- checker env expr
    case funcType of
        TyFunc expectedArgs retType ->
            verifyFuncCall checker env
                (DT.pack "<anonymous_call>") args expectedArgs retType
        _ -> Left $ "Error: expression is not a function ("
            ++ typeToString funcType ++ ")"

-- | Main dispatcher for built-in operators and special functions.
--
-- Tries to match arithmetic, comparison, logic, or special internal calls
-- like 'attr_update'.
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
        strictOps = map DT.pack ["teq?", "tneq?"]

-- | Main dispatcher for named keyword functions.
--
-- This function acts as a router that delegates the verification to specific
-- sub-dispatchers based on the function category. It uses the (<|>) operator
-- to try each handler in sequence:
-- 1. Base Operators (equality, not, structs)
-- 2. System Functions (print, exit, typeof)
-- 3. IO Functions (files, input)
-- 4. Data Functions (casts, list ops)
--
-- Arguments:
--   c (checker) : The callback function to verify sub-expressions.
--   e (env)     : The current semantic environment.
--   n (name)    : The function/keyword name (e.g. "print", "open").
--   a (args)    : The list of AST arguments passed to the function.
--
-- Returns 'Just (Right Type)' if a match is found and valid,
-- 'Just (Left Error)' if a match is found but invalid,
-- or 'Nothing' if the symbol is not a keyword.
checkKeywordFuncs :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
                  -> Maybe (Either String Type)
checkKeywordFuncs c e n a =
    checkBaseOps c e n a <|>
    checkSystemOps c e n a <|>
    checkIOFuncs c e n a <|>
    checkDataFuncs c e n a

-- | Dispatcher for fundamental language operators.
--
-- Handles:
-- * Equality checks: 'eq?' (==), 'neq?' (!=)
-- * Boolean negation: '!'
-- * Internal structure updates: attr_update
checkBaseOps :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
             -> Maybe (Either String Type)
checkBaseOps c e n a
    | n == DT.pack "eq?" || n == DT.pack "neq?" = Just $ checkEqualityOp c e a
    | n == DT.pack "!"           = Just $ checkUnaryOp c e n a TyBool TyBool
    | n == DT.pack "attr_update" = Just $ checkSetField c e a
    | otherwise = Nothing

-- | Dispatcher for system utilities and introspection functions.
--
-- Handles:
-- * 'print': Standard output.
-- * 'exit': Terminate the program with a code.
-- * 'typeof': Runtime type inspection.
checkSystemOps :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
               -> Maybe (Either String Type)
checkSystemOps c e n a
    | n == DT.pack "print"  = Just $ checkPrint c e a
    | n == DT.pack "exit"   = Just $ checkExit c e a
    | n == DT.pack "typeof" = Just $ checkTypeof c e a
    | otherwise = Nothing

-- | Dispatcher for Input/Output operations (File System & Console).
--
-- Handles:
-- * High-level File I/O: 'ffread' (read whole file), 'ffwrite' (write content).
-- * Low-level I/O: 'open' (get fd), 'close' (close fd), 'read' (read bytes),
--   'input' (read from stdin/fd).
checkIOFuncs :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
             -> Maybe (Either String Type)
checkIOFuncs c e n a
    | n == DT.pack "ffread"  = Just $ checkFFRead c e a
    | n == DT.pack "ffwrite" = Just $ checkFFWrite c e a
    | n == DT.pack "open"    = Just $ checkOpen c e a
    | n == DT.pack "close"   = Just $ checkClose c e a
    | n == DT.pack "read"    = Just $ checkRead c e a
    | n == DT.pack "input"   = Just $ checkInput c e a
    | n == DT.pack "write"   = Just $ checkWrite c e a
    | otherwise = Nothing

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
        listOps = map DT.pack ["cons", "head", "tail", "nth", "nth_update"]

-- | Dispatcher for List specific operations.
checkListOps :: CheckExprFn -> CheckEnv -> DT.Text -> [Ast]
             -> Either String Type
checkListOps c e n a
    | n == DT.pack "cons" = checkCons c e a
    | n == DT.pack "head" = checkHead c e a
    | n == DT.pack "tail" = checkTail c e a
    | n == DT.pack "nth"  = checkNth c e a
    | n == DT.pack "nth_update" = checkUpdate c e a
    | otherwise = Left "Unknown list operator"

-- | Validates 'update(list, index, value)'.
-- Separates type retrieval from validation to fit coding style.
checkUpdate :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkUpdate checker env [lst, idx, val] = do
    tLst <- checker env lst
    tIdx <- checker env idx
    tVal <- checker env val
    validateListUpdate tLst tIdx tVal
checkUpdate _ _ _ = Left "update expects 3 arguments (list, index, value)"

-- | Helper to validate types for list update.
validateListUpdate :: Type -> Type -> Type -> Either String Type
validateListUpdate (TyList inner) tIdx tVal
    | not (areTypesCompatible TyInt tIdx) =
        Left "update index must be an integer"
    | areTypesCompatible inner tVal = Right (TyList inner)
    | otherwise = Left $ "Type mismatch in list update: expected " ++
                         typeToString inner ++ " but got " ++
                         typeToString tVal
validateListUpdate _ _ _ = Left "update expects a list as first argument"

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

-- | Validates the special internal function 'attr_update'.
--
-- This function is generated by the Parser when assigning a value to a
-- structure field (e.g. `p.x = 10` becomes `attr_update(p, "x", 10)`).
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
        _ -> Left "attr_update expects a structure as first argument"
checkSetField _ _ _ = Left "attr_update expects 3 arguments"

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
-- to transmit the field name string to the 'attr_update' function.
extractStringFromAst :: Ast -> Either String DT.Text
extractStringFromAst (AList chars) =
    let extractChar (AInteger (IChar c)) = Just (chr (fromIntegral c))
        extractChar _ = Nothing
    in case mapM extractChar chars of
        Just str -> Right (DT.pack str)
        Nothing -> Left "Invalid string format in AST"
extractStringFromAst _ = Left "Invalid field name format in attr_update"

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

-- | Validates 'typeof(arg)'.
--
-- Introspection function that returns the type of the argument as a string.
-- It accepts any valid expression type.
--
-- @param arg: Any expression.
-- @return: String (TyList TyInt) representing the type name (e.g., "int", "bool").
checkTypeof :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkTypeof checker env [arg] = do
    _ <- checker env arg
    Right (TyList TyInt)
checkTypeof _ _ _ = Left "typeof expects 1 argument"

-- | Validates 'ffread(path)'.
--
-- High-level File Read: Reads the entire content of a file.
--
-- @param path: String (TyList TyInt) representing the file path.
-- @return: [String] (TyList (TyList TyInt)), where each element is a line.
checkFFRead :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkFFRead checker env [path] = do
    tPath <- checker env path
    case tPath of
        TyList TyInt -> Right (TyList (TyList TyInt))
        _ -> Left "ffread expects a string (path) as argument"
checkFFRead _ _ _ = Left "ffread expects 1 argument"

-- | Validates 'ffwrite(path, content)'.
--
-- High-level File Write: Writes a list of strings to a file.
--
-- @param path: String (TyList TyInt) representing the file path.
-- @param content: [String] (TyList (TyList TyInt)) the lines to write.
-- @return: Bool (TyBool) indicating success or failure.
checkFFWrite :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkFFWrite checker env [path, content] = do
    tPath <- checker env path
    tContent <- checker env content
    
    case (tPath, tContent) of
        (TyList TyInt, TyList (TyList TyInt)) -> Right TyBool
        _ -> Left "ffwrite expects (path: string, content: [string])"
checkFFWrite _ _ _ = Left "ffwrite expects 2 arguments"

-- | Validates 'open(path, mode)'.
--
-- Low-level Open: Opens a file and returns a file descriptor.
--
-- @param path: String (TyList TyInt) representing the file path.
-- @param mode: Int (TyInt) representing the access mode (e.g., 0 for read, 1 for write).
-- @return: Int (TyInt) the file descriptor (FD).
checkOpen :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkOpen checker env [path, mode] = do
    tPath <- checker env path
    tMode <- checker env mode
    if areTypesCompatible (TyList TyInt)
            tPath && areTypesCompatible TyInt tMode
        then Right TyInt
        else Left "open expects (path: string, mode: int)"
checkOpen _ _ _ = Left "open expects 2 arguments"

-- | Validates 'close(fd)'.
--
-- Low-level Close: Closes an open file descriptor.
--
-- @param fd: Int (TyInt) the file descriptor to close.
-- @return: Int (TyInt) usually 0 for success.
checkClose :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkClose checker env [fd] = do
    tFd <- checker env fd
    if areTypesCompatible TyInt tFd
        then Right TyInt
        else Left "close expects an integer file descriptor"
checkClose _ _ _ = Left "close expects 1 argument"

-- | Validates 'read(fd, size)'.
--
-- Low-level Read: Reads a specific number of bytes/characters from a file descriptor.
--
-- @param fd: Int (TyInt) the file descriptor to read from.
-- @param size: Int (TyInt) the number of characters to read.
-- @return: String (TyList TyInt) containing the read data.
checkRead :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkRead checker env [fd, size] = do
    tFd <- checker env fd
    tSize <- checker env size
    if areTypesCompatible TyInt tFd && areTypesCompatible TyInt tSize
        then Right (TyList TyInt)
        else Left "read expects (fd: int, size: int)"
checkRead _ _ _ = Left "read expects 2 arguments"

-- | Validates 'input(fd)'.
--
-- Stream Input: Reads a line from a file descriptor (often used with 0 for stdin).
--
-- @param fd: Int (TyInt) the file descriptor to read from.
-- @return: String (TyList TyInt) containing the input line.
checkInput :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkInput checker env [fd] = do
    tFd <- checker env fd
    if areTypesCompatible TyInt tFd
        then Right (TyList TyInt)
        else Left "input expects an integer file descriptor"
checkInput _ _ _ = Left "input expects 1 argument"

-- | Validates 'write(fd, content)'.
--
-- Low-level Write: Writes content to a file descriptor.
--
-- @param fd: Int (TyInt) the file descriptor.
-- @param content: String/List ([char] or [int]) the data to write.
-- @return: Int (TyInt) usually the number of bytes written.
checkWrite :: CheckExprFn -> CheckEnv -> [Ast] -> Either String Type
checkWrite checker env [fd, content] = do
    tFd <- checker env fd
    tContent <- checker env content
    unless (areTypesCompatible TyInt tFd) $ 
        Left "write expects an integer file descriptor (int) as first argument"
    case tContent of
        TyList _ -> Right TyInt
        _ -> Left "write expects a string or list ([char]) as second argument"
checkWrite _ _ _ = Left "write expects 2 arguments (fd: int, content: [char])"
