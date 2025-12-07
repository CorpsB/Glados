{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Builtins
-}

module Eval.Builtins (execBuiltin) where

import Ast (Ast(..))
import Type.Integer (toInt64, fromInt64)
import Utils.List (astToList, listToAst, sameLength)
import Control.Monad (foldM)

builtinEq :: [Ast] -> Either String Ast
builtinEq [AInteger x, AInteger y] = Right $ ABool (toInt64 x == toInt64 y)
builtinEq [ABool x, ABool y] = Right $ ABool (x == y)
builtinEq args = Left $ "*** ERROR: 'eq?' expects two " ++
    "integers or two booleans, got: " ++ show args

builtinLowerThan :: [Ast] -> Either String Ast
builtinLowerThan [AInteger x, AInteger y] =
    Right $ ABool (toInt64 x < toInt64 y)
builtinLowerThan args = Left $ "*** ERROR: '<' expects two " ++
    "integers, got: " ++ show args

builtinGreaterThan :: [Ast] -> Either String Ast
builtinGreaterThan [AInteger x, AInteger y] =
    Right $ ABool (toInt64 x > toInt64 y)
builtinGreaterThan args = Left $ "*** ERROR: '>' expects two " ++
    "integers, got: " ++ show args

builtinAddition :: [Ast] -> Either String Ast
builtinAddition [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (toInt64 x + toInt64 y))
builtinAddition args = Left $ "*** ERROR: '+' expects two " ++
    "integers, got: " ++ show args

builtinSubtraction :: [Ast] -> Either String Ast
builtinSubtraction [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (toInt64 x - toInt64 y))
builtinSubtraction args = Left $ "*** ERROR: '-' expects two " ++
    "integers, got: " ++ show args

builtinMultiplication :: [Ast] -> Either String Ast
builtinMultiplication [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (toInt64 x * toInt64 y))
builtinMultiplication args = Left $ "*** ERROR: '*' expects two " ++
    "integers, got: " ++ show args

builtinDivision :: [Ast] -> Either String Ast
builtinDivision [AInteger _, AInteger y]
    | toInt64 y == 0 = Left $ "*** ERROR: 'div' division by zero"
builtinDivision [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (div (toInt64 x) (toInt64 y)))
builtinDivision args =
    Left $ "*** ERROR: 'div' expects two " ++
    "integers, got: " ++ show args

builtinModulo :: [Ast] -> Either String Ast
builtinModulo [AInteger _, AInteger y]
    | toInt64 y == 0 = Left $ "*** ERROR: 'mod' division by zero"
builtinModulo [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (mod (toInt64 x) (toInt64 y)))
builtinModulo args = Left $ "*** ERROR: 'mod' expects two " ++
    "integers, got: " ++ show args

execBuiltin :: String -> [Ast] -> Either String Ast
execBuiltin "eq?" as = builtinEq as
execBuiltin "<" as = builtinLowerThan as
execBuiltin ">" as = builtinGreaterThan as
execBuiltin "+" as = builtinAddition as
execBuiltin "-" as = builtinSubtraction as
execBuiltin "*" as = builtinMultiplication as
execBuiltin "div" as = builtinDivision as
execBuiltin "mod" as = builtinModulo as
execBuiltin "list" as = builtinList as
execBuiltin "cons" as = builtinCons as
execBuiltin "car" as = builtinCar as
execBuiltin "cdr" as = builtinCdr as
execBuiltin "list?" as = builtinIsList as
execBuiltin "append" as = builtinAppend as
execBuiltin "length" as = builtinLength as
execBuiltin op _ = Left $ "*** ERROR: Unknown builtin: " ++ op


builtinList :: [Ast] -> Either String Ast
builtinList as = Right $ listToAst as

builtinCons :: [Ast] -> Either String Ast
builtinCons [v, lst] = do
    xs <- astToList lst
    Right $ listToAst (v : xs)
builtinCons args =
    Left $ "*** ERROR: 'cons' expects two args, got: " ++ show args

builtinCar :: [Ast] -> Either String Ast
builtinCar [lst] = do
    xs <- astToList lst
    case xs of
        (h:_) -> Right h
        [] -> Left "*** ERROR: 'car' called on empty list"
builtinCar args = Left $ "*** ERROR: 'car' expects one arg, got: " ++ show args

builtinCdr :: [Ast] -> Either String Ast
builtinCdr [lst] = do
    xs <- astToList lst
    case xs of
        (_:ts) -> Right $ listToAst ts
        [] -> Left "*** ERROR: 'cdr' called on empty list"
builtinCdr args = Left $ "*** ERROR: 'cdr' expects one arg, got: " ++ show args

builtinIsList :: [Ast] -> Either String Ast
builtinIsList [x] = case x of
    AList _ -> Right $ ABool True
    _ -> Right $ ABool False
builtinIsList args =
    Left $ "*** ERROR: 'list?' expects one arg, got: " ++ show args

builtinAppend :: [Ast] -> Either String Ast
builtinAppend args = do
    concatenated <- foldM (\buf a -> do
        xs <- astToList a
        Right $ buf ++ xs) [] args
    Right $ listToAst concatenated

builtinLength :: [Ast] -> Either String Ast
builtinLength [lst] = do
    xs <- astToList lst
    Right $ AInteger (fromInt64 (fromIntegral (length xs)))
builtinLength args =
    Left $ "*** ERROR: 'length' expects one arg, got: " ++ show args
