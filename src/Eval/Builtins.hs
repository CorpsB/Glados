{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Builtins
-}

module Eval.Builtins (execBuiltin) where

import Ast (Ast(..))
import Type.Integer (toInt64, fromInt64)
import Utils.List (astToList, listToAst)
import Control.Monad (foldM)
import qualified Data.Text as DT

builtinEq :: [Ast] -> Either DT.Text Ast
builtinEq [AInteger x, AInteger y] = Right $ ABool (toInt64 x == toInt64 y)
builtinEq [ABool x, ABool y] = Right $ ABool (x == y)
builtinEq args = Left $ DT.pack $ "*** ERROR: 'eq?' expects two " ++
    "integers or two booleans, got: " ++ show args

builtinLowerThan :: [Ast] -> Either DT.Text Ast
builtinLowerThan [AInteger x, AInteger y] =
    Right $ ABool (toInt64 x < toInt64 y)
builtinLowerThan args = Left $ DT.pack $ "*** ERROR: '<' expects two " ++
    "integers, got: " ++ show args

builtinGreaterThan :: [Ast] -> Either DT.Text Ast
builtinGreaterThan [AInteger x, AInteger y] =
    Right $ ABool (toInt64 x > toInt64 y)
builtinGreaterThan args = Left $ DT.pack $ "*** ERROR: '>' expects two " ++
    "integers, got: " ++ show args

builtinAddition :: [Ast] -> Either DT.Text Ast
builtinAddition [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (toInt64 x + toInt64 y))
builtinAddition args = Left $ DT.pack $ "*** ERROR: '+' expects two " ++
    "integers, got: " ++ show args

builtinSubtraction :: [Ast] -> Either DT.Text Ast
builtinSubtraction [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (toInt64 x - toInt64 y))
builtinSubtraction args = Left $ DT.pack $ "*** ERROR: '-' expects two " ++
    "integers, got: " ++ show args

builtinMultiplication :: [Ast] -> Either DT.Text Ast
builtinMultiplication [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (toInt64 x * toInt64 y))
builtinMultiplication args = Left $ DT.pack $ "*** ERROR: '*' expects two " ++
    "integers, got: " ++ show args

builtinDivision :: [Ast] -> Either DT.Text Ast
builtinDivision [AInteger _, AInteger y]
    | toInt64 y == 0 = Left $ DT.pack "*** ERROR: 'div' division by zero"
builtinDivision [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (div (toInt64 x) (toInt64 y)))
builtinDivision args =
    Left $ DT.pack $ "*** ERROR: 'div' expects two " ++
    "integers, got: " ++ show args

builtinModulo :: [Ast] -> Either DT.Text Ast
builtinModulo [AInteger _, AInteger y]
    | toInt64 y == 0 = Left $ DT.pack "*** ERROR: 'mod' division by zero"
builtinModulo [AInteger x, AInteger y] =
    Right $ AInteger (fromInt64 (mod (toInt64 x) (toInt64 y)))
builtinModulo args = Left $ DT.pack $ "*** ERROR: 'mod' expects two " ++
    "integers, got: " ++ show args

mathOps :: [(DT.Text, [Ast] -> Either DT.Text Ast)]
mathOps =
    [ (DT.pack "+", builtinAddition)
    , (DT.pack "-", builtinSubtraction)
    , (DT.pack "*", builtinMultiplication)
    , (DT.pack "div", builtinDivision)
    , (DT.pack "mod", builtinModulo)
    ]

logicOps :: [(DT.Text, [Ast] -> Either DT.Text Ast)]
logicOps =
    [ (DT.pack "eq?", builtinEq)
    , (DT.pack "<", builtinLowerThan)
    , (DT.pack ">", builtinGreaterThan)
    ]

listOps :: [(DT.Text, [Ast] -> Either DT.Text Ast)]
listOps =
    [ (DT.pack "list", builtinList)
    , (DT.pack "cons", builtinCons)
    , (DT.pack "car", builtinCar)
    , (DT.pack "cdr", builtinCdr)
    , (DT.pack "list?", builtinIsList)
    , (DT.pack "append", builtinAppend)
    , (DT.pack "length", builtinLength)
    ]

builtinsTable :: [(DT.Text, [Ast] -> Either DT.Text Ast)]
builtinsTable = mathOps ++ logicOps ++ listOps

execBuiltin :: DT.Text -> [Ast] -> Either DT.Text Ast
execBuiltin op args = case lookup op builtinsTable of
    Just func -> func args
    Nothing   -> Left $ DT.pack $
        "*** ERROR: Unknown builtin: " ++ DT.unpack op

builtinList :: [Ast] -> Either DT.Text Ast
builtinList as = Right $ listToAst as

builtinCons :: [Ast] -> Either DT.Text Ast
builtinCons [v, lst] = do
    xs <- astToList lst
    Right $ listToAst (v : xs)
builtinCons args =
    Left $ DT.pack $ "*** ERROR: 'cons' expects two args, got: " ++ show args

builtinCar :: [Ast] -> Either DT.Text Ast
builtinCar [lst] = do
    xs <- astToList lst
    case xs of
        (h:_) -> Right h
        [] -> Left $ DT.pack "*** ERROR: 'car' called on empty list"
builtinCar args = Left $ DT.pack $
    "*** ERROR: 'car' expects one arg, got: " ++ show args

builtinCdr :: [Ast] -> Either DT.Text Ast
builtinCdr [lst] = do
    xs <- astToList lst
    case xs of
        (_:ts) -> Right $ listToAst ts
        [] -> Left $ DT.pack "*** ERROR: 'cdr' called on empty list"
builtinCdr args = Left $ DT.pack $
    "*** ERROR: 'cdr' expects one arg, got: " ++ show args

builtinIsList :: [Ast] -> Either DT.Text Ast
builtinIsList [x] = case x of
    AList _ -> Right $ ABool True
    _ -> Right $ ABool False
builtinIsList args =
    Left $ DT.pack $ "*** ERROR: 'list?' expects one arg, got: " ++ show args

builtinAppend :: [Ast] -> Either DT.Text Ast
builtinAppend args = do
    concatenated <- foldM (\buf a -> do
        xs <- astToList a
        Right $ buf ++ xs) [] args
    Right $ listToAst concatenated

builtinLength :: [Ast] -> Either DT.Text Ast
builtinLength [lst] = do
    xs <- astToList lst
    Right $ AInteger (fromInt64 (fromIntegral (length xs)))
builtinLength args =
    Left $ DT.pack $ "*** ERROR: 'length' expects one arg, got: " ++ show args
