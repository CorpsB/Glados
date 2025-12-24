{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Builtins
-}

module Z_old.Src.Eval.Builtins (execBuiltin) where

import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (toInt64, fromInt64)
import Z_old.Src.Utils.List (astToList, listToAst)
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

builtinAnd :: [Ast] -> Either DT.Text Ast
builtinAnd [ABool x, ABool y] = Right $ ABool (x && y)
builtinAnd args = Left $ DT.pack $ "*** ERROR: '&&' expects two " ++
    "booleans, got: " ++ show args

builtinOr :: [Ast] -> Either DT.Text Ast
builtinOr [ABool x, ABool y] = Right $ ABool (x || y)
builtinOr args = Left $ DT.pack $ "*** ERROR: '||' expects two " ++
    "booleans, got: " ++ show args

builtinNot :: [Ast] -> Either DT.Text Ast
builtinNot [ABool x] = Right $ ABool (not x)
builtinNot args = Left $ DT.pack $ "*** ERROR: '!' expects one " ++
    "boolean, got: " ++ show args

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

builtinNth :: [Ast] -> Either DT.Text Ast
builtinNth [AList list, AInteger index] =
    let idx = fromIntegral (toInt64 index)
    in if idx >= 0 && idx < length list
       then Right (list !! idx)
       else Left $ DT.pack $ "*** ERROR: Index out of bounds: " ++ show idx
builtinNth [AList _, val] = Left $ DT.pack $
    "*** ERROR: 'nth' expects an integer index, got: " ++ show val
builtinNth [val, _] = Left $ DT.pack $
    "*** ERROR: 'nth' expects a list, got: " ++ show val
builtinNth args = Left $ DT.pack $
    "*** ERROR: 'nth' expects list and index, got: " ++ show args

builtinUpdate :: [Ast] -> Either DT.Text Ast
builtinUpdate [AList list, AInteger index, val] =
    let idx = fromIntegral (toInt64 index)
    in if idx >= 0 && idx < length list
       then let (left, right) = splitAt idx list
            in case right of
                (_:rest) -> Right (AList (left ++ [val] ++ rest))
                [] -> Left $ DT.pack
                    "*** ERROR: Index out of bounds (logic error)"
       else Left $ DT.pack $ "*** ERROR: Index out of bounds: " ++ show idx
builtinUpdate args = Left $ DT.pack $
    "*** ERROR: 'update' expects [list, index, value], got: " ++ show args

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
    , (DT.pack "&&", builtinAnd)
    , (DT.pack "||", builtinOr)
    , (DT.pack "!", builtinNot)
    ]

listCreationOps :: [(DT.Text, [Ast] -> Either DT.Text Ast)]
listCreationOps =
    [ (DT.pack "list", builtinList)
    , (DT.pack "cons", builtinCons)
    , (DT.pack "append", builtinAppend)
    , (DT.pack "update", builtinUpdate)
    ]

listQueryOps :: [(DT.Text, [Ast] -> Either DT.Text Ast)]
listQueryOps =
    [ (DT.pack "car", builtinCar)
    , (DT.pack "cdr", builtinCdr)
    , (DT.pack "list?", builtinIsList)
    , (DT.pack "length", builtinLength)
    , (DT.pack "nth", builtinNth)
    ]

listOps :: [(DT.Text, [Ast] -> Either DT.Text Ast)]
listOps = listCreationOps ++ listQueryOps

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
