{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Eval.Ast (Ast(..), sexprToAST, execBuiltin, evalAST) where

import Lisp (SExpr(..))

data Ast =
    Define String Ast
    | AInteger Int
    | ABool Bool
    | ASymbol String
    | Condition Ast Ast Ast
    | Call Ast [Ast]
    deriving Show

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInteger n) = Just $ AInteger n
sexprToAST (SSymbol "#t") = Just $ ABool True
sexprToAST (SSymbol "#f") = Just $ ABool False
sexprToAST (SSymbol s) = Just $ ASymbol s
sexprToAST (List (SSymbol "define" : SSymbol name : body : [])) = do
    b <- sexprToAST body
    return (Define name b)
sexprToAST (List (h:q)) = do
    h2 <- sexprToAST h
    q2 <- mapM sexprToAST q
    return (Call h2 q2)
sexprToAST _ = Nothing

toInt :: Ast -> Maybe Int
toInt (AInteger n) = Just n
toInt _ = Nothing

-- Builtins

execBuiltin :: String -> [Ast] -> Maybe Ast
execBuiltin "eq?" as = builtinEq as
execBuiltin "<" as = builtinLowerThan as
execBuiltin ">" as = builtinGreaterThan as
execBuiltin "+" as = builtinAddition as
execBuiltin "-" as = builtinSubtraction as
execBuiltin "*" as = builtinMultiplication as
execBuiltin "div" as = builtinDivision as
execBuiltin "mod" as = builtinModulo as
execBuiltin _ _ = Nothing

builtinEq :: [Ast] -> Maybe Ast
builtinEq [AInteger x, AInteger y] = Just $ ABool (x == y)
builtinEq [ABool x, ABool y] = Just $ ABool (x == y)
builtinEq _ = Nothing

builtinLowerThan :: [Ast] -> Maybe Ast
builtinLowerThan [AInteger x, AInteger y] = Just $ ABool (x < y)
builtinLowerThan _ = Nothing

builtinGreaterThan :: [Ast] -> Maybe Ast
builtinGreaterThan [AInteger x, AInteger y] = Just $ ABool (x > y)
builtinGreaterThan _ = Nothing

builtinAddition :: [Ast] -> Maybe Ast
builtinAddition [AInteger x, AInteger y] = Just $ AInteger (x + y)
builtinAddition _ = Nothing

builtinSubtraction :: [Ast] -> Maybe Ast
builtinSubtraction [AInteger x, AInteger y] = Just $ AInteger (x - y)
builtinSubtraction _ = Nothing

builtinMultiplication :: [Ast] -> Maybe Ast
builtinMultiplication [AInteger x, AInteger y] = Just $ AInteger (x * y)
builtinMultiplication _ = Nothing

builtinDivision :: [Ast] -> Maybe Ast
builtinDivision [_, AInteger 0] = Nothing
builtinDivision [AInteger x, AInteger y] = Just $ AInteger (div x y)
builtinDivision _ = Nothing

builtinModulo :: [Ast] -> Maybe Ast
builtinModulo [_, AInteger 0] = Nothing
builtinModulo [AInteger x, AInteger y] = Just $ AInteger (mod x y)
builtinModulo _ = Nothing

-- Condition

execCondition :: Ast -> Ast -> Ast -> Maybe Ast
execCondition (ABool True) th _ = Just $ th
execCondition (ABool False) _ el = Just $ el
execCondition (AInteger 1) th _ = Just $ th
execCondition (AInteger 0) _ el = Just $ el
execCondition _ _ _ = Nothing

-- Evaluation

evalAST :: Ast -> Maybe Ast
evalAST (AInteger n) = Just $ AInteger n
evalAST (ABool b) = Just $ ABool b
evalAST (ASymbol _) = Nothing
evalAST (Define name body) = do
    b2 <- evalAST body
    return (Define name b2)
evalAST (Condition cond th el) = do
    c <- evalAST cond
    t <- evalAST th
    e <- evalAST el
    execCondition c t e
evalAST (Call (ASymbol op) args) = do
    evalArgs <- mapM evalAST args
    execBuiltin op evalArgs
evalAST (Call _ _) = Nothing
