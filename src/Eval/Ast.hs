{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Ast (Ast(..), sexprToAST, evalAST) where

import Lisp (SExpr(..))

data Ast = Define String Ast
    | AInteger Int
    | ASymbol String
    | Call Ast [Ast]
    deriving Show

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInteger n) = Just (AInteger n)
sexprToAST (SSymbol s) = Just (ASymbol s)
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

execBuiltin :: String -> [Ast] -> Maybe Ast
execBuiltin "+" as = do
    numbers <- traverse toInt as
    return (AInteger (sum numbers))
execBuiltin "*" as = do
    numbers <- traverse toInt as
    return (AInteger (product numbers))
execBuiltin "-" as = do
    numbers <- traverse toInt as
    case numbers of
        (x:xs) -> return (AInteger (foldl (-) x xs))
        _ -> Nothing
execBuiltin "/" as = do
    numbers <- traverse toInt as
    case numbers of
        (x:xs) -> if all (/= 0) xs
                  then return (AInteger (foldl div x xs))
                  else Nothing
        _ -> Nothing
execBuiltin ">" as = do
    numbers <- traverse toInt as
    case numbers of
        (a:b:_) -> return (AInteger (if a > b then 1 else 0))
        _ -> Nothing
execBuiltin _ _ = Nothing

evalAST :: Ast -> Maybe Ast
evalAST (AInteger n) = Just (AInteger n)
evalAST (ASymbol _) = Nothing
evalAST (Define name body) = do
    b2 <- evalAST body
    return (Define name b2)
evalAST (Call (ASymbol op) args) = do
    evalArgs <- mapM evalAST args
    execBuiltin op evalArgs
evalAST (Call _ _) = Nothing
