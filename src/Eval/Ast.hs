{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Ast (Ast(..)) where

data Ast
    = AInteger Int
    | ABool Bool
    | ASymbol String
    | Define String Ast
    | DefineFun String [String] Ast -- (define f [a,b] exec)
    | Call Ast [Ast]
    deriving Show
