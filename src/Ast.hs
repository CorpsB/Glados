{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Ast (Ast(..), Env) where

type Env = [(String, Ast)]

data Ast
    = AInteger Int
    | ABool Bool
    | ASymbol String
    | Condition Ast Ast Ast
    | Define String Ast
    | DefineFun String [String] Ast -- (define f [a,b] exec)
    | Call Ast [Ast]
    | Lambda [String] Ast
    | Closure [String] Ast Env
    deriving Show
