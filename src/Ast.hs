{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Ast (Ast(..), Env) where

import Type.Integer (IntValue(..))

type Env = [(String, Ast)]

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | Less
    deriving (Show, Eq)

data Ast
    = AInteger IntValue
    | ABool Bool
    | ASymbol String
    | AString String
    | AChar Char
    | AVoid
    | BinaryOp BinOp Ast Ast
    | Condition Ast Ast Ast
    | Define String Ast
    | DefineFun String [String] Ast -- (define f [a,b] exec)
    | Call Ast [Ast]
    | Lambda [String] Ast
    | Closure [String] Ast Env
    | AList [Ast]
    | Import String
    deriving Show
