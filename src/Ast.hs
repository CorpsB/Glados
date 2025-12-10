{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Ast (Ast(..), Env) where

import Type.Integer (IntValue(..))
import Data.Text as DT

type Env = [(DT.Text, Ast)]

data Ast
    = AInteger IntValue
    | ABool Bool
    | ASymbol DT.Text
    | AVoid
    | Condition Ast Ast Ast
    | Define DT.Text Ast
    | DefineFun DT.Text [DT.Text] Ast -- (define f [a,b] exec)
    | Call Ast [Ast]
    | Lambda [DT.Text] Ast
    | Closure [DT.Text] Ast Env
    | AList [Ast]
    | Import DT.Text
    deriving Show
