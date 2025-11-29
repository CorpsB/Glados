{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Ast (Ast(..), IntValue(..), Env, intValueToInteger, intValueIsZero) where

import Data.Int (Int8, Int16, Int32, Int64)

data IntValue
    = I8 Int8
    | I16 Int16
    | I32 Int32
    | I64 Int64
    deriving (Show, Eq, Ord)

intValueToInteger :: IntValue -> Integer
intValueToInteger (I8 v)  = fromIntegral v
intValueToInteger (I16 v) = fromIntegral v
intValueToInteger (I32 v) = fromIntegral v
intValueToInteger (I64 v) = fromIntegral v

intValueIsZero :: IntValue -> Bool
intValueIsZero v = intValueToInteger v == 0

type Env = [(String, Ast)]

data Ast
    = AInteger IntValue
    | ABool Bool
    | ASymbol String
    | Condition Ast Ast Ast
    | Define String Ast
    | DefineFun String [String] Ast -- (define f [a,b] exec)
    | Call Ast [Ast]
    | Lambda [String] Ast
    | Closure [String] Ast Env
    deriving Show
