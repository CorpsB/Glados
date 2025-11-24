{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Builtins
-}

module Eval.Builtins (execBuiltin) where

import Ast (Ast(..))

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
