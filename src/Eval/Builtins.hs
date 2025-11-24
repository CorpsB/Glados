{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Builtins
-}

module Eval.Builtins (execBuiltin) where

import Ast (Ast(..))

builtinEq :: [Ast] -> Either String Ast
builtinEq [AInteger x, AInteger y] = Right $ ABool (x == y)
builtinEq [ABool x, ABool y] = Right $ ABool (x == y)
builtinEq args = Left $ "*** ERROR: 'eq?' expects two " ++
    "integers or two booleans, got: " ++ show args

builtinLowerThan :: [Ast] -> Either String Ast
builtinLowerThan [AInteger x, AInteger y] = Right $ ABool (x < y)
builtinLowerThan args = Left $ "*** ERROR: '<' expects two " ++
    "integers, got: " ++ show args

builtinGreaterThan :: [Ast] -> Either String Ast
builtinGreaterThan [AInteger x, AInteger y] = Right $ ABool (x > y)
builtinGreaterThan args = Left $ "*** ERROR: '>' expects two " ++
    "integers, got: " ++ show args

builtinAddition :: [Ast] -> Either String Ast
builtinAddition [AInteger x, AInteger y] = Right $ AInteger (x + y)
builtinAddition args = Left $ "*** ERROR: '+' expects two " ++
    "integers, got: " ++ show args

builtinSubtraction :: [Ast] -> Either String Ast
builtinSubtraction [AInteger x, AInteger y] = Right $ AInteger (x - y)
builtinSubtraction args = Left $ "*** ERROR: '-' expects two " ++
    "integers, got: " ++ show args

builtinMultiplication :: [Ast] -> Either String Ast
builtinMultiplication [AInteger x, AInteger y] = Right $ AInteger (x * y)
builtinMultiplication args = Left $ "*** ERROR: '*' expects two " ++
    "integers, got: " ++ show args

builtinDivision :: [Ast] -> Either String Ast
builtinDivision [_, AInteger 0] = Left $ "*** ERROR: 'div' division by zero"
builtinDivision [AInteger x, AInteger y] = Right $ AInteger (div x y)
builtinDivision args = Left $ "*** ERROR: 'div' expects two " ++
    "integers, got: " ++ show args

builtinModulo :: [Ast] -> Either String Ast
builtinModulo [_, AInteger 0] = Left $ "*** ERROR: 'mod' division by zero"
builtinModulo [AInteger x, AInteger y] = Right $ AInteger (mod x y)
builtinModulo args = Left $ "*** ERROR: 'mod' expects two " ++
    "integers, got: " ++ show args

execBuiltin :: String -> [Ast] -> Either String Ast
execBuiltin "eq?" as = builtinEq as
execBuiltin "<" as = builtinLowerThan as
execBuiltin ">" as = builtinGreaterThan as
execBuiltin "+" as = builtinAddition as
execBuiltin "-" as = builtinSubtraction as
execBuiltin "*" as = builtinMultiplication as
execBuiltin "div" as = builtinDivision as
execBuiltin "mod" as = builtinModulo as
execBuiltin op _ = Left $ "*** ERROR: Unknown builtin: " ++ op
