{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Functions
-}

import Ast (Ast(..), sexprToAST, execBuiltin, evalAST)

-- TO REFACTO (DefineFun)
registerFunction :: String -> [Ast] -> [Define] -> [Define]
registerFunction name value stack = ((Define name value):stack)

getFunctionValue :: String -> [Define] -> Maybe [Ast]
getFunctionValue _ [] = Nothing
getFunctionValue target ((Define key value):q)
    | key == target = Just value
    | otherwise     = getFunctionValue target q

execFunction :: Maybe [Ast] -> [Ast] -> Maybe Ast
execFunction (Just pattern) args = -- TO DO

callFunction :: Call -> Maybe Ast
callFunction (Call name args)
    | value == Nothing = Nothing
    | otherwise        = execFunction value args
    where value = getFunctionValue name
