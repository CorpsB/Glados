{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Functions
-}

module Eval.Functions (registerFunction, getFunction, callFunction) where

import Ast (Ast(..))
import Utils.List (sameLength)

type FuncTable = [(String, [String], Ast)]
type Env = [(String, Ast)]

registerFunction :: FuncTable -> String -> [String] -> Ast -> Maybe FuncTable
registerFunction ftable name params body = case getFunction ftable name of
    Just _  -> Nothing
    Nothing -> Just ((name, params, body) : ftable)

getFunction :: FuncTable -> String -> Maybe ([String], Ast)
getFunction [] _ = Nothing
getFunction ((n, ps, b):xs) name
    | n == name = Just (ps, b)
    | otherwise = getFunction xs name

callFunction :: FuncTable -> Env -> String -> [Ast] -> Maybe Ast
callFunction ftable env name args = case getFunction ftable name of
    Nothing -> Nothing
    Just (params, body)
        | not (Utils.List.sameLength params args) -> Nothing
        | otherwise -> let localEnv = zip params args ++ env in
            evalWithEnv ftable localEnv body
