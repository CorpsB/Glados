{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Functions
-}

module Eval.Functions (FuncTable, Env,
    registerFunction, getFunction, callFunction) where

import Ast (Ast(..), Env)
import Utils.List (sameLength)

type FuncTable = [(String, [String], Ast)]

registerFunction :: FuncTable -> String -> [String] -> Ast ->
    Either String FuncTable
registerFunction ftable name params body = case getFunction ftable name of
    Just _  -> Left $ "*** ERROR: Function already exists: " ++ name
    Nothing -> Right $ (name, params, body) : ftable

getFunction :: FuncTable -> String -> Maybe ([String], Ast)
getFunction [] _ = Nothing
getFunction ((n, ps, b):xs) name
    | n == name = Just (ps, b)
    | otherwise = getFunction xs name

execFunc :: (FuncTable -> Env -> Ast -> Maybe Ast) -> FuncTable -> Env ->
    ([String], Ast, [Ast], String) -> Either String Ast
execFunc evalFn ft env (p, body, args, name)
    | not (sameLength p args) = Left $ "*** ERROR: Argument length " ++
        "mismatch for function " ++ name
    | otherwise = case evalFn ft (zip p args ++ env) body of
        Just result -> Right result
        Nothing -> Left $ "*** ERROR: Function evaluation failed" ++ name

callFunction :: (FuncTable -> Env -> Ast -> Maybe Ast) ->
    FuncTable -> Env -> String -> [Ast] -> Either String Ast
callFunction evalFn ftable env name args = case getFunction ftable name of
    Nothing -> Left $ "*** ERROR: Unknown function: " ++ name
    Just (p, body) -> execFunc evalFn ftable env (p, body, args, name)
