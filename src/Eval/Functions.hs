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
import qualified Data.Text as DT

type FuncTable = [(DT.Text, [DT.Text], Ast)]

registerFunction :: FuncTable -> DT.Text -> [DT.Text] -> Ast ->
    Either DT.Text FuncTable
registerFunction ftable name params body = case getFunction ftable name of
    Just _  -> Left $ DT.pack "*** ERROR: Function already exists: " <> name
    Nothing -> Right $ (name, params, body) : ftable

getFunction :: FuncTable -> DT.Text -> Maybe ([DT.Text], Ast)
getFunction [] _ = Nothing
getFunction ((n, ps, b):xs) name
    | n == name = Just (ps, b)
    | otherwise = getFunction xs name

execFunc :: (FuncTable -> Env -> Ast -> Either DT.Text Ast) -> FuncTable -> Env ->
    ([DT.Text], Ast, [Ast], DT.Text) -> Either DT.Text Ast
execFunc evalFn ft env (p, body, args, name)
    | not (sameLength p args) = Left $ DT.pack "*** ERROR: Argument length mismatch for function " <> name
    | otherwise = case evalFn ft (zip p args ++ env) body of
        Right result -> Right result
        Left err -> Left $ DT.pack "*** ERROR: Function evaluation failed: " <> err

callFunction :: (FuncTable -> Env -> Ast -> Either DT.Text Ast) ->
    FuncTable -> Env -> DT.Text -> [Ast] -> Either DT.Text Ast
callFunction evalFn ftable env name args = case getFunction ftable name of
    Nothing -> Left $ DT.pack "*** ERROR: Unknown function: " <> name
    Just (p, body) -> execFunc evalFn ftable env (p, body, args, name)
