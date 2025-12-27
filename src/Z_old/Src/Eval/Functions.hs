{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Functions
-}

module Z_old.Src.Eval.Functions (FuncTable,
    registerFunction, getFunction, callFunction) where

import Z_old.Src.Ast (OldAst(..), OldEnv)
import Z_old.Src.Utils.List (sameLength)
import qualified Data.Text as DT

type FuncTable = [(DT.Text, [DT.Text], OldAst)]

registerFunction :: FuncTable -> DT.Text -> [DT.Text] -> OldAst ->
    Either DT.Text FuncTable
registerFunction ftable name params body = case getFunction ftable name of
    Just _  -> Left $ DT.pack "*** ERROR: Function already exists: " <> name
    Nothing -> Right $ (name, params, body) : ftable

getFunction :: FuncTable -> DT.Text -> Maybe ([DT.Text], OldAst)
getFunction [] _ = Nothing
getFunction ((n, ps, b):xs) name
    | n == name = Just (ps, b)
    | otherwise = getFunction xs name

execFunc :: (FuncTable -> OldEnv -> OldAst -> Either DT.Text OldAst) ->
    FuncTable -> OldEnv -> ([DT.Text], OldAst, [OldAst], DT.Text) ->
    Either DT.Text OldAst
execFunc evalFn ft env (p, body, args, name)
    | not (sameLength p args) = Left $ DT.pack
        "*** ERROR: Argument length mismatch for function " <> name
    | otherwise = case evalFn ft (zip p args ++ env) body of
        Right result -> Right result
        Left err -> Left $ DT.pack
            "*** ERROR: Function evaluation failed: " <> err

callFunction :: (FuncTable -> OldEnv -> OldAst -> Either DT.Text OldAst) ->
    FuncTable -> OldEnv -> DT.Text -> [OldAst] -> Either DT.Text OldAst
callFunction evalFn ftable env name args = case getFunction ftable name of
    Nothing -> Left $ DT.pack "*** ERROR: Unknown function: " <> name
    Just (p, body) -> execFunc evalFn ftable env (p, body, args, name)
