{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- FunctionsSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Eval.FunctionsSpec (spec) where

import Test.Hspec
import Ast (Ast(..))
import Eval.Functions (registerFunction, getFunction, callFunction, FuncTable, Env)

lookupTest :: String -> Env -> Maybe Ast
lookupTest _ [] = Nothing
lookupTest key ((k,v):xs)
    | k == key = Just v
    | otherwise = lookupTest key xs

mockEvalSuccess :: FuncTable -> Env -> Ast -> Maybe Ast
mockEvalSuccess _ _ _ = Just (AInteger 42)

mockEvalFail :: FuncTable -> Env -> Ast -> Maybe Ast
mockEvalFail _ _ _ = Nothing

spyEval :: FuncTable -> Env -> Ast -> Maybe Ast
spyEval ft env body = 
    case (lookupTest "x" env, lookupTest "glob" env, body, length ft) of
        (Just (AInteger 10), Just (AInteger 99), AInteger 1, len) | len > 0 -> Just (AInteger 1000)
        _ -> Nothing

spec :: Spec
spec = describe "Eval - Functions unit tests" $ do
    let funcBody = AInteger 1
    let funcName = "myFunc"
    let params = ["x"]
    let emptyTable = [] :: FuncTable
    let otherFn = ("other", [], AInteger 0)
    let populatedTable = [(funcName, params, funcBody), otherFn] :: FuncTable
    let globalEnv = [("glob", AInteger 99)]

    describe "registerFunction" $ do
        it "Registers a new function and preserves existing table (Right Nothing args check)" $ do
            let startTable = [otherFn]
            registerFunction startTable funcName params funcBody `shouldSatisfy` \case
                Right ((n, p, b):rest) -> 
                    n == funcName && 
                    p == params && 
                    (case b of AInteger 1 -> True; _ -> False) &&
                    length rest == 1
                _ -> False
        it "Fails to register if function already exists" $ do
            registerFunction populatedTable funcName ["z"] (AInteger 2) `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Function already exists: " ++ funcName
                _ -> False

    describe "getFunction" $ do
        it "Returns Nothing from empty table" $ do
            getFunction emptyTable "anything" `shouldSatisfy` \case
                Nothing -> True
                _ -> False
        it "Returns function params and EXACT body if found (checking b in Just(ps, b))" $ do
            getFunction populatedTable funcName `shouldSatisfy` \case
                Just (p, AInteger 1) -> p == params
                _ -> False
        it "Recursively searches the table" $ do
            getFunction populatedTable "other" `shouldSatisfy` \case
                Just ([], AInteger 0) -> True
                _ -> False

    describe "callFunction (integration with execFunc)" $ do
        it "Fails if function does not exist" $ do
            callFunction mockEvalSuccess emptyTable [] "unknown" [] `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Unknown function: unknown"
                _ -> False
        it "Fails if argument length mismatch" $ do
            callFunction mockEvalSuccess populatedTable [] funcName [] `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Argument length mismatch for function " ++ funcName
                _ -> False
        it "Passes correct Env, FT and Body to EvalFn (checking execFunc args)" $ do
            let args = [AInteger 10]
            callFunction spyEval populatedTable globalEnv funcName args `shouldSatisfy` \case
                Right (AInteger 1000) -> True
                _ -> False
        it "Handles EvalFn failure" $ do
            let args = [AInteger 10]
            callFunction mockEvalFail populatedTable [] funcName args `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Function evaluation failed" ++ funcName
                _ -> False
