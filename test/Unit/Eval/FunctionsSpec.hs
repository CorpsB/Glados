{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- FunctionsSpec
-}

{-# LANGUAGE LambdaCase #-}

module Eval.FunctionsSpec (spec) where

import Test.Hspec
import Eval.Functions
import Ast (Ast(..))

mockEvalSuccess :: FuncTable -> Env -> Ast -> Either String Ast
mockEvalSuccess _ _ _ = Right (AInteger 1000)

mockEvalFail :: FuncTable -> Env -> Ast -> Either String Ast
mockEvalFail _ _ _ = Left "Mock Error"

spyEval :: FuncTable -> Env -> Ast -> Either String Ast
spyEval _ _ _ = Right (AInteger 1000)

spec :: Spec
spec = describe "Functions Management" $ do
    let emptyTable = []
    let funcName = "testFunc"
    let params = ["a"]
    let body = AInteger 1
    let populatedTable = [(funcName, params, body)]
    let args = [AInteger 2]
    let globalEnv = []

    describe "registerFunction" $ do
        it "registers a new function" $ do
            registerFunction emptyTable funcName params body `shouldSatisfy` \case
                Right ((n, p, _):_) -> n == funcName && p == params
                _ -> False

        it "fails if function already exists" $ do
            registerFunction populatedTable funcName params body `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "callFunction" $ do
        it "fails for unknown function" $ do
             callFunction mockEvalSuccess emptyTable [] "unknown" [] `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Unknown function: unknown"
                 _ -> False

        it "fails on argument mismatch" $ do
             callFunction mockEvalSuccess populatedTable [] funcName [] `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Argument length mismatch for function " ++ funcName
                 _ -> False

        it "executes function successfully" $ do
             callFunction spyEval populatedTable globalEnv funcName args `shouldSatisfy` \case
                 Right (AInteger 1000) -> True
                 _ -> False

        it "propagates evaluation failure" $ do
             callFunction mockEvalFail populatedTable [] funcName args `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Function evaluation failed: Mock Error"
                 _ -> False
