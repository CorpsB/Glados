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
import Type.Integer (IntValue(..))
import Ast (Ast(..))

mockEvalSuccess :: FuncTable -> Env -> Ast -> Either String Ast
mockEvalSuccess _ _ _ = Right (AInteger (I16 1000))

mockEvalFail :: FuncTable -> Env -> Ast -> Either String Ast
mockEvalFail _ _ _ = Left "Mock Error"

spyForceAll :: FuncTable -> Env -> Ast -> Either String Ast
spyForceAll ft env body =
    if null ft then Left "Spy Error: Ftable is empty!"
    else
        case lookup "global" env of
            Nothing -> Left "Spy Error: 'global' not found in env"
            Just _ ->
                case body of
                     AInteger _ -> Right (AInteger (I16 1000))
                     _ -> Right (AInteger (I8 0))

spec :: Spec
spec = describe "Functions Management" $ do
    let emptyTable = []
    let funcName = "testFunc"
    let params = ["a"]
    let body = AInteger (I8 1)

    let populatedTable = [(funcName, params, body)]
    let multiTable = [("f1", ["x"], AInteger (I8 10)), ("f2", ["y"], AInteger (I8 20))]

    let args = [AInteger (I8 2)]
    let globalEnv = [("global", AInteger (I8 99))] 

    describe "registerFunction" $ do
        it "registers a new function on top of existing ones (forcing body and ftable evaluation)" $ do
            registerFunction populatedTable "newFunc" ["b"] (AInteger (I16 999)) `shouldSatisfy` \case
                Right ((n, p, b) : rest) ->
                    n == "newFunc" &&
                    p == ["b"] &&
                    (case b of AInteger (I16 999) -> True; _ -> False) &&
                    length rest == 1
                _ -> False
        it "fails if function already exists" $ do
            registerFunction populatedTable funcName params body `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Function already exists: " ++ funcName
                _ -> False

    describe "getFunction" $ do
        it "finds a function deeper in the list" $ do
            callFunction mockEvalSuccess multiTable [] "f2" [AInteger (I8 0)] `shouldSatisfy` \case
                Right _ -> True
                _ -> False

    describe "callFunction & execFunc" $ do
        it "fails for unknown function" $ do
             callFunction mockEvalSuccess emptyTable [] "unknown" [] `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Unknown function: unknown"
                 _ -> False
        it "fails on argument mismatch" $ do
             callFunction mockEvalSuccess populatedTable [] funcName [] `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Argument length mismatch for function " ++ funcName
                 _ -> False
        it "passes evaluated ftable, env AND body to evalFn" $ do
             callFunction spyForceAll multiTable globalEnv "f2" [AInteger (I8 0)] `shouldSatisfy` \case
                 Right (AInteger (I16 1000)) -> True
                 Left err -> error err 
                 _ -> False
        it "propagates evaluation failure" $ do
             callFunction mockEvalFail populatedTable [] funcName args `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Function evaluation failed: Mock Error"
                 _ -> False
