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
import qualified Data.Text as DT

mockEvalSuccess :: FuncTable -> Env -> Ast -> Either DT.Text Ast
mockEvalSuccess _ _ _ = Right (AInteger (I16 1000))

mockEvalFail :: FuncTable -> Env -> Ast -> Either DT.Text Ast
mockEvalFail _ _ _ = Left (DT.pack "Mock Error")

spyForceAll :: FuncTable -> Env -> Ast -> Either DT.Text Ast
spyForceAll ft env body =
    if null ft then Left (DT.pack "Spy Error: Ftable is empty!")
    else
        case lookup (DT.pack "global") env of
            Nothing -> Left (DT.pack "Spy Error: 'global' not found in env")
            Just _ ->
                case body of
                     AInteger _ -> Right (AInteger (I16 1000))
                     _ -> Right (AInteger (I8 0))

spec :: Spec
spec = describe "Functions Management" $ do
    let emptyTable = []
    let funcName = DT.pack "testFunc"
    let params = [DT.pack "a"]
    let body = AInteger (I8 1)

    let populatedTable = [(funcName, params, body)]
    let multiTable = [(DT.pack "f1", [DT.pack "x"], AInteger (I8 10)), (DT.pack "f2", [DT.pack "y"], AInteger (I8 20))]

    let args = [AInteger (I8 2)]
    let globalEnv = [(DT.pack "global", AInteger (I8 99))] 

    describe "registerFunction" $ do
        it "registers a new function on top of existing ones (forcing body and ftable evaluation)" $ do
            registerFunction populatedTable (DT.pack "newFunc") [DT.pack "b"] (AInteger (I16 999)) `shouldSatisfy` \case
                Right ((n, p, b) : rest) ->
                    n == DT.pack "newFunc" &&
                    p == [DT.pack "b"] &&
                    (case b of AInteger (I16 999) -> True; _ -> False) &&
                    length rest == 1
                _ -> False
        it "fails if function already exists" $ do
            registerFunction populatedTable funcName params body `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Function already exists: testFunc"
                _ -> False

    describe "getFunction" $ do
        it "finds a function deeper in the list" $ do
            callFunction mockEvalSuccess multiTable [] (DT.pack "f2") [AInteger (I8 0)] `shouldSatisfy` \case
                Right _ -> True
                _ -> False

    describe "callFunction & execFunc" $ do
        it "fails for unknown function" $ do
             callFunction mockEvalSuccess emptyTable [] (DT.pack "unknown") [] `shouldSatisfy` \case
                 Left err -> err == DT.pack "*** ERROR: Unknown function: unknown"
                 _ -> False
        it "fails on argument mismatch" $ do
             callFunction mockEvalSuccess populatedTable [] funcName [] `shouldSatisfy` \case
                 Left err -> err == DT.pack "*** ERROR: Argument length mismatch for function testFunc"
                 _ -> False
        it "passes evaluated ftable, env AND body to evalFn" $ do
             callFunction spyForceAll multiTable globalEnv (DT.pack "f2") [AInteger (I8 0)] `shouldSatisfy` \case
                 Right (AInteger (I16 1000)) -> True
                 Left err -> error (DT.unpack err)
                 _ -> False
        it "propagates evaluation failure" $ do
             callFunction mockEvalFail populatedTable [] funcName args `shouldSatisfy` \case
                 Left err -> err == DT.pack "*** ERROR: Function evaluation failed: Mock Error"
                 _ -> False
