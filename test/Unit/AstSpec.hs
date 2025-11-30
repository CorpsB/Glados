{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module AstSpec (spec) where

import Test.Hspec
import Ast (Ast(..))
import Type.Integer (IntValue(..))

spec :: Spec
spec = describe "AST - Data Structure" $ do
    describe "Basic Values & Integer Types" $ do
        it "AInteger stores Int8 (Small values)" $ do
            AInteger (I8 42) `shouldSatisfy` \case
                AInteger (I8 42) -> True
                _ -> False
        it "AInteger stores Int16 (Medium values)" $ do
            AInteger (I16 300) `shouldSatisfy` \case
                AInteger (I16 300) -> True
                _ -> False
        it "AInteger stores Int32 (Large values)" $ do
            AInteger (I32 70000) `shouldSatisfy` \case
                AInteger (I32 70000) -> True
                _ -> False
        it "AInteger stores Int64 (Huge values)" $ do
            AInteger (I64 5000000000) `shouldSatisfy` \case
                AInteger (I64 5000000000) -> True
                _ -> False
        it "ABool stores boolean value" $ do
            ABool True `shouldSatisfy` \case
                ABool True -> True
                _ -> False
        it "ASymbol stores string identifier" $ do
            ASymbol "foo" `shouldSatisfy` \case
                ASymbol "foo" -> True
                _ -> False

    describe "Control Flow & Definitions" $ do
        it "Condition stores if/then/else with integers" $ do
            Condition (ABool True) (AInteger (I8 1)) (AInteger (I8 0)) `shouldSatisfy` \case
                Condition (ABool True) (AInteger (I8 1)) (AInteger (I8 0)) -> True
                _ -> False
        it "Define stores variable assignment" $ do
            Define "x" (AInteger (I8 42)) `shouldSatisfy` \case
                Define "x" (AInteger (I8 42)) -> True
                _ -> False
        it "DefineFun stores function definition" $ do
            let args = ["a", "b"]
            let body = Call (ASymbol "+") [ASymbol "a", ASymbol "b"]
            DefineFun "add" args body `shouldSatisfy` \case
                DefineFun "add" ["a", "b"] (Call (ASymbol "+") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False

    describe "Function Calls" $ do
        it "Call stores function and arguments" $ do
            Call (ASymbol "eq?") [AInteger (I8 1), AInteger (I8 1)] `shouldSatisfy` \case
                Call (ASymbol "eq?") [AInteger (I8 1), AInteger (I8 1)] -> True
                _ -> False
        it "Call supports empty arguments" $ do
            Call (ASymbol "print") [] `shouldSatisfy` \case
                Call (ASymbol "print") [] -> True
                _ -> False

    describe "Show Instance (String representation)" $ do
        it "Correctly formats AInteger with IntValue type" $ do
            show (AInteger (I8 42)) `shouldSatisfy` (== "AInteger (I8 42)")
        it "Correctly formats Call with mixed types" $ do
            let call = Call (ASymbol "test") [ABool True, AInteger (I16 255)]
            show call `shouldSatisfy` (== "Call (ASymbol \"test\") [ABool True,AInteger (I16 255)]")
