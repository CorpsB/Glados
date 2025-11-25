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

spec :: Spec
spec = describe "AST - Data Structure" $ do
    describe "Basic Values" $ do
        it "AInteger stores integer value" $ do
            AInteger 42 `shouldSatisfy` \case
                AInteger 42 -> True
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
        it "Condition stores if/then/else" $ do
            Condition (ABool True) (AInteger 1) (AInteger 0) `shouldSatisfy` \case
                Condition (ABool True) (AInteger 1) (AInteger 0) -> True
                _ -> False
        it "Define stores variable assignment" $ do
            Define "x" (AInteger 42) `shouldSatisfy` \case
                Define "x" (AInteger 42) -> True
                _ -> False
        it "DefineFun stores function definition" $ do
            let args = ["a", "b"]
            let body = Call (ASymbol "+") [ASymbol "a", ASymbol "b"]
            DefineFun "add" args body `shouldSatisfy` \case
                DefineFun "add" ["a", "b"] (Call (ASymbol "+") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False
    describe "Function Calls" $ do
        it "Call stores function and arguments" $ do
            Call (ASymbol "eq?") [AInteger 1, AInteger 1] `shouldSatisfy` \case
                Call (ASymbol "eq?") [AInteger 1, AInteger 1] -> True
                _ -> False
        it "Call supports empty arguments" $ do
            Call (ASymbol "print") [] `shouldSatisfy` \case
                Call (ASymbol "print") [] -> True
                _ -> False
    describe "Show Instance (String representation)" $ do
        it "Correctly formats AInteger" $ do
            show (AInteger 42) `shouldSatisfy` (== "AInteger 42")
        it "Correctly formats Call with list" $ do
            show (Call (ASymbol "test") [ABool True]) `shouldSatisfy` (== "Call (ASymbol \"test\") [ABool True]")
