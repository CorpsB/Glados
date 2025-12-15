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
import qualified Data.Text as DT

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
        it "ASymbol stores text identifier" $ do
            ASymbol (DT.pack "foo") `shouldSatisfy` \case
                ASymbol s -> s == DT.pack "foo"
                _ -> False

    describe "Control Flow & Definitions" $ do
        it "Condition stores if/then/else with integers" $ do
            Condition (ABool True) (AInteger (I8 1)) (AInteger (I8 0)) `shouldSatisfy` \case
                Condition (ABool True) (AInteger (I8 1)) (AInteger (I8 0)) -> True
                _ -> False
        it "Define stores variable assignment" $ do
            Define (DT.pack "x") (DT.pack "int") (AInteger (I8 42)) `shouldSatisfy` \case
                Define name _type (AInteger (I8 42)) -> name == DT.pack "x"
                _ -> False
        it "DefineFun stores function definition" $ do
            let args = [(DT.pack "a", DT.pack "int"), (DT.pack "b", DT.pack "int")]
            let body = Call (ASymbol (DT.pack "+")) [ASymbol (DT.pack "a"), ASymbol (DT.pack "b")]
            DefineFun (DT.pack "add") args (DT.pack "int") body `shouldSatisfy` \case
                DefineFun name params _ret (Call (ASymbol op) _) -> 
                    name == DT.pack "add" && params == args && op == DT.pack "+"
                _ -> False

    describe "Function Calls" $ do
        it "Call stores function and arguments" $ do
            Call (ASymbol (DT.pack "eq?")) [AInteger (I8 1), AInteger (I8 1)] `shouldSatisfy` \case
                Call (ASymbol s) _ -> s == DT.pack "eq?"
                _ -> False
        it "Call supports empty arguments" $ do
            Call (ASymbol (DT.pack "print")) [] `shouldSatisfy` \case
                Call (ASymbol s) [] -> s == DT.pack "print"
                _ -> False

    describe "Show Instance (String representation)" $ do
        it "Correctly formats AInteger with IntValue type" $ do
            show (AInteger (I8 42)) `shouldSatisfy` (== "AInteger (I8 42)")
