{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Parser.AstSpec (spec) where

import Test.Hspec
import Ast (Ast(..))
import Lisp (SExpr(..))
import Parser.Ast (sexprToAST)
import Type.Integer (IntValue(..))

spec :: Spec
spec = describe "Parser - AST unit tests" $ do
    describe "Atomic types" $ do
        it "Parses Integer" $ do
            sexprToAST (SInteger 42) `shouldSatisfy` \case
                Right (AInteger (I8 42)) -> True
                _ -> False
        it "Parses Boolean #t" $ do
            sexprToAST (SSymbol "#t") `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Parses Boolean #f" $ do
            sexprToAST (SSymbol "#f") `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Parses Symbol" $ do
            sexprToAST (SSymbol "foo") `shouldSatisfy` \case
                Right (ASymbol "foo") -> True
                _ -> False

    describe "Special Forms - Lambda" $ do
        it "Parses valid lambda" $ do
            let input = List [SSymbol "lambda", List [SSymbol "x", SSymbol "y"], List [SSymbol "+", SSymbol "x", SSymbol "y"]]
            sexprToAST input `shouldSatisfy` \case
                Right (Lambda ["x", "y"] (Call (ASymbol "+") [ASymbol "x", ASymbol "y"])) -> True
                _ -> False
        it "Fails on invalid lambda params" $ do
            let input = List [SSymbol "lambda", List [SInteger 1], SSymbol "x"]
            sexprToAST input `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Special Forms - Define" $ do
        it "Parses simple define" $ do
            let input = List [SSymbol "define", SSymbol "x", SInteger 42]
            sexprToAST input `shouldSatisfy` \case
                Right (Define "x" (AInteger (I8 42))) -> True
                _ -> False
        it "Parses function define" $ do
            let input = List [SSymbol "define", List [SSymbol "f", SSymbol "x"], List [SSymbol "+", SSymbol "x", SInteger 1]]
            sexprToAST input `shouldSatisfy` \case
                Right (DefineFun "f" ["x"] (Call (ASymbol "+") [ASymbol "x", AInteger (I8 1)])) -> True
                _ -> False
        it "Fails define function with invalid params" $ do
            let input = List [SSymbol "define", List [SSymbol "f", SInteger 1], SSymbol "x"]
            sexprToAST input `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Special Forms - If" $ do
        it "Parses if condition" $ do
            let input = List [SSymbol "if", SSymbol "#t", SInteger 1, SInteger 0]
            sexprToAST input `shouldSatisfy` \case
                Right (Condition (ABool True) (AInteger (I8 1)) (AInteger (I8 0))) -> True
                _ -> False

    describe "Function Calls" $ do
        it "Parses standard call" $ do
            let input = List [SSymbol "+", SInteger 1, SInteger 2]
            sexprToAST input `shouldSatisfy` \case
                Right (Call (ASymbol "+") [AInteger (I8 1), AInteger (I8 2)]) -> True
                _ -> False
        it "Propagates errors in call arguments" $ do
            let invalidLambda = List [SSymbol "lambda", List [SInteger 1], SSymbol "x"]
            let input = List [SSymbol "+", invalidLambda]
            sexprToAST input `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Edge cases" $ do
        it "Returns Nothing for empty list" $ do
            sexprToAST (List []) `shouldSatisfy` \case
                Left _ -> True
                _ -> False
