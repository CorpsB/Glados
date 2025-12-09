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
import qualified Data.Text as DT

spec :: Spec
spec = describe "Parser - AST unit tests" $ do
    describe "Atomic types" $ do
        it "Parses Integer" $ do
            sexprToAST (SInteger 42) `shouldSatisfy` \case
                Right (AInteger (I8 42)) -> True
                _ -> False
        it "Parses Boolean #t" $ do
            sexprToAST (SSymbol (DT.pack "#t")) `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Parses Boolean #f" $ do
            sexprToAST (SSymbol (DT.pack "#f")) `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Parses Symbol" $ do
            sexprToAST (SSymbol (DT.pack "foo")) `shouldSatisfy` \case
                Right (ASymbol s) -> s == DT.pack "foo"
                _ -> False

    describe "Special Forms - Lambda" $ do
        it "Parses valid lambda" $ do
            let input = List [SSymbol (DT.pack "lambda"), List [SSymbol (DT.pack "x"), SSymbol (DT.pack "y")], List [SSymbol (DT.pack "+"), SSymbol (DT.pack "x"), SSymbol (DT.pack "y")]]
            sexprToAST input `shouldSatisfy` \case
                Right (Lambda args (Call (ASymbol op) _)) -> args == [DT.pack "x", DT.pack "y"] && op == DT.pack "+"
                _ -> False
        it "Fails on invalid lambda params" $ do
            let input = List [SSymbol (DT.pack "lambda"), List [SInteger 1], SSymbol (DT.pack "x")]
            sexprToAST input `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Special Forms - Define" $ do
        it "Parses simple define" $ do
            let input = List [SSymbol (DT.pack "define"), SSymbol (DT.pack "x"), SInteger 42]
            sexprToAST input `shouldSatisfy` \case
                Right (Define name (AInteger (I8 42))) -> name == DT.pack "x"
                _ -> False
        it "Parses function define" $ do
            let input = List [SSymbol (DT.pack "define"), List [SSymbol (DT.pack "f"), SSymbol (DT.pack "x")], List [SSymbol (DT.pack "+"), SSymbol (DT.pack "x"), SInteger 1]]
            sexprToAST input `shouldSatisfy` \case
                Right (DefineFun name params _) -> name == DT.pack "f" && params == [DT.pack "x"]
                _ -> False
        it "Fails define function with invalid params" $ do
            let input = List [SSymbol (DT.pack "define"), List [SSymbol (DT.pack "f"), SInteger 1], SSymbol (DT.pack "x")]
            sexprToAST input `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Special Forms - If" $ do
        it "Parses if condition" $ do
            let input = List [SSymbol (DT.pack "if"), SSymbol (DT.pack "#t"), SInteger 1, SInteger 0]
            sexprToAST input `shouldSatisfy` \case
                Right (Condition (ABool True) (AInteger (I8 1)) (AInteger (I8 0))) -> True
                _ -> False

    describe "Function Calls" $ do
        it "Parses standard call" $ do
            let input = List [SSymbol (DT.pack "+"), SInteger 1, SInteger 2]
            sexprToAST input `shouldSatisfy` \case
                Right (Call (ASymbol s) [AInteger (I8 1), AInteger (I8 2)]) -> s == DT.pack "+"
                _ -> False
        it "Propagates errors in call arguments" $ do
            let invalidLambda = List [SSymbol (DT.pack "lambda"), List [SInteger 1], SSymbol (DT.pack "x")]
            let input = List [SSymbol (DT.pack "+"), invalidLambda]
            sexprToAST input `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Edge cases" $ do
        it "Returns Nothing for empty list" $ do
            sexprToAST (List []) `shouldSatisfy` \case
                Left _ -> True
                _ -> False
