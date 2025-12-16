{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Z_old.Parser.AstSpec (spec) where

import Test.Hspec
import AST.Ast (Ast(..))
import Z_old.Src.Lisp (SExpr(..))
import Z_old.Src.Parser.Ast (sexprToAST)
import Z_old.Src.Type.Integer (IntValue(..))
import qualified Data.Text as DT
import Data.List (isInfixOf)

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Parser - AST unit tests" $ do
    
    describe "Atomic types" $ do
        it "Parses Integer" $ do
            sexprToAST (SInteger 42) `shouldSatisfy` \case
                Right (AInteger (I8 42)) -> True
                _ -> False
        it "Parses Boolean #t" $ do
            sexprToAST (SSymbol (p "#t")) `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Parses Boolean #f" $ do
            sexprToAST (SSymbol (p "#f")) `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Parses Symbol" $ do
            sexprToAST (SSymbol (p "foo")) `shouldSatisfy` \case
                Right (ASymbol s) -> s == p "foo"
                _ -> False

    describe "Special Forms - Lambda" $ do
        it "Parses valid lambda" $ do
            let input = List [SSymbol (p "lambda"), List [SSymbol (p "x")], SSymbol (p "x")]
            sexprToAST input `shouldSatisfy` \case
                Right (Lambda args _) -> args == [p "x"]
                _ -> False

        it "Fails on lambda with non-symbol parameter (Lines 30-31)" $ do
            let input = List [SSymbol (p "lambda"), List [SInteger 1], SSymbol (p "x")]
            sexprToAST input `shouldSatisfy` \case
                Left err -> "Parameters must be symbols" `isInfixOf` DT.unpack err
                _ -> False

        it "Fails on malformed lambda (missing body) (Lines 56-57)" $ do
            let input = List [SSymbol (p "lambda"), List [SSymbol (p "x")]]
            sexprToAST input `shouldSatisfy` \case
                Left err -> "Invalid 'lambda' expression" `isInfixOf` DT.unpack err
                _ -> False

    describe "Special Forms - Define" $ do
        it "Parses simple define and assigns 'undefined' type (Line 63)" $ do
            let input = List [SSymbol (p "define"), SSymbol (p "x"), SInteger 42]
            sexprToAST input `shouldSatisfy` \case
                Right (Define name typeVar (AInteger (I8 42))) -> 
                    name == p "x" && typeVar == p "undefined"
                _ -> False
                
        it "Parses function define and assigns 'Any' types (Lines 71-72)" $ do
            let input = List [SSymbol (p "define"), List [SSymbol (p "f"), SSymbol (p "x")], SInteger 1]
            sexprToAST input `shouldSatisfy` \case
                Right (DefineFun name params retType _) -> 
                    name == p "f" && 
                    retType == p "Any" &&
                    params == [(p "x", p "Any")]
                _ -> False

        it "Fails on malformed define (missing value) (Lines 73-75)" $ do
            let input = List [SSymbol (p "define"), SSymbol (p "x")]
            sexprToAST input `shouldSatisfy` \case
                Left err -> "Invalid 'define' expression" `isInfixOf` DT.unpack err
                _ -> False
        
        it "Fails on malformed function define (missing body) (Lines 73-75)" $ do
            let input = List [SSymbol (p "define"), List [SSymbol (p "f"), SSymbol (p "x")]]
            sexprToAST input `shouldSatisfy` \case
                Left err -> "Invalid 'define' expression" `isInfixOf` DT.unpack err
                _ -> False

    describe "Special Forms - If & Calls" $ do
        it "Parses if condition" $ do
            let input = List [SSymbol (p "if"), SSymbol (p "#t"), SInteger 1, SInteger 0]
            sexprToAST input `shouldSatisfy` \case
                Right (Condition (ABool True) _ _) -> True
                _ -> False

        it "Parses standard call" $ do
            let input = List [SSymbol (p "+"), SInteger 1, SInteger 2]
            sexprToAST input `shouldSatisfy` \case
                Right (Call (ASymbol s) _) -> s == p "+"
                _ -> False

    describe "Edge cases" $ do
        it "Returns Error for empty list" $ do
            sexprToAST (List []) `shouldSatisfy` \case
                Left err -> "Invalid expressions" `isInfixOf` DT.unpack err
                _ -> False
