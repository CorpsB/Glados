{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParserISLTest.hs
-}

{-# LANGUAGE LambdaCase #-}

module Parser.ParserISLTest (spec) where

import Test.Hspec
import Parser.ParserISL (parseLisp)
import Lisp (SExpr(..))

spec :: Spec
spec = describe "Parser LISP - SExpr Conversion" $ do

    describe "Integers" $ do
      it "parses positive integer" $ do
        parseLisp "42" `shouldSatisfy` \case
            Right (SInteger 42) -> True
            _ -> False

      it "parses negative integer" $ do
        parseLisp "-42" `shouldSatisfy` \case
            Right (SInteger (-42)) -> True
            _ -> False

      it "parses large integer" $ do
        parseLisp "123456789" `shouldSatisfy` \case
            Right (SInteger 123456789) -> True
            _ -> False

    describe "Symbols" $ do
      it "parses simple symbols" $ do
        parseLisp "foo" `shouldSatisfy` \case
            Right (SSymbol "foo") -> True
            _ -> False

      it "parses operators" $ do
        parseLisp ">=" `shouldSatisfy` \case
            Right (SSymbol ">=") -> True
            _ -> False

    describe "Lists" $ do
      it "parses empty list" $ do
        parseLisp "()" `shouldSatisfy` \case
            Right (List []) -> True
            _ -> False

      it "parses simple list (+ 1 2)" $ do
        parseLisp "(+ 1 2)" `shouldSatisfy` \case
            Right (List [SSymbol "+", SInteger 1, SInteger 2]) -> True
            _ -> False

    describe "Error Handling" $ do
      it "fails on empty input" $ do
        parseLisp "" `shouldSatisfy` \case
            Left _ -> True
            _ -> False

      it "fails on unclosed parenthesis" $ do
        parseLisp "(+ 1 2" `shouldSatisfy` \case
            Left _ -> True
            _ -> False
