{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParserISLTest.hs
-}

{-# LANGUAGE LambdaCase #-}

module Z_old.Parser.ParserISLTest (spec) where

import Test.Hspec
import Z_old.Src.Parser.ParserISL (parseLisp, parseLispLine)
import Z_old.Src.Lisp (SExpr(..))
import qualified Data.Text as DT

spec :: Spec
spec = describe "Parser LISP - SExpr Conversion" $ do

    describe "Integers" $ do
      it "parses positive integer" $ do
        parseLisp (DT.pack "42") `shouldSatisfy` \case
            Right [SInteger 42] -> True 
            _ -> False

      it "parses negative integer" $ do
        parseLisp (DT.pack "-42") `shouldSatisfy` \case
            Right [SInteger (-42)] -> True
            _ -> False

      it "parses large integer" $ do
        parseLisp (DT.pack "123456789") `shouldSatisfy` \case
            Right [SInteger 123456789] -> True
            _ -> False

    describe "Symbols" $ do
      it "parses simple symbols" $ do
        parseLisp (DT.pack "foo") `shouldSatisfy` \case
            Right [SSymbol s] -> s == DT.pack "foo"
            _ -> False

      it "parses operators" $ do
        parseLisp (DT.pack ">=") `shouldSatisfy` \case
            Right [SSymbol s] -> s == DT.pack ">="
            _ -> False

    describe "Lists" $ do
      it "parses empty list" $ do
        parseLisp (DT.pack "()") `shouldSatisfy` \case
            Right [List []] -> True
            _ -> False

      it "parses simple list (+ 1 2)" $ do
        parseLisp (DT.pack "(+ 1 2)") `shouldSatisfy` \case
            Right [List [SSymbol op, SInteger 1, SInteger 2]] -> op == DT.pack "+"
            _ -> False

    describe "Error Handling" $ do
      it "returns empty list on empty input" $ do
        parseLisp (DT.pack "") `shouldSatisfy` \case
            Right [] -> True
            _ -> False

      it "fails on unclosed parenthesis" $ do
        parseLisp (DT.pack "(+ 1 2") `shouldSatisfy` \case
            Left _ -> True
            _ -> False
      
    describe "parseLispLine (REPL Mode)" $ do
      it "parses a single integer" $ do
        parseLispLine (DT.pack "42") `shouldSatisfy` \case
            Right (SInteger 42) -> True
            _ -> False

      it "parses a single list" $ do
        parseLispLine (DT.pack "(+ 1 2)") `shouldSatisfy` \case
            Right (List [SSymbol op, SInteger 1, SInteger 2]) -> op == DT.pack "+"
            _ -> False

      it "ignores surrounding spaces" $ do
        parseLispLine (DT.pack "   foo   ") `shouldSatisfy` \case
            Right (SSymbol s) -> s == DT.pack "foo"
            _ -> False

      it "fails on incomplete input" $ do
        parseLispLine (DT.pack "(") `shouldSatisfy` \case
            Left _ -> True
            _ -> False
