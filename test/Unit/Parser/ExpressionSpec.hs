{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ExpressionSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ExpressionSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse)
import Parser.Expression (pExpr)
import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (IntValue(..))
import qualified Data.Text as DT
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)

parseExpr :: DT.Text -> Either (ParseErrorBundle DT.Text Void) Ast
parseExpr input = parse pExpr "" input

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Parser.Expression - Full Coverage" $ do

    describe "Booleans" $ do
        it "Parses True" $ do
            parseExpr "True" `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        
        it "Parses False (Coverage Target)" $ do
            parseExpr "False" `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False

    describe "Parentheses & Priority" $ do
        it "Parses parenthesized expression" $ do
            parseExpr "(42)" `shouldSatisfy` \case
                Right (AInteger (I8 42)) -> True
                _ -> False

        it "Parses nested parentheses" $ do
            parseExpr "((10))" `shouldSatisfy` \case
                Right (AInteger (I8 10)) -> True
                _ -> False

    describe "Operators Coverage" $ do
        
        it "Parses Division (/ -> div)" $ do
            parseExpr "10 / 2" `shouldSatisfy` \case
                Right (Call (ASymbol op) [AInteger (I8 10), AInteger (I8 2)]) -> op == p "div"
                _ -> False

        it "Parses Modulo (% -> mod)" $ do
            parseExpr "10 % 3" `shouldSatisfy` \case
                Right (Call (ASymbol op) [AInteger (I8 10), AInteger (I8 3)]) -> op == p "mod"
                _ -> False

        it "Parses Subtraction (-)" $ do
            parseExpr "10 - 5" `shouldSatisfy` \case
                Right (Call (ASymbol op) [AInteger (I8 10), AInteger (I8 5)]) -> op == p "-"
                _ -> False

        it "Parses Equality (== -> eq?)" $ do
            parseExpr "1 == 1" `shouldSatisfy` \case
                Right (Call (ASymbol op) [AInteger (I8 1), AInteger (I8 1)]) -> op == p "eq?"
                _ -> False

        it "Parses Less Than (<)" $ do
            parseExpr "1 < 2" `shouldSatisfy` \case
                Right (Call (ASymbol op) [AInteger (I8 1), AInteger (I8 2)]) -> op == p "<"
                _ -> False
        
        it "Parses Greater Than (>)" $ do
            parseExpr "2 > 1" `shouldSatisfy` \case
                Right (Call (ASymbol op) [AInteger (I8 2), AInteger (I8 1)]) -> op == p ">"
                _ -> False

    describe "Other Types (Strings, Chars, Lists)" $ do
        it "Parses String" $ do
            parseExpr "\"abc\"" `shouldSatisfy` \case
                Right (AList _) -> True
                _ -> False

        it "Parses Char" $ do
            parseExpr "'c'" `shouldSatisfy` \case
                Right (AInteger (IChar 'c')) -> True
                _ -> False

        it "Parses List Literal" $ do
            parseExpr "[1, 2]" `shouldSatisfy` \case
                Right (AList _) -> True
                _ -> False
        
        it "Parses Function Call" $ do
            parseExpr "foo(1)" `shouldSatisfy` \case
                Right (Call (ASymbol s) _) -> s == p "foo"
                _ -> False
