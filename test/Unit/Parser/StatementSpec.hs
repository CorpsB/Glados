{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- StatementSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.StatementSpec (spec) where

import Test.Hspec
import Parser.Statement (parseALL)
import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (IntValue(..))
import qualified Data.Text as DT
import Data.List (isInfixOf)
import Text.Megaparsec.Error (errorBundlePretty)

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Parser C-Style - Statement & Expression" $ do

    describe "Literals & Types" $ do
        
        it "Parses integers" $ do
            let code = "42;"
            parseALL (p code) `shouldSatisfy` \case
                Right [AInteger (I8 42)] -> True
                _ -> False

        it "Parses booleans (True)" $ do
            let code = "True;"
            parseALL (p code) `shouldSatisfy` \case
                Right [ABool True] -> True
                _ -> False

        it "Parses characters ('c')" $ do
            let code = "'c';"
            parseALL (p code) `shouldSatisfy` \case
                Right [AInteger (IChar 'c')] -> True
                _ -> False

        it "Parses strings as list of chars" $ do
            let code = "\"Hi\";"
            parseALL (p code) `shouldSatisfy` \case
                Right [AList [AInteger (IChar 'H'), AInteger (IChar 'i')]] -> True
                _ -> False

        it "Parses list literals" $ do
            let code = "[1, 2];"
            parseALL (p code) `shouldSatisfy` \case
                Right [AList [AInteger (I8 1), AInteger (I8 2)]] -> True
                _ -> False

    describe "Edge Cases & Error Handling (Coverage Target)" $ do

        it "Fails on empty function body with specific error message" $ do
            let code = "func fail() {}"
            parseALL (p code) `shouldSatisfy` \case
                Left err -> 
                    "Empty function body not supported yet" `isInfixOf` errorBundlePretty err
                _ -> False

        it "Returns the last statement of a block" $ do
            let code = "func last() { 1; 2; 3; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [ADefineFunc _ _ _ (AInteger (I8 3))] -> True
                _ -> False

        it "Triggers parse error (covers parseALL error formatting)" $ do
            let code = "func ("
            parseALL (p code) `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Variable Definitions" $ do

        it "Parses untyped assignment (defaults to undefined)" $ do
            let code = "x = 10;"
            parseALL (p code) `shouldSatisfy` \case
                Right [ASetVar name typeVar (AInteger (I8 10))] -> 
                    name == p "x" && typeVar == p "undefined"
                _ -> False

        it "Parses typed assignment (int)" $ do
            let code = "y: int = 20;"
            parseALL (p code) `shouldSatisfy` \case
                Right [ASetVar name typeVar (AInteger (I8 20))] -> 
                    name == p "y" && typeVar == p "int"
                _ -> False

        it "Parses specific types (int8, uint64)" $ do
            let code = "z: int8 = 100;"
            parseALL (p code) `shouldSatisfy` \case
                Right [ASetVar name typeVar _] -> 
                    name == p "z" && typeVar == p "int8"
                _ -> False

        it "Parses list types ([char])" $ do
            let code = "str: [char] = \"hello\";"
            parseALL (p code) `shouldSatisfy` \case
                Right [ASetVar name typeVar _] -> 
                    name == p "str" && typeVar == p "[char]"
                _ -> False

    describe "Expressions & Precedence" $ do

        it "Parses binary operations (+)" $ do
            let code = "1 + 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [ACall (ASymbol op) [AInteger (I8 1), AInteger (I8 2)]] -> op == p "+"
                _ -> False

        it "Respects precedence (* before +)" $ do
            let code = "1 + 2 * 3;"
            parseALL (p code) `shouldSatisfy` \case
                Right [ACall (ASymbol opPlus) [AInteger (I8 1), ACall (ASymbol opMul) [AInteger (I8 2), AInteger (I8 3)]]] -> 
                    opPlus == p "+" && opMul == p "*"
                _ -> False

        it "Parses function calls in expressions" $ do
            let code = "add(x, 5);"
            parseALL (p code) `shouldSatisfy` \case
                Right [ACall (ASymbol func) [ASymbol arg1, AInteger (I8 5)]] -> 
                    func == p "add" && arg1 == p "x"
                _ -> False

    describe "Function Definitions" $ do

        it "Parses simple function with return type" $ do
            let code = "func add(a: int, b: int) -> int { ret a + b; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [ADefineFunc name args retType _body] -> 
                    name == p "add" &&
                    args == [(p "a", p "int"), (p "b", p "int")] &&
                    retType == p "int"
                _ -> False

        it "Parses function without return type (implicit Void)" $ do
            let code = "func main() { ret 0; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [ADefineFunc name args retType _] -> 
                    name == p "main" &&
                    args == [] &&
                    retType == p "Void"
                _ -> False

        it "Parses function with complex arguments" $ do
            let code = "func printList(lst: [char]) -> void { ret 0; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [ADefineFunc _ args retType _] -> 
                    args == [(p "lst", p "[char]")] &&
                    retType == p "void"
                _ -> False

    describe "Control Flow" $ do
        
        it "Parses return statement" $ do
            let code = "ret 42;"
            parseALL (p code) `shouldSatisfy` \case
                Right [AInteger (I8 42)] -> True
                _ -> False

    describe "Coverage & Edge Cases" $ do

        it "Fails on empty function body (Empty function body not supported yet)" $ do
            let code = "func fail() {}"
            parseALL (p code) `shouldSatisfy` \case
                Left _ -> True
                _ -> False

        it "Returns the last statement of a block (return (last xs))" $ do
            let code = "func last() { 1; 2; 3; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [ADefineFunc _ _ _ (AInteger (I8 3))] -> True
                _ -> False

        it "Triggers parse error to cover parseALL error handling" $ do
            let code = "func ("
            parseALL (p code) `shouldSatisfy` \case
                Left _ -> True
                _ -> False
