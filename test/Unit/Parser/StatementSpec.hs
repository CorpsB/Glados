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

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Parser - Statement & Expression" $ do

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

        it "Parses empty function body (returns AVoid)" $ do
            let code = "func fail() {}"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun _ _ _ AVoid] -> True
                _ -> False

        it "Returns a list of statements for a block" $ do
            let code = "func last() { 1; 2; 3; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun _ _ _ (AList [AInteger (I8 1), AInteger (I8 2), AInteger (I8 3)])] -> True
                _ -> False

        it "Triggers parse error (covers parseALL error formatting)" $ do
            let code = "func ("
            parseALL (p code) `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Variable Definitions" $ do

        it "Parses untyped assignment (defaults to auto)" $ do
            let code = "x = 10;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar (AInteger (I8 10))] -> 
                    name == p "x" && typeVar == p "auto"
                _ -> False

        it "Parses typed assignment (int)" $ do
            let code = "y: int = 20;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar (AInteger (I8 20))] -> 
                    name == p "y" && typeVar == p "int"
                _ -> False

        it "Parses specific types (int8, uint64)" $ do
            let code = "z: int8 = 100;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar _] -> 
                    name == p "z" && typeVar == p "int8"
                _ -> False

        it "Parses list types ([char])" $ do
            let code = "str: [char] = \"hello\";"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar _] -> 
                    name == p "str" && typeVar == p "[char]"
                _ -> False

    describe "Expressions & Precedence" $ do

        it "Parses binary operations (+)" $ do
            let code = "1 + 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol op) [AInteger (I8 1), AInteger (I8 2)]] -> op == p "+"
                _ -> False

        it "Respects precedence (* before +)" $ do
            let code = "1 + 2 * 3;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol opPlus) [AInteger (I8 1), Call (ASymbol opMul) [AInteger (I8 2), AInteger (I8 3)]]] -> 
                    opPlus == p "+" && opMul == p "*"
                _ -> False

        it "Parses function calls in expressions" $ do
            let code = "add(x, 5);"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol func) [ASymbol arg1, AInteger (I8 5)]] -> 
                    func == p "add" && arg1 == p "x"
                _ -> False

    describe "Function Definitions" $ do

        it "Parses simple function with return type" $ do
            let code = "func add(a: int, b: int) -> int { ret a + b; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun name args retType _body] -> 
                    name == p "add" &&
                    args == [(p "a", p "int"), (p "b", p "int")] &&
                    retType == p "int"
                _ -> False

        it "Parses function without return type (implicit Void)" $ do
            let code = "func main() { ret 0; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun name args retType _] -> 
                    name == p "main" &&
                    args == [] &&
                    retType == p "Void"
                _ -> False

        it "Parses function with complex arguments" $ do
            let code = "func printList(lst: [char]) -> void { ret 0; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun _ args retType _] -> 
                    args == [(p "lst", p "[char]")] &&
                    retType == p "void"
                _ -> False

    describe "Control Flow" $ do
        
        it "Parses return statement" $ do
            let code = "ret 42;"
            parseALL (p code) `shouldSatisfy` \case
                Right [AInteger (I8 42)] -> True
                _ -> False

    describe "Unary Operators (Increment/Decrement)" $ do
        
        it "Parses ++x as x = x + 1" $ do
            let code = "++x;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call (ASymbol op) _)] -> 
                    name == p "x" && op == p "+"
                _ -> False

        it "Parses --y as y = y - 1" $ do
            let code = "--y;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call (ASymbol op) _)] -> 
                    name == p "y" && op == p "-"
                _ -> False

        it "Parses ++ inside an expression (complex)" $ do
            let code = "z = ++x * 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call _ [incrX, _])] -> 
                    case incrX of
                        Define x _ _ -> x == p "x"
                        _ -> False
                _ -> False
    
    describe "Compound Assignment Operators" $ do
        
        it "Parses += as addition (x = x + 5)" $ do
            let code = "x += 5;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "x" && 
                    (case op of ASymbol s -> s == p "+"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "x"; _ -> False)
                _ -> False

        it "Parses -= as subtraction (y -= 2)" $ do
            let code = "y -= 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "y" && 
                    (case op of ASymbol s -> s == p "-"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "y"; _ -> False)
                _ -> False

        it "Parses *= as multiplication (z *= 10)" $ do
            let code = "z *= 10;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "z" && 
                    (case op of ASymbol s -> s == p "*"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "z"; _ -> False)
                _ -> False

        it "Parses /= as division (w /= 2)" $ do
            let code = "w /= 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "w" && 
                    (case op of ASymbol s -> s == p "div"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "w"; _ -> False)
                _ -> False
