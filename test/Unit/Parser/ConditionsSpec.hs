{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ConditionSpec.hs - Tests for If, Else, Loops
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ConditionsSpec (spec) where

import Test.Hspec
import Parser.Statement (parseALL)
import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (IntValue(..))
import qualified Data.Text as DT

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Parser C-Style - Control Flow (Conditions)" $ do

    describe "Standard If / Else" $ do

        it "Parses a simple if statement (no else)" $ do
            let code = "if (x > 0) { x = 1; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [Condition cond _ elseBlock] -> 
                    case (cond, elseBlock) of
                        (Call (ASymbol op) _, AVoid) -> op == p ">"
                        _ -> False
                _ -> False

        it "Parses if-else statement" $ do
            let code = "if (True) { 1; } else { 0; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [Condition (ABool True) (AList [AInteger (I8 1)]) (AList [AInteger (I8 0)])] -> True
                _ -> False

        it "Parses nested if-else (else if chain)" $ do
            let code = "if (a) { 1; } else if (b) { 2; } else { 3; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [Condition 
                        (ASymbol cond1) 
                        _ 
                        (Condition (ASymbol cond2) _ _)] -> 
                            cond1 == p "a" && cond2 == p "b"
                _ -> False

    describe "Bonus: If with Initialization" $ do

        it "Parses if with init statement: if (init; cond)" $ do
            let code = "if (x = 10; x > 5) { ret x; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [AList [initStmt, Condition _ _ _]] -> 
                    case initStmt of
                        Define name typeVar (AInteger (I8 10)) -> 
                            name == p "x" && typeVar == p "auto"
                        _ -> False
                _ -> False
    
    describe "Loops: While" $ do
        
        it "Parses a simple while loop" $ do
            let code = "while (x < 10) { x = x + 1; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [While cond body] -> 
                    case (cond, body) of
                        (Call (ASymbol op) _, AList _) -> op == p "<"
                        _ -> False
                _ -> False

        it "Parses while(true) infinite loop" $ do
            let code = "while (True) {}"
            parseALL (p code) `shouldSatisfy` \case
                Right [While (ABool True) AVoid] -> True
                _ -> False

    describe "Loops: For" $ do
        
        it "Parses a standard for loop" $ do
            let code = "for (i = 0; i < 10; i = i + 1) { print(i); }"
            parseALL (p code) `shouldSatisfy` \case
                Right [For initS cond updateS _] -> 
                    case (initS, cond, updateS) of
                        (Define _ _ _, Call (ASymbol op) _, Define _ _ _) -> op == p "<"
                        _ -> False
                _ -> False

    describe "Loops: For" $ do
        
        it "Parses a standard for loop (init; cond; step)" $ do
            let code = "for (i = 0; i < 10; i = i + 1) { print(i); }"
            parseALL (p code) `shouldSatisfy` \case
                Right [For initS cond updateS body] -> 
                    case (initS, cond, updateS, body) of
                        (Define _ _ _, Call (ASymbol op) _, Define _ _ _, AList _) -> op == p "<"
                        _ -> False
                _ -> False

        it "Parses a for loop with boolean logic in condition" $ do
            let code = "for (i = 0; i < 10 && running; i = i + 1) { }"
            parseALL (p code) `shouldSatisfy` \case
                Right [For _ cond _ _] -> 
                    case cond of
                        Call (ASymbol op) _ -> op == p "&&"
                        _ -> False
                _ -> False

        it "Parses a for loop with multiplication update" $ do
            let code = "for (n = 1; n < 100; n = n * 2) { print(n); }"
            parseALL (p code) `shouldSatisfy` \case
                Right [For _ _ updateS _] -> 
                    case updateS of
                        Define _ _ (Call (ASymbol op) _) -> op == p "*"
                        _ -> False
                _ -> False

        it "Parses a for loop with boolean flag initialization" $ do
            let code = "for (ok = True; ok; ok = False) { run_once(); }"
            parseALL (p code) `shouldSatisfy` \case
                Right [For initS cond _ _] -> 
                    case (initS, cond) of
                        (Define _ _ (ABool True), ASymbol _) -> True
                        _ -> False
                _ -> False

        it "Parses a for loop with empty body" $ do
            let code = "for (i=0; i<10; i=i+1) {}"
            parseALL (p code) `shouldSatisfy` \case
                Right [For _ _ _ AVoid] -> True
                _ -> False
