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
import AST.Ast (Ast(..), cleanAst)
import Common.Type.Integer (IntValue(..))
import qualified Data.Text as DT
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)

p :: String -> DT.Text
p = DT.pack

-- Helper de parsing qui nettoie l'AST
parseClean :: DT.Text -> Either (ParseErrorBundle DT.Text Void) [Ast]
parseClean input = fmap (map cleanAst) (parseALL input)

spec :: Spec
spec = describe "Parser C-Style - Control Flow (Conditions)" $ do
    
    describe "Standard If / Else" $ do
        it "Parses a simple if statement (no else)" $ do
            let code = "if (x > 0) { x = 1; }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AIf cond _ AVoid] -> 
                    case cond of
                        ACall (ASymbol op) _ -> op == p ">"
                        _ -> False
                _ -> False

        it "Parses if-else statement" $ do
            let code = "if (True) { 1; } else { 0; }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AIf (ABool True) thenB elseB] -> 
                    checkBlockContent thenB 1 && checkBlockContent elseB 0
                _ -> False

        it "Parses nested if-else (else if chain)" $ do
            let code = "if (a) { 1; } else if (b) { 2; } else { 3; }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AIf (ASymbol cond1) _ (AIf (ASymbol cond2) _ _)] -> 
                            cond1 == p "a" && cond2 == p "b"
                _ -> False

    describe "Bonus: If with Initialization" $ do
        it "Parses if with init statement: if (init; cond)" $ do
            let code = "if (x = 10; x > 5) { ret x; }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AList [initStmt, AIf _ _ _]] -> 
                    case initStmt of
                        ASetVar name typeVar (AInteger (I8 10)) -> 
                            name == p "x" && typeVar == p "auto"
                        _ -> False
                _ -> False

    describe "Loops: While" $ do
        it "Parses a simple while loop" $ do
            let code = "while (x < 10) { x = x + 1; }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AWhile cond body] -> 
                    case cond of
                        ACall (ASymbol op) _ -> op == p "<" && isBlock body
                        _ -> False
                _ -> False

        it "Parses while(true) infinite loop" $ do
            let code = "while (True) {}"
            parseClean (p code) `shouldSatisfy` \case
                Right [AWhile (ABool True) AVoid] -> True
                _ -> False

    describe "Loops: For" $ do
        it "Parses a standard for loop" $ do
            let code = "for (i = 0; i < 10; i = i + 1) { print(i); }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AFor initS cond updateS body] -> 
                    case (initS, cond, updateS) of
                        (ASetVar _ _ _, ACall (ASymbol op) _, ASetVar _ _ _) -> 
                            op == p "<" && isBlock body
                        _ -> False
                _ -> False

        it "Parses a for loop with boolean logic in condition" $ do
            let code = "for (i = 0; i < 10 && running; i = i + 1) { }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AFor _ cond _ _] -> 
                    case cond of
                        ACall (ASymbol op) _ -> op == p "&&"
                        _ -> False
                _ -> False

        it "Parses a for loop with multiplication update" $ do
            let code = "for (n = 1; n < 100; n = n * 2) { print(n); }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AFor _ _ updateS _] -> 
                    case updateS of
                        ASetVar _ _ (ACall (ASymbol op) _) -> op == p "*"
                        _ -> False
                _ -> False

        it "Parses a for loop with boolean flag initialization" $ do
            let code = "for (ok = True; ok; ok = False) { run_once(); }"
            parseClean (p code) `shouldSatisfy` \case
                Right [AFor initS cond _ _] -> 
                    case (initS, cond) of
                        (ASetVar _ _ (ABool True), ASymbol _) -> True
                        _ -> False
                _ -> False

        it "Parses a for loop with empty body" $ do
            let code = "for (i=0; i<10; i=i+1) {}"
            parseClean (p code) `shouldSatisfy` \case
                Right [AFor _ _ _ AVoid] -> True
                _ -> False

checkBlockContent :: Ast -> Int -> Bool
checkBlockContent (AList [AExprStmt (AInteger (I8 v))]) target = fromIntegral v == target
checkBlockContent (ABlock [AExprStmt (AInteger (I8 v))]) target = fromIntegral v == target
checkBlockContent (AList [AInteger (I8 v)]) target = fromIntegral v == target
checkBlockContent (ABlock [AInteger (I8 v)]) target = fromIntegral v == target
checkBlockContent _ _ = False

isBlock :: Ast -> Bool
isBlock (AList _) = True
isBlock (ABlock _) = True
isBlock _ = False
