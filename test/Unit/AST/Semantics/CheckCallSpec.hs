{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Unit tests for CheckCall
-}

{-# LANGUAGE LambdaCase #-}

module AST.Semantics.CheckCallSpec (spec) where

import Test.Hspec
import AST.Semantics.CheckCall
import AST.Semantics.Type
import AST.Ast (Ast(..))
import Common.Type.Integer (fitInteger)
import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)

p :: String -> DT.Text
p = DT.pack

testEnv :: CheckEnv
testEnv = CheckEnv 
    (Map.fromList [
        (p "i", TyInt),
        (p "b", TyBool),
        (p "add", TyFunc [TyInt, TyInt] TyInt),
        (p "isZero", TyFunc [TyInt] TyBool),
        (p "notAFunc", TyInt)
    ]) 
    Map.empty

mockCheckExpr :: CheckEnv -> Ast -> Either String Type
mockCheckExpr _ (AInteger _) = Right TyInt
mockCheckExpr _ (ABool _) = Right TyBool
mockCheckExpr env (ASymbol name) = case Map.lookup name (envVars env) of
    Just t -> Right t
    Nothing -> Left $ "Undefined variable '" ++ DT.unpack name ++ "'"
mockCheckExpr _ _ = Left "Mock error: expression not supported in unit test"

spec :: Spec
spec = describe "AST.Semantics.CheckCall" $ do

    describe "Arithmetic Operators (+, -, *, div, mod)" $ do
        it "Validates int + int" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "+")) args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Validates division" $ do
            let args = [AInteger (fitInteger 10), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "div")) args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Rejects int + bool" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "+")) args `shouldSatisfy` \case
                Left err -> "expects (int, int)" `isInfixOf` err
                _ -> False

        it "Rejects wrong argument count" $ do
            let args = [AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "+")) args `shouldSatisfy` \case
                Left err -> "expects 2 arguments" `isInfixOf` err
                _ -> False

    describe "Comparison Operators (<, >, <=, >=)" $ do
        it "Validates int < int" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "<")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects bool < int" $ do
            let args = [ABool True, AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "<")) args `shouldSatisfy` \case
                Left err -> "expects (int, int)" `isInfixOf` err
                _ -> False

    describe "Equality Operator (eq?)" $ do
        it "Validates int == int" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "eq?")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Validates bool == bool" $ do
            let args = [ABool True, ABool False]
            checkCall mockCheckExpr testEnv (ASymbol (p "eq?")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects int == bool (Incompatible types)" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "eq?")) args `shouldSatisfy` \case
                Left err -> "compatible types" `isInfixOf` err
                _ -> False

    describe "Logic Operators (&&, ||)" $ do
        it "Validates bool && bool" $ do
            let args = [ABool True, ABool False]
            checkCall mockCheckExpr testEnv (ASymbol (p "&&")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects bool && int" $ do
            let args = [ABool True, AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "&&")) args `shouldSatisfy` \case
                Left err -> "expects (bool, bool)" `isInfixOf` err
                _ -> False

    describe "Unary Operator (!)" $ do
        it "Validates !bool" $ do
            let args = [ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "!")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects !int" $ do
            let args = [AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "!")) args `shouldSatisfy` \case
                Left err -> "expects bool" `isInfixOf` err
                _ -> False
        
        it "Rejects unary op with 2 arguments" $ do
            let args = [ABool True, ABool False]
            checkCall mockCheckExpr testEnv (ASymbol (p "!")) args `shouldSatisfy` \case
                Left err -> "expects 1 argument" `isInfixOf` err
                _ -> False

    describe "User Defined Functions" $ do
        it "Validates correct function call (add)" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "add")) args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Validates correct function call (isZero)" $ do
            let args = [AInteger (fitInteger 0)]
            checkCall mockCheckExpr testEnv (ASymbol (p "isZero")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects invalid argument count" $ do
            let args = [AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "add")) args `shouldSatisfy` \case
                Left err -> "expects 2 arguments" `isInfixOf` err
                _ -> False

        it "Rejects invalid argument types" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "add")) args `shouldSatisfy` \case
                Left err -> "Argument type mismatch" `isInfixOf` err
                _ -> False

        it "Rejects undefined function" $ do
            let args = []
            checkCall mockCheckExpr testEnv (ASymbol (p "unknown")) args `shouldSatisfy` \case
                Left err -> "Undefined function" `isInfixOf` err
                _ -> False

        it "Rejects calling a non-function variable" $ do
            let args = []
            checkCall mockCheckExpr testEnv (ASymbol (p "notAFunc")) args `shouldSatisfy` \case
                Left err -> "is not a function" `isInfixOf` err
                _ -> False
    
    describe "Edge Cases" $ do
        it "Handles APos wrapper on function name transparently" $ do
            let funcNode = APos 1 1 (ASymbol (p "+"))
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv funcNode args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Rejects invalid call node (not a symbol)" $ do
            let funcNode = AInteger (fitInteger 1)
            checkCall mockCheckExpr testEnv funcNode [] `shouldSatisfy` \case
                Left err -> "Invalid function call" `isInfixOf` err
                _ -> False
