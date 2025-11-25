{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ConditionsSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Eval.ConditionsSpec (spec) where

import Test.Hspec
import Ast (Ast(..))
import Eval.Conditions (execCondition)

spec :: Spec
spec = describe "AST - Conditions unit tests" $ do
    describe "Condition - Boolean checks" $ do
        it "Executes 'then' branch when condition is True" $ do
            execCondition (Just (ABool True)) (Just (AInteger 42)) (Just (AInteger 21)) `shouldSatisfy` \case
                Right (AInteger 42) -> True
                _ -> False
        it "Executes 'else' branch when condition is False" $ do
            execCondition (Just (ABool False)) (Just (AInteger 42)) (Just (AInteger 21)) `shouldSatisfy` \case
                Right (AInteger 21) -> True
                _ -> False

    describe "Condition - Integer checks (Legacy/C-style)" $ do
        it "Executes 'then' branch when condition is 1" $ do
            execCondition (Just (AInteger 1)) (Just (ASymbol "yes")) (Just (ASymbol "no")) `shouldSatisfy` \case
                Right (ASymbol "yes") -> True
                _ -> False
        it "Executes 'else' branch when condition is 0" $ do
            execCondition (Just (AInteger 0)) (Just (ASymbol "yes")) (Just (ASymbol "no")) `shouldSatisfy` \case
                Right (ASymbol "no") -> True
                _ -> False

    describe "Condition - Error handling" $ do
        it "Error: Condition is missing (Nothing)" $ do
            execCondition Nothing (Just (AInteger 1)) (Just (AInteger 2)) `shouldSatisfy` \case
                Left "*** ERROR: Condition is missing" -> True
                _ -> False
        it "Error: Invalid condition type (Symbol)" $ do
            execCondition (Just (ASymbol "foo")) (Just (AInteger 1)) (Just (AInteger 2)) `shouldSatisfy` \case
                Left "*** ERROR: Invalid condition: ASymbol \"foo\"" -> True
                _ -> False
        it "Error: Invalid Integer value (not 0 or 1)" $ do
            execCondition (Just (AInteger 42)) (Just (AInteger 1)) (Just (AInteger 2)) `shouldSatisfy` \case
                Left "*** ERROR: Invalid condition: AInteger 42" -> True
                _ -> False
