{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ConditionsSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Eval.ConditionsSpec (spec) where

import Test.Hspec
import Eval.Conditions (execCondition)
import Ast (Ast(..))

spec :: Spec
spec = describe "Condition Evaluation Logic" $ do

    describe "Boolean Conditions" $ do
      it "selects 'then' branch when True" $ do
        execCondition (ABool True) (AInteger 42) (AInteger 21) `shouldSatisfy` \case
             Right (AInteger 42) -> True
             _ -> False

      it "selects 'else' branch when False" $ do
        execCondition (ABool False) (AInteger 42) (AInteger 21) `shouldSatisfy` \case
             Right (AInteger 21) -> True
             _ -> False

    describe "Integer Conditions (C-style)" $ do
      it "selects 'then' branch when 1" $ do
        execCondition (AInteger 1) (ASymbol "yes") (ASymbol "no") `shouldSatisfy` \case
             Right (ASymbol "yes") -> True
             _ -> False

      it "selects 'else' branch when 0" $ do
        execCondition (AInteger 0) (ASymbol "yes") (ASymbol "no") `shouldSatisfy` \case
             Right (ASymbol "no") -> True
             _ -> False

    describe "Error Handling" $ do
      it "fails on invalid condition type" $ do
        execCondition (ASymbol "foo") (AInteger 1) (AInteger 2) `shouldSatisfy` \case
             Left _ -> True
             _ -> False
