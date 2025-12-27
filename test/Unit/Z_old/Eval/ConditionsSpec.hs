{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ConditionsSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Z_old.Eval.ConditionsSpec (spec) where

import Test.Hspec
import Z_old.Src.Eval.Conditions (execCondition)
import Z_old.Src.Ast (OldAst(..))
import Z_old.Src.Type.Integer (IntValue(..))
import qualified Data.Text as DT
import Data.List (isInfixOf)

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Condition Evaluation Logic" $ do
    describe "Boolean Conditions" $ do
        it "selects 'then' branch when True" $ do
            execCondition (ABool True) (AInteger (I8 42)) (AInteger (I8 21)) `shouldSatisfy` \case
                Right (AInteger (I8 42)) -> True
                _ -> False
        it "selects 'else' branch when False" $ do
            execCondition (ABool False) (AInteger (I8 42)) (AInteger (I8 21)) `shouldSatisfy` \case
                Right (AInteger (I8 21)) -> True
                _ -> False

    describe "Integer Conditions (C-style)" $ do
        it "selects 'then' branch when != 0" $ do
            execCondition (AInteger (I8 1)) (ASymbol (p "yes")) (ASymbol (p "no")) `shouldSatisfy` \case
                Right (ASymbol s) -> s == p "yes"
                _ -> False
        it "selects 'else' branch when 0" $ do
            execCondition (AInteger (I8 0)) (ASymbol (p "yes")) (ASymbol (p "no")) `shouldSatisfy` \case
                Right (ASymbol s) -> s == p "no"
                _ -> False

    describe "Error Handling (Coverage Target)" $ do
        it "fails on invalid condition type (not Bool/Int)" $ do
            let cond = ASymbol (p "foo")
            execCondition cond (AInteger (I8 1)) (AInteger (I8 2)) `shouldSatisfy` \case
                Left err -> "Invalid condition" `isInfixOf` DT.unpack err
                _ -> False
