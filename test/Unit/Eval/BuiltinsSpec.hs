{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- BuiltinsSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Eval.BuiltinsSpec (spec) where

import Test.Hspec
import Ast (Ast(..))
import Eval.Builtins (execBuiltin)

spec :: Spec
spec = describe "AST - Builtins unit tests" $ do
    describe "Builtin - eq?" $ do
        it "Integers - Equal" $ do
            execBuiltin "eq?" [AInteger 42, AInteger 42] `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Integers - Not equal" $ do
            execBuiltin "eq?" [AInteger 42, AInteger 84] `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Booleans - Equal" $ do
            execBuiltin "eq?" [ABool True, ABool True] `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Booleans - Not equal" $ do
            execBuiltin "eq?" [ABool True, ABool False] `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "eq?" [ABool True, AInteger 42] `shouldSatisfy` \case
                Left "*** ERROR: 'eq?' expects two integers or two booleans, got: [ABool True,AInteger 42]" -> True
                _ -> False
    describe "Builtin - <" $ do
        it "Integers - Lower" $ do
            execBuiltin "<" [AInteger 42, AInteger 84] `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Integers - Not Lower" $ do
            execBuiltin "<" [AInteger 84, AInteger 42] `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "<" [ABool True, ASymbol "abc"] `shouldSatisfy` \case
                Left "*** ERROR: '<' expects two integers, got: [ABool True,ASymbol \"abc\"]" -> True
                _ -> False
    describe "Builtin - >" $ do
        it "Integers - Greater" $ do
            execBuiltin ">" [AInteger 84, AInteger 42] `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Integers - Not Greater" $ do
            execBuiltin ">" [AInteger 42, AInteger 84] `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin ">" [ABool True, ASymbol "abc"] `shouldSatisfy` \case
                Left "*** ERROR: '>' expects two integers, got: [ABool True,ASymbol \"abc\"]" -> True
                _ -> False
    describe "Builtin - +" $ do
        it "Integers - Addition" $ do
            execBuiltin "+" [AInteger 42, AInteger 42] `shouldSatisfy` \case
                Right (AInteger 84) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "+" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: '+' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - -" $ do
        it "Integers - Substraction" $ do
            execBuiltin "-" [AInteger 42, AInteger 21] `shouldSatisfy` \case
                Right (AInteger 21) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "-" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: '-' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - *" $ do
        it "Integers - Multiplication" $ do
            execBuiltin "*" [AInteger 5, AInteger 5] `shouldSatisfy` \case
                Right (AInteger 25) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "*" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: '*' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - div" $ do
        it "Integers - Division" $ do
            execBuiltin "div" [AInteger 6, AInteger 2] `shouldSatisfy` \case
                Right (AInteger 3) -> True
                _ -> False
        it "Integers - Division by zero" $ do
            execBuiltin "div" [AInteger 42, AInteger 0] `shouldSatisfy` \case
                Left "*** ERROR: 'div' division by zero" -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "div" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: 'div' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - mod" $ do
        it "Integers - Division" $ do
            execBuiltin "mod" [AInteger 7, AInteger 2] `shouldSatisfy` \case
                Right (AInteger 1) -> True
                _ -> False
        it "Integers - Division by zero" $ do
            execBuiltin "mod" [AInteger 42, AInteger 0] `shouldSatisfy` \case
                Left "*** ERROR: 'mod' division by zero" -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "mod" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: 'mod' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - Fail" $ do
        it "Unknown builtin" $ do
            execBuiltin "noopy" [] `shouldSatisfy` \case
                Left "*** ERROR: Unknown builtin: noopy" -> True
                _ -> False