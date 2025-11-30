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
import Type.Integer (IntValue(..))
import Eval.Builtins (execBuiltin)

spec :: Spec
spec = describe "AST - Builtins unit tests" $ do
    describe "Builtin - eq?" $ do
        it "Integers - Equal" $ do
            execBuiltin "eq?" [AInteger (I8 42), AInteger (I8 42)] `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Integers - Not equal" $ do
            execBuiltin "eq?" [AInteger (I8 42), AInteger (I8 84)] `shouldSatisfy` \case
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
            execBuiltin "eq?" [ABool True, AInteger (I8 42)] `shouldSatisfy` \case
                Left "*** ERROR: 'eq?' expects two integers or two booleans, got: [ABool True,AInteger (I8 42)]" -> True
                _ -> False
    describe "Builtin - <" $ do
        it "Integers - Lower" $ do
            execBuiltin "<" [AInteger (I8 42), AInteger (I8 84)] `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Integers - Not Lower" $ do
            execBuiltin "<" [AInteger (I8 84), AInteger (I8 42)] `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "<" [ABool True, ASymbol "abc"] `shouldSatisfy` \case
                Left "*** ERROR: '<' expects two integers, got: [ABool True,ASymbol \"abc\"]" -> True
                _ -> False
    describe "Builtin - >" $ do
        it "Integers - Greater" $ do
            execBuiltin ">" [AInteger (I8 84), AInteger (I8 42)] `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        it "Integers - Not Greater" $ do
            execBuiltin ">" [AInteger (I8 42), AInteger (I8 84)] `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin ">" [ABool True, ASymbol "abc"] `shouldSatisfy` \case
                Left "*** ERROR: '>' expects two integers, got: [ABool True,ASymbol \"abc\"]" -> True
                _ -> False
    describe "Builtin - +" $ do
        it "Integers - Addition" $ do
            execBuiltin "+" [AInteger (I8 42), AInteger (I8 42)] `shouldSatisfy` \case
                Right (AInteger (I8 84)) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "+" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: '+' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - -" $ do
        it "Integers - Substraction" $ do
            execBuiltin "-" [AInteger (I8 42), AInteger (I8 21)] `shouldSatisfy` \case
                Right (AInteger (I8 21)) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "-" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: '-' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - *" $ do
        it "Integers - Multiplication" $ do
            execBuiltin "*" [AInteger (I8 5), AInteger (I8 5)] `shouldSatisfy` \case
                Right (AInteger (I8 25)) -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "*" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: '*' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - div" $ do
        it "Integers - Division" $ do
            execBuiltin "div" [AInteger (I8 6), AInteger (I8 2)] `shouldSatisfy` \case
                Right (AInteger (I8 3)) -> True
                _ -> False
        it "Integers - Division by zero" $ do
            execBuiltin "div" [AInteger (I8 42), AInteger (I8 0)] `shouldSatisfy` \case
                Left "*** ERROR: 'div' division by zero" -> True
                _ -> False
        it "Invalid arguments" $ do
            execBuiltin "div" [ASymbol "abc", ASymbol "def"] `shouldSatisfy` \case
                Left "*** ERROR: 'div' expects two integers, got: [ASymbol \"abc\",ASymbol \"def\"]" -> True
                _ -> False
    describe "Builtin - mod" $ do
        it "Integers - Division" $ do
            execBuiltin "mod" [AInteger (I8 7), AInteger (I8 2)] `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True
                _ -> False
        it "Integers - Division by zero" $ do
            execBuiltin "mod" [AInteger (I8 42), AInteger (I8 0)] `shouldSatisfy` \case
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
