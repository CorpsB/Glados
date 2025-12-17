{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerStateSpec
-}

module Compiler.CompilerStateSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Compiler.CompilerState (CompilerState(..), createCompilerState)

spec :: Spec
spec = describe "Compiler.CompilerState" $ do
  it "createCompilerState initializes empty code" $ do
    csCode createCompilerState `shouldBe` Seq.empty

  it "createCompilerState initializes empty symbol table" $ do
    csSymbols createCompilerState `shouldBe` Map.empty

  it "createCompilerState sets csNextIndex to 0" $ do
    csNextIndex createCompilerState `shouldBe` 0

  it "createCompilerState sets csLabelCnt to 0" $ do
    csLabelCnt createCompilerState `shouldBe` 0

  it "Show instance works (does not crash)" $ do
    length (show createCompilerState) `shouldSatisfy` (>= 0)

  it "Eq instance: state equals itself" $ do
    createCompilerState `shouldBe` createCompilerState

