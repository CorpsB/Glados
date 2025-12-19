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
import qualified Data.Text as T

import Compiler.CompilerState (CompilerState(..), createCompilerState)

spec :: Spec
spec = describe "Compiler.CompilerState (source module coverage)" $ do
  it "createCompilerState initializes empty code" $
    csCode createCompilerState `shouldBe` Seq.empty

  it "createCompilerState initializes empty symbol table" $
    csSymbols createCompilerState `shouldBe` Map.empty

  it "createCompilerState sets csNextIndex to 0" $
    csNextIndex createCompilerState `shouldBe` 0

  it "createCompilerState sets csLabelCnt to 0" $
    csLabelCnt createCompilerState `shouldBe` 0

  it "Show instance produces a readable non-empty string" $
    show createCompilerState `shouldSatisfy` (not . null)

  it "Eq compares fields (changing one field makes it different)" $ do
    let a = createCompilerState
    let b = createCompilerState { csNextIndex = 1 }
    (a == b) `shouldBe` False

  it "Eq: two identical states are equal (field-by-field)" $ do
    let a = createCompilerState
    let b = createCompilerState
    (a == b) `shouldBe` True

  it "Record update affects only targeted field (invariant test)" $ do
    let s = createCompilerState { csLabelCnt = 10 }
    csLabelCnt s `shouldBe` 10
    csNextIndex s `shouldBe` 0
