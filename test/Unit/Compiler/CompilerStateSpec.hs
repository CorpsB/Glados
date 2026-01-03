{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerStateSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.CompilerStateSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.List (isInfixOf)

import Compiler.CompilerState (CompilerState(..), ScopeType(..), createCompilerState)
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..))

spec :: Spec
spec = describe "Compiler.CompilerState (max coverage)" $ do
  describe "createCompilerState fields" $ do
    it "initializes empty/zero fields" $ do
      csSymbols createCompilerState `shouldBe` Map.empty
      csNextIndex createCompilerState `shouldBe` 0
      csLabelCnt createCompilerState `shouldBe` 0
      csCode createCompilerState `shouldBe` Seq.empty
      csFuncs createCompilerState `shouldBe` Seq.empty

  describe "ScopeType derivations" $ do
    it "Eq/Show sanity" $ do
      ScopeGlobal `shouldBe` ScopeGlobal
      ScopeLocal `shouldNotBe` ScopeCapture
      show ScopeCapture `shouldSatisfy` (not . null)

  describe "CompilerState Eq/Show deep checks" $ do
    it "Eq matches identical states and differs when a field changes" $ do
      let a = createCompilerState
      let b = createCompilerState
      a `shouldBe` b
      a `shouldNotBe` (b { csNextIndex = 1 })
      a `shouldNotBe` (b { csLabelCnt = 1 })
      a `shouldNotBe` (b { csCode = Seq.singleton (Real Halt) })
      a `shouldNotBe` (b { csFuncs = Seq.singleton (LabelDef "F") })

    it "Map insertion order does not affect equality" $ do
      let symA = Map.fromList [("x", (ScopeGlobal, 0)), ("y", (ScopeLocal, 1))]
      let symB = Map.fromList [("y", (ScopeLocal, 1)), ("x", (ScopeGlobal, 0))]
      let sA = createCompilerState { csSymbols = symA }
      let sB = createCompilerState { csSymbols = symB }
      sA `shouldBe` sB

    it "Show contains key structural hints" $ do
      let s =
            createCompilerState
              { csSymbols = Map.fromList [("x", (ScopeGlobal, 0))]
              , csNextIndex = 1
              , csLabelCnt = 2
              , csCode = Seq.fromList [LabelDef "L1", Real Halt]
              , csFuncs = Seq.fromList [LabelDef "F1", Real Ret]
              }
      let sh = show s
      sh `shouldSatisfy` ("CompilerState" `isInfixOf`)
      sh `shouldSatisfy` ("csSymbols" `isInfixOf`)
      sh `shouldSatisfy` ("ScopeGlobal" `isInfixOf`)
      sh `shouldSatisfy` ("L1" `isInfixOf`)
      sh `shouldSatisfy` ("F1" `isInfixOf`)
      sh `shouldSatisfy` ("Ret" `isInfixOf`)
