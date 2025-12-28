{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerMonadSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ASM.CompilerMonadSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Compiler.CompilerState (CompilerState(..), ScopeType(..), createCompilerState)
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..))
import Compiler.ASM.CompilerMonad

expectRight :: Either e a -> a
expectRight (Right x) = x
expectRight (Left _)  = error "Expected Right, got Left"

expectLeft :: Either e a -> e
expectLeft (Left e)  = e
expectLeft (Right _) = error "Expected Left, got Right"

runCM :: CompilerMonad a -> CompilerState -> Either T.Text (a, CompilerState)
runCM = runStateT

spec :: Spec
spec = describe "Compiler.ASM.CompilerMonad (max coverage)" $ do
  describe "Helpers coverage" $ do
    it "expectRight / expectLeft cover both branches + throw branches" $ do
      expectRight (Right (1 :: Int) :: Either T.Text Int) `shouldBe` 1
      expectLeft (Left ("e" :: T.Text) :: Either T.Text Int) `shouldBe` "e"
      evaluate (expectRight (Left ("boom" :: T.Text) :: Either T.Text Int)) `shouldThrow` anyErrorCall
      evaluate (expectLeft (Right (2 :: Int) :: Either T.Text Int)) `shouldThrow` anyErrorCall

  describe "appendPseudoInstruction + basic emitters" $ do
    it "appendPseudoInstruction appends into csCode" $ do
      let (_, st) = expectRight (runCM (appendPseudoInstruction (LabelDef "L")) createCompilerState)
      csCode st `shouldBe` Seq.singleton (LabelDef "L")

    it "emitInstruction wraps in Real" $ do
      let (_, st) = expectRight (runCM (emitInstruction Halt) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real Halt)

    it "emitLabelDefinition / emitJump* / emitCallToLabel all append expected PsInstruction" $ do
      let action = do
            emitLabelDefinition "A"
            emitJumpToLabel "B"
            emitJumpIfFalseToLabel "C"
            emitJumpIfTrueToLabel "D"
            emitCallToLabel "E"
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe` Seq.fromList
        [ LabelDef "A"
        , JumpLabel "B"
        , JumpIfFalseLabel "C"
        , JumpIfTrueLabel "D"
        , CallLabel "E"
        ]

  describe "generateUniqueLabel" $ do
    it "increments csLabelCnt and returns unique labels" $ do
      let action = do
            l0 <- generateUniqueLabel "label"
            l1 <- generateUniqueLabel "label"
            pure (l0, l1)
      let ((l0, l1), st) = expectRight (runCM action createCompilerState)
      l0 `shouldBe` "label_0"
      l1 `shouldBe` "label_1"
      csLabelCnt st `shouldBe` 2

  describe "defineSymbol" $ do
    it "allocates ScopeGlobal indices and increments csNextIndex" $ do
      let action = do
            i0 <- defineSymbol "varA"
            i1 <- defineSymbol "varB"
            pure (i0, i1)
      let ((i0, i1), st) = expectRight (runCM action createCompilerState)
      i0 `shouldBe` 0
      i1 `shouldBe` 1
      csNextIndex st `shouldBe` 2
      Map.lookup "varA" (csSymbols st) `shouldBe` Just (ScopeGlobal, 0)
      Map.lookup "varB" (csSymbols st) `shouldBe` Just (ScopeGlobal, 1)

    it "fails if already defined" $ do
      let action = do
            _ <- defineSymbol "conflict"
            _ <- defineSymbol "conflict"
            pure ()
      let err = expectLeft (runStateT action createCompilerState)
      err `shouldBe` "Symbol already defined: conflict"

  describe "registerSymbol (all branches)" $ do
    it "ScopeLocal adjusts csNextIndex to max(old, idx+1)" $ do
      let action = do
            registerSymbol "x" ScopeLocal 0
            registerSymbol "y" ScopeLocal 3
      let (_, st) = expectRight (runCM action createCompilerState)
      csNextIndex st `shouldBe` 4
      Map.lookup "x" (csSymbols st) `shouldBe` Just (ScopeLocal, 0)
      Map.lookup "y" (csSymbols st) `shouldBe` Just (ScopeLocal, 3)

    it "ScopeGlobal adjusts csNextIndex similarly" $ do
      let action = do
            registerSymbol "g" ScopeGlobal 10
      let (_, st) = expectRight (runCM action createCompilerState)
      csNextIndex st `shouldBe` 11
      Map.lookup "g" (csSymbols st) `shouldBe` Just (ScopeGlobal, 10)

    it "ScopeCapture does NOT touch csNextIndex" $ do
      let st0 = createCompilerState { csNextIndex = 7 }
      let (_, st) = expectRight (runCM (registerSymbol "c" ScopeCapture 2) st0)
      csNextIndex st `shouldBe` 7
      Map.lookup "c" (csSymbols st) `shouldBe` Just (ScopeCapture, 2)

  describe "compileInIsolatedFunctionScope" $ do
    it "moves isolated csCode into outer csFuncs, restores outer csCode, and merges nested csFuncs too" $ do
      let action = do
            emitInstruction Halt
            compileInIsolatedFunctionScope $ do
              emitLabelDefinition "inner"
              emitInstruction Nop
              compileInIsolatedFunctionScope $ do
                emitLabelDefinition "nested"
                emitInstruction Ret
              emitInstruction Ret
            emitInstruction Halt
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe` Seq.fromList [Real Halt, Real Halt]
      csFuncs st `shouldBe` Seq.fromList
        [ LabelDef "inner"
        , Real Nop
        , Real Ret
        , LabelDef "nested"
        , Real Ret
        ]

    it "propagates Left from isolated scope" $ do
      let action = compileInIsolatedFunctionScope (lift (Left "Manual Error") :: CompilerMonad ())
      let err = expectLeft (runStateT action createCompilerState)
      err `shouldBe` "Manual Error"

  describe "Underlying Either error propagation" $ do
    it "lift Left bubbles up" $ do
      let action = (lift (Left "Manual Error") :: CompilerMonad ())
      let err = expectLeft (runStateT action createCompilerState)
      err `shouldBe` "Manual Error"
