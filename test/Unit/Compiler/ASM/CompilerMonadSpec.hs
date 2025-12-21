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

import Compiler.CompilerState (CompilerState(..), createCompilerState)
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.ASM.CompilerMonad
import Common.Type.Integer (IntValue(..))

expectRight :: Either e a -> a
expectRight (Right x) = x
expectRight (Left _)  = error "Expected Right, got Left"

expectLeft :: Either e a -> e
expectLeft (Left e)  = e
expectLeft (Right _) = error "Expected Left, got Right"

execCM :: CompilerMonad a -> Either T.Text CompilerState
execCM action = snd <$> runStateT action createCompilerState

spec :: Spec
spec = describe "Compiler.ASM.CompilerMonad (coverage maximale)" $ do

  describe "Spec helpers coverage" $ do
    it "expectRight throws on Left" $ do
      evaluate (expectRight (Left ("boom" :: T.Text) :: Either T.Text Int))
        `shouldThrow` anyErrorCall

    it "expectLeft throws on Right" $ do
      evaluate (expectLeft (Right (123 :: Int) :: Either T.Text Int))
        `shouldThrow` anyErrorCall

  describe "Individual Instruction Emitters" $ do
    it "emitJumpToLabel emits JumpLabel" $ do
      let st = expectRight (execCM (emitJumpToLabel "L1"))
      csCode st `shouldBe` Seq.singleton (JumpLabel "L1")

    it "emitJumpIfFalseToLabel emits JumpIfFalseLabel" $ do
      let st = expectRight (execCM (emitJumpIfFalseToLabel "L2"))
      csCode st `shouldBe` Seq.singleton (JumpIfFalseLabel "L2")

    it "emitJumpIfTrueToLabel emits JumpIfTrueLabel" $ do
      let st = expectRight (execCM (emitJumpIfTrueToLabel "L3"))
      csCode st `shouldBe` Seq.singleton (JumpIfTrueLabel "L3")

    it "emitCallToLabel emits CallLabel" $ do
      let st = expectRight (execCM (emitCallToLabel "func"))
      csCode st `shouldBe` Seq.singleton (CallLabel "func")

  describe "Label Generation" $ do
    it "generateUniqueLabel increments counter and returns unique labels" $ do
      let action = do
            l1 <- generateUniqueLabel "label"
            l2 <- generateUniqueLabel "label"
            return (l1, l2)

      let ((l1, l2), st) = expectRight (runStateT action createCompilerState)
      l1 `shouldBe` "label_1"
      l2 `shouldBe` "label_2"
      csLabelCnt st `shouldBe` 2

  describe "Symbol Management" $ do
    it "defineSymbol assigns indices and increments csNextIndex" $ do
      let action = do
            i1 <- defineSymbol "varA"
            i2 <- defineSymbol "varB"
            return (i1, i2)

      let ((i1, i2), st) = expectRight (runStateT action createCompilerState)
      i1 `shouldBe` 0
      i2 `shouldBe` 1
      csNextIndex st `shouldBe` 2
      Map.lookup "varA" (csSymbols st) `shouldBe` Just 0
      Map.lookup "varB" (csSymbols st) `shouldBe` Just 1

    it "defineSymbol fails if symbol already exists" $ do
      let action = do
            _ <- defineSymbol "conflict"
            _ <- defineSymbol "conflict"
            return ()

      let err = expectLeft (execCM action)
      err `shouldBe` "Symbol already defined: conflict"

  describe "Complex Scenario (State Integrity)" $ do
    it "maintains consistency through a mixed sequence of operations" $ do
      let action = do
            emitLabelDefinition "start"
            idx <- defineSymbol "x"
            emitInstruction (Push (ImmInt (I32 100)))
            emitInstruction (StoreGlobal idx)
            lbl <- generateUniqueLabel "loop"
            emitLabelDefinition lbl
            emitJumpIfFalseToLabel "end"
            emitJumpToLabel lbl
            emitLabelDefinition "end"
            emitInstruction Halt
            return ()

      let ((), st) = expectRight (runStateT action createCompilerState)
      csNextIndex st `shouldBe` 1
      csLabelCnt st `shouldBe` 1
      Map.size (csSymbols st) `shouldBe` 1
      Seq.length (csCode st) `shouldBe` 8
      Seq.index (csCode st) 7 `shouldBe` Real Halt
      Seq.index (csCode st) 3 `shouldBe` LabelDef "loop_1"

  describe "Error Branch Coverage" $ do
    it "propagates Left from the underlying Either layer" $ do
      let action = (lift (Left "Manual Error") :: CompilerMonad ())
      let err = expectLeft (runStateT action createCompilerState)
      err `shouldBe` "Manual Error"
