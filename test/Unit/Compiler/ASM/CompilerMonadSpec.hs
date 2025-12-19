{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerMonadSpec
-}

{-# LANGUAGE OverloadedStrings #-}
module Compiler.ASM.CompilerMonadSpec (spec) where

import Test.Hspec
import Control.Monad.State
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Compiler.CompilerState
import Compiler.PsInstruction
import Compiler.Instruction
import Compiler.ASM.CompilerMonad

-- Helper pour exécuter le monade et récupérer l'état
execCM :: CompilerMonad a -> Either T.Text CompilerState
execCM action = snd <$> runStateT action createCompilerState

spec :: Spec
spec = describe "Compiler.ASM.CompilerMonad" $ do

  describe "Instruction Emitters" $ do
    it "correctly emits every type of jump and call" $ do
      let action = do
            emitJumpToLabel "LabelJump"
            emitJumpIfFalseToLabel "LabelFalse"
            emitJumpIfTrueToLabel "LabelTrue"
            emitCallToLabel "LabelCall"
            emitInstruction Halt -- Test de emitInstruction de base
            
      let res = execCM action
      case res of
        Left err -> expectationFailure (T.unpack err)
        Right st -> csCode st `shouldBe` Seq.fromList 
          [ JumpLabel "LabelJump"
          , JumpIfFalseLabel "LabelFalse"
          , JumpIfTrueLabel "LabelTrue"
          , CallLabel "LabelCall"
          , Real Halt
          ]

  describe "Label Generation" $ do
    it "generateUniqueLabel increments counter" $ do
      let action = do
            l1 <- generateUniqueLabel "test"
            l2 <- generateUniqueLabel "test"
            return (l1, l2)
            
      let res = runStateT action createCompilerState
      case res of
        Right ((l1, l2), st) -> do
          l1 `shouldBe` "test_0"
          l2 `shouldBe` "test_1"
          csLabelCnt st `shouldBe` 2
        Left _ -> expectationFailure "Should not fail"

    it "emitLabelDefinition adds a LabelDef" $ do
      let Right st = execCM (emitLabelDefinition "Start")
      csCode st `shouldBe` Seq.singleton (LabelDef "Start")