{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- LogicSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.Instruction.LogicSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))

import VM.Instruction.Logic
  ( instEq
  , instLt
  , instLe
  , instNot
  , instAnd
  , instOr
  )

-- Helpers
runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

mkVM :: [VMValue] -> VMState
mkVM stk =
  (createVMState BS.empty False)
    { vStack = V.fromList stk
    }

execOp :: VirtualMachine () -> [VMValue] -> IO VMState
execOp op stk = snd <$> runVM op (mkVM stk)

spec :: Spec
spec = describe "VM.Instruction.Logic" $ do

  describe "instNot" $ do
    it "negates True -> False" $ do
      vm1 <- execOp instNot [VBool True]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "negates False -> True" $ do
      vm1 <- execOp instNot [VBool False]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "throws when operand is not a boolean" $ do
      let vm0 = mkVM [VInt (I64 1)]
      evalVM instNot vm0 `shouldThrow` errorCall "VM Error: NOT expects Boolean"

    it "propagates Stack Underflow on empty stack" $ do
      let vm0 = mkVM []
      evalVM instNot vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"

  describe "instAnd" $ do
    it "computes True && True -> True" $ do
      vm1 <- execOp instAnd [VBool True, VBool True]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "computes True && False -> False" $ do
      vm1 <- execOp instAnd [VBool True, VBool False]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "computes False && True -> False" $ do
      vm1 <- execOp instAnd [VBool False, VBool True]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "throws when operands are not both booleans" $ do
      let vm0 = mkVM [VBool True, VInt (I64 0)]
      evalVM instAnd vm0 `shouldThrow` errorCall "VM Error: AND expects Booleans"

    it "propagates Stack Underflow when only one operand is present" $ do
      let vm0 = mkVM [VBool True]
      evalVM instAnd vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"

  describe "instOr" $ do
    it "computes False || False -> False" $ do
      vm1 <- execOp instOr [VBool False, VBool False]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "computes True || False -> True" $ do
      vm1 <- execOp instOr [VBool True, VBool False]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "computes False || True -> True" $ do
      vm1 <- execOp instOr [VBool False, VBool True]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "throws when operands are not both booleans" $ do
      let vm0 = mkVM [VInt (I64 1), VBool True]
      evalVM instOr vm0 `shouldThrow` errorCall "VM Error: OR expects Booleans"

    it "propagates Stack Underflow when only one operand is present" $ do
      let vm0 = mkVM [VBool False]
      evalVM instOr vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"

  describe "instEq" $ do
    it "compares integers equality (equal)" $ do
      vm1 <- execOp instEq [VInt (I64 5), VInt (I64 5)]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "compares integers equality (not equal)" $ do
      vm1 <- execOp instEq [VInt (I64 5), VInt (I64 6)]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "compares booleans equality (equal)" $ do
      vm1 <- execOp instEq [VBool True, VBool True]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "compares booleans equality (not equal)" $ do
      vm1 <- execOp instEq [VBool True, VBool False]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "throws on type mismatch (int vs bool)" $ do
      vm1 <- execOp instEq [VInt (I64 1), VBool False]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "propagates Stack Underflow when operands are missing" $ do
      evalVM instEq (mkVM []) `shouldThrow` errorCall "VM Error: Stack Underflow"
      evalVM instEq (mkVM [VBool True]) `shouldThrow` errorCall "VM Error: Stack Underflow"

  describe "instLt" $ do
    it "computes a < b with correct operand order ([a,b] -> a < b)" $ do
      vm1 <- execOp instLt [VInt (I64 2), VInt (I64 5)]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "returns False when a is not less than b" $ do
      vm1 <- execOp instLt [VInt (I64 5), VInt (I64 2)]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "throws when operands are not integers" $ do
      let vm0 = mkVM [VBool True, VBool False]
      evalVM instLt vm0 `shouldThrow` errorCall "VM Error: LT expects Integers"

    it "propagates Stack Underflow when operands are missing" $ do
      evalVM instLt (mkVM []) `shouldThrow` errorCall "VM Error: Stack Underflow"
      evalVM instLt (mkVM [VInt (I64 1)]) `shouldThrow` errorCall "VM Error: Stack Underflow"

  describe "instLe" $ do
    it "computes a <= b (True when equal)" $ do
      vm1 <- execOp instLe [VInt (I64 3), VInt (I64 3)]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "computes a <= b (True when smaller)" $ do
      vm1 <- execOp instLe [VInt (I64 2), VInt (I64 5)]
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "computes a <= b (False when greater)" $ do
      vm1 <- execOp instLe [VInt (I64 5), VInt (I64 2)]
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "throws when operands are not integers" $ do
      let vm0 = mkVM [VBool True, VInt (I64 1)]
      evalVM instLe vm0 `shouldThrow` errorCall "VM Error: LE expects Integers"

    it "propagates Stack Underflow when operands are missing" $ do
      evalVM instLe (mkVM []) `shouldThrow` errorCall "VM Error: Stack Underflow"
      evalVM instLe (mkVM [VInt (I64 1)]) `shouldThrow` errorCall "VM Error: Stack Underflow"
