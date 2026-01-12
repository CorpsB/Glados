{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VMStackSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.VMStackSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Word (Word8)

import VM.VMStack (stackPush, stackPop, stackTop)
import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))

-- Helpers
runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

mkVM :: [Word8] -> VMState
mkVM bytes = createVMState (BS.pack bytes)

spec :: Spec
spec = describe "VM.VMStack" $ do

  describe "stackPush" $ do
    it "pushes a value onto an empty stack" $ do
      let vm0 = mkVM []
      (_, vm1) <- runVM (stackPush (VInt (I8 42))) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I8 42)]

    it "pushes multiple values and preserves order" $ do
      let vm0 = mkVM []
      (_, vm1) <- runVM (stackPush (VInt (I8 1)) >> stackPush (VBool True) >> stackPush VVoid) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I8 1), VBool True, VVoid]

  describe "stackTop" $ do
    it "returns the top value without removing it" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VInt (I8 1), VBool False] }
      (topVal, vm1) <- runVM stackTop vm0
      topVal `shouldBe` VBool False
      vStack vm1 `shouldBe` V.fromList [VInt (I8 1), VBool False]

    it "throws Stack Underflow (Top) on empty stack" $ do
      let vm0 = mkVM []
      evalVM stackTop vm0 `shouldThrow` errorCall "VM Error: Stack Underflow (Top)"

  describe "stackPop" $ do
    it "pops the top value and removes it from the stack" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VInt (I8 1), VBool True, VVoid] }
      (popped, vm1) <- runVM stackPop vm0
      popped `shouldBe` VVoid
      vStack vm1 `shouldBe` V.fromList [VInt (I8 1), VBool True]

    it "can pop until the stack becomes empty" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VInt (I8 9)] }
      (p1, vm1) <- runVM stackPop vm0
      p1 `shouldBe` VInt (I8 9)
      vStack vm1 `shouldBe` V.empty

    it "throws Stack Underflow on empty stack" $ do
      let vm0 = mkVM []
      evalVM stackPop vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"

  describe "integration: push/top/pop behavior" $ do
    it "top after pushes reflects last pushed, and pop returns that same value" $ do
      let vm0 = mkVM []
      (topVal, vm1) <- runVM (stackPush (VInt (I8 1)) >> stackPush (VBool True) >> stackTop) vm0
      topVal `shouldBe` VBool True

      (popped, vm2) <- runVM stackPop vm1
      popped `shouldBe` VBool True
      vStack vm2 `shouldBe` V.fromList [VInt (I8 1)]
