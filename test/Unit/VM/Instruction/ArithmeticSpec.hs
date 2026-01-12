{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ArithmeticSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.Instruction.ArithmeticSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Control.Exception (evaluate, ArithException(..))

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..), intValueToInt)

import VM.Instruction.Arithmetic
  ( instAdd, instSub, instMul, instDiv, instMod )

runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

mkVM :: [VMValue] -> VMState
mkVM stk =
  (createVMState BS.empty)
    { vStack = V.fromList stk }

forceTopInt :: VMState -> IO Int
forceTopInt st =
  case V.last (vStack st) of
    VInt iv -> evaluate (intValueToInt iv)
    x       -> error ("expected VInt on top, got: " ++ show x)

spec :: Spec
spec = describe "VM.Instruction.Arithmetic" $ do

  describe "instAdd" $ do
    it "adds two integers and pushes an I64 result" $ do
      let vm0 = mkVM [VInt (I64 10), VInt (I64 32)]
      (_, vm1) <- runVM instAdd vm0
      vStack vm1 `shouldSatisfy` \s -> V.length s == 1
      forceTopInt vm1 `shouldReturn` 42

    it "preserves lower stack elements (only consumes top two)" $ do
      let vm0 = mkVM [VBool True, VInt (I64 1), VInt (I64 2)]
      (_, vm1) <- runVM instAdd vm0
      V.toList (vStack vm1) `shouldBe` [VBool True, VInt (I64 3)]

  describe "instSub" $ do
    it "subtracts with correct operand order: [a,b] -> a - b" $ do
      let vm0 = mkVM [VInt (I64 10), VInt (I64 3)]
      (_, vm1) <- runVM instSub vm0
      forceTopInt vm1 `shouldReturn` 7

    it "shows operand order matters (a - b, not b - a)" $ do
      let vm0 = mkVM [VInt (I64 3), VInt (I64 10)]
      (_, vm1) <- runVM instSub vm0
      forceTopInt vm1 `shouldReturn` (-7)

  describe "instMul" $ do
    it "multiplies two integers" $ do
      let vm0 = mkVM [VInt (I64 6), VInt (I64 7)]
      (_, vm1) <- runVM instMul vm0
      forceTopInt vm1 `shouldReturn` 42

  describe "instDiv" $ do
    it "divides two integers using integer division" $ do
      let vm0 = mkVM [VInt (I64 7), VInt (I64 2)]
      (_, vm1) <- runVM instDiv vm0
      forceTopInt vm1 `shouldReturn` 3

    it "throws DivideByZero when dividing by zero (forced)" $ do
      let vm0 = mkVM [VInt (I64 10), VInt (I64 0)]
      (_, vm1) <- runVM instDiv vm0
      (forceTopInt vm1) `shouldThrow` (== DivideByZero)

  describe "instMod" $ do
    it "computes modulo" $ do
      let vm0 = mkVM [VInt (I64 10), VInt (I64 3)]
      (_, vm1) <- runVM instMod vm0
      forceTopInt vm1 `shouldReturn` 1

    it "throws DivideByZero when modulo by zero (forced)" $ do
      let vm0 = mkVM [VInt (I64 10), VInt (I64 0)]
      (_, vm1) <- runVM instMod vm0
      (forceTopInt vm1) `shouldThrow` (== DivideByZero)

  describe "type errors (non-integers)" $ do
    it "throws when right operand is not an integer" $ do
      let vm0 = mkVM [VInt (I64 1), VBool True]
      evalVM instAdd vm0 `shouldThrow` errorCall "VM Error: Arithmetic instruction expects Integers"

    it "throws when left operand is not an integer" $ do
      let vm0 = mkVM [VBool True, VInt (I64 1)]
      evalVM instAdd vm0 `shouldThrow` errorCall "VM Error: Arithmetic instruction expects Integers"

  describe "stack underflow propagation" $ do
    it "throws Stack Underflow on empty stack (needs 2 operands)" $ do
      let vm0 = mkVM []
      evalVM instAdd vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"

    it "throws Stack Underflow when only one operand is present" $ do
      let vm0 = mkVM [VInt (I64 1)]
      evalVM instAdd vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"
