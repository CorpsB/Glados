{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- IndexSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.Instruction.IndexSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Bits (shiftR)
import Data.Word (Word8, Word32)
import Data.Int (Int32)

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import VM.Instruction.Index (instJump, instJumpIfFalse, instJumpIfTrue)

runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

beI32 :: Int -> [Word8]
beI32 n =
  let w :: Word32
      w = fromIntegral (fromIntegral n :: Int32)
  in [ fromIntegral (w `shiftR` 24)
     , fromIntegral (w `shiftR` 16)
     , fromIntegral (w `shiftR` 8)
     , fromIntegral w
     ]

mkVM :: [Word8] -> [VMValue] -> VMState
mkVM bytes stk =
  (createVMState (BS.pack bytes) False)
    { vStack = V.fromList stk
    , bytecodeIndex = 0
    }

spec :: Spec
spec = describe "VM.Instruction.Index" $ do

  describe "instJump" $ do
    it "adds offset to bytecodeIndex (offset relative to post-read index)" $ do
      let vm0 = mkVM (beI32 5) []
      (_, vm1) <- runVM instJump vm0
      bytecodeIndex vm1 `shouldBe` 9

    it "supports negative offsets" $ do
      let vm0 = mkVM (beI32 (-2)) []
      (_, vm1) <- runVM instJump vm0
      bytecodeIndex vm1 `shouldBe` 2

  describe "instJumpIfFalse" $ do
    it "jumps when condition is VBool False" $ do
      let vm0 = mkVM (beI32 10) [VBool False]
      (_, vm1) <- runVM instJumpIfFalse vm0
      bytecodeIndex vm1 `shouldBe` 14
      vStack vm1 `shouldBe` V.empty

    it "does not jump when condition is VBool True (fall-through)" $ do
      let vm0 = mkVM (beI32 10) [VBool True]
      (_, vm1) <- runVM instJumpIfFalse vm0
      bytecodeIndex vm1 `shouldBe` 4
      vStack vm1 `shouldBe` V.empty

  describe "instJumpIfTrue" $ do
    it "jumps when condition is VBool True" $ do
      let vm0 = mkVM (beI32 10) [VBool True]
      (_, vm1) <- runVM instJumpIfTrue vm0
      bytecodeIndex vm1 `shouldBe` 14
      vStack vm1 `shouldBe` V.empty

    it "does not jump when condition is VBool False (fall-through)" $ do
      let vm0 = mkVM (beI32 10) [VBool False]
      (_, vm1) <- runVM instJumpIfTrue vm0
      bytecodeIndex vm1 `shouldBe` 4
      vStack vm1 `shouldBe` V.empty
