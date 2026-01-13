{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- StructSpec
-}

{-# LANGUAGE LambdaCase #-}

module VM.Instruction.StructSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Control.Exception (ErrorCall(..))
import Data.List (isInfixOf)
import Data.Bits (shiftR)
import Data.Int (Int32)
import Data.Word (Word8, Word32)

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))

import VM.Instruction.Struct (instBuildStruct, instGetStructField)

runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

i8 :: Int -> VMValue
i8 n = VInt (I8 (fromIntegral n))

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
mkVM bcBytes stk =
  (createVMState (BS.pack bcBytes))
    { vStack = V.fromList stk
    , bytecodeIndex = 0
    }

spec :: Spec
spec = describe "VM.Instruction.Struct" $ do

  describe "instBuildStruct (BUILD_STRUCT 0x62)" $ do
    it "builds a struct from the last N fields and pushes VStruct" $ do
      -- count=2 => consumes last 2 stack values, pushes VStruct [..]
      let vm0 = mkVM (beI32 2) [i8 1, i8 10, i8 20]
      (_, vm1) <- runVM instBuildStruct vm0
      bytecodeIndex vm1 `shouldBe` 4
      vStack vm1 `shouldBe` V.fromList [i8 1, VStruct (V.fromList [i8 10, i8 20])]

    it "throws BUILD_STRUCT stack underflow if not enough values" $ do
      let vm0 = mkVM (beI32 3) [i8 1, i8 2]
      evalVM instBuildStruct vm0 `shouldThrow` \(ErrorCall msg) ->
        "VM Error: BUILD_STRUCT Stack Underflow" `isInfixOf` msg

  describe "instGetStructField (GET_STRUCT_FIELD 0x63)" $ do
    it "pushes the struct field at index (struct is consumed)" $ do
      let st = VStruct (V.fromList [i8 10, i8 20])
      let vm0 = mkVM (beI32 1) [i8 99, st] -- struct on top
      (_, vm1) <- runVM instGetStructField vm0
      bytecodeIndex vm1 `shouldBe` 4
      vStack vm1 `shouldBe` V.fromList [i8 99, i8 20]

    it "throws out of bounds when index is invalid" $ do
      let st = VStruct (V.fromList [i8 10, i8 20])
      let vm0 = mkVM (beI32 2) [st]
      evalVM instGetStructField vm0 `shouldThrow` \(ErrorCall msg) ->
        "VM Error: Struct Field Access Out of Bounds (2)" `isInfixOf` msg

    it "throws when top of stack is not a struct" $ do
      let vm0 = mkVM (beI32 0) [i8 1]
      evalVM instGetStructField vm0 `shouldThrow` \(ErrorCall msg) ->
        "VM Error: GET_STRUCT_FIELD expects a Struct" `isInfixOf` msg
