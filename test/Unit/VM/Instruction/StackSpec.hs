{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- StackInstructionSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.Instruction.StackSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Bits (shiftR)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Control.Exception (ErrorCall(..))
import Data.List (isPrefixOf)

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))

import VM.Instruction.Stack (instPush, instPop, instDup)

-- Helpers
runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

mkVM :: [Word8] -> [VMValue] -> Int -> VMState
mkVM bytes stk ip =
  (createVMState (BS.pack bytes) False)
    { vStack = V.fromList stk
    , bytecodeIndex = ip
    }

-- Big-endian encoders matching VM.Bytecode.Reader
beW16 :: Word16 -> [Word8]
beW16 w =
  [ fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

beW32 :: Word32 -> [Word8]
beW32 w =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

beW64 :: Word64 -> [Word8]
beW64 w =
  [ fromIntegral (w `shiftR` 56)
  , fromIntegral (w `shiftR` 48)
  , fromIntegral (w `shiftR` 40)
  , fromIntegral (w `shiftR` 32)
  , fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

beI32 :: Int -> [Word8]
beI32 n =
  let w :: Word32
      w = fromIntegral (fromIntegral n :: Int32) -- 2's complement
  in beW32 w

spec :: Spec
spec = describe "VM.Instruction.Stack" $ do

  describe "instPush" $ do
    it "pushes Bool (TypeID 0x00): 0 -> #f, non-zero -> #t" $ do
      let codeFalse = [0x00, 0x00] -- typeId, value
      let vm0 = mkVM codeFalse [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VBool False]
      bytecodeIndex vm1 `shouldBe` 2

      let codeTrue = [0x00, 0x2A]
      let vm2 = mkVM codeTrue [] 0
      (_, vm3) <- runVM instPush vm2
      vStack vm3 `shouldBe` V.fromList [VBool True]
      bytecodeIndex vm3 `shouldBe` 2

    it "pushes Int8 (TypeID 0x01)" $ do
      let code = [0x01, 0xFF] -- -1
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I8 (-1 :: Int8))]
      bytecodeIndex vm1 `shouldBe` 2

    it "pushes UInt8 (TypeID 0x02)" $ do
      let code = [0x02, 0xFE]
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI8 254)]
      bytecodeIndex vm1 `shouldBe` 2

    it "pushes Int16 (TypeID 0x03) big-endian" $ do
      let val :: Int16
          val = -2
      let w :: Word16
          w = fromIntegral val
      let code = [0x03] ++ beW16 w
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I16 val)]
      bytecodeIndex vm1 `shouldBe` 3

    it "pushes UInt16 (TypeID 0x04) big-endian" $ do
      let code = [0x04] ++ beW16 0x1234
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI16 0x1234)]
      bytecodeIndex vm1 `shouldBe` 3

    it "pushes Int32 (TypeID 0x05) using readInt32 then casts into I32" $ do
      let code = [0x05] ++ beI32 (-12345)
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I32 (-12345))]
      bytecodeIndex vm1 `shouldBe` 5

    it "pushes UInt32 (TypeID 0x06)" $ do
      let code = [0x06] ++ beW32 0x89ABCDEF
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI32 0x89ABCDEF)]
      bytecodeIndex vm1 `shouldBe` 5

    it "pushes Int64 (TypeID 0x07)" $ do
      let val :: Int64
          val = -1
      let w :: Word64
          w = fromIntegral val
      let code = [0x07] ++ beW64 w
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 val)]
      bytecodeIndex vm1 `shouldBe` 9

    it "pushes UInt64 (TypeID 0x08)" $ do
      let code = [0x08] ++ beW64 0x0102030405060708
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI64 0x0102030405060708)]
      bytecodeIndex vm1 `shouldBe` 9

    it "pushes Char (TypeID 0x09) as IChar (via readInt8)" $ do
      let code = [0x09, 0x41] -- 'A' as 65
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (IChar (65 :: Int8))]
      bytecodeIndex vm1 `shouldBe` 2

    it "pushes UChar (TypeID 0x10) as UIChar (via readWord8)" $ do
      let code = [0x10, 0xFF]
      let vm0 = mkVM code [] 0
      (_, vm1) <- runVM instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UIChar 255)]
      bytecodeIndex vm1 `shouldBe` 2

    it "throws on unsupported TypeID" $ do
      let code = [0x99, 0x00]
      let vm0 = mkVM code [] 0
      evalVM instPush vm0
        `shouldThrow` \e -> case e of
          ErrorCall msg -> "VM Error: Unsupported PUSH TypeID:" `isPrefixOf` msg
          _             -> False

    it "propagates End of Bytecode error when value bytes are missing" $ do
      -- asks for Int32 but only provides 1 byte of payload
      let code = [0x05, 0x01]
      let vm0 = mkVM code [] 0
      evalVM instPush vm0
        `shouldThrow` errorCall "VM Error: Unexpected End of Bytecode (Segmentation Fault)"

  describe "instPop" $ do
    it "removes and discards the top stack value" $ do
      let vm0 = mkVM [] [VInt (I64 1), VBool True] 0
      (_, vm1) <- runVM instPop vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1)]

    it "propagates Stack Underflow on empty stack" $ do
      let vm0 = mkVM [] [] 0
      evalVM instPop vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"

  describe "instDup" $ do
    it "duplicates the top value" $ do
      let vm0 = mkVM [] [VInt (I64 1), VBool False] 0
      (_, vm1) <- runVM instDup vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1), VBool False, VBool False]

    it "propagates Stack Underflow (Top) on empty stack" $ do
      let vm0 = mkVM [] [] 0
      evalVM instDup vm0 `shouldThrow` errorCall "VM Error: Stack Underflow (Top)"
