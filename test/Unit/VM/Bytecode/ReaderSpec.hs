{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ReaderSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.Bytecode.ReaderSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int64)

import VM.Bytecode.Reader
import VM.VMState (VMState(..), VirtualMachine)

-- ⚠️ Si ton VMState a d'autres champs, remplace ce constructeur par ton "createVMState"
-- puis fais un record update : createVMState { bytecode = ..., bytecodeIndex = ... }
mkState :: [Word8] -> Int -> VMState
mkState bytes idx =
  VMState
    { bytecode = BS.pack bytes
    , bytecodeIndex = idx
    }

eobMsg :: String
eobMsg = "VM Error: Unexpected End of Bytecode (Segmentation Fault)"

-- Helpers
runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

spec :: Spec
spec = describe "VM.Bytecode.Reader" $ do

  describe "readByte" $ do
    it "reads the byte at current index and increments bytecodeIndex" $ do
      let st0 = mkState [0xAA, 0xBB] 0
      (b, st1) <- runVM readByte st0
      b `shouldBe` (0xAA :: Word8)
      bytecodeIndex st1 `shouldBe` 1

    it "reads using a non-zero start index" $ do
      let st0 = mkState [0xAA, 0xBB, 0xCC] 1
      (b, st1) <- runVM readByte st0
      b `shouldBe` (0xBB :: Word8)
      bytecodeIndex st1 `shouldBe` 2

    it "throws when index is out of bounds" $ do
      let st0 = mkState [] 0
      evalVM readByte st0 `shouldThrow` errorCall eobMsg

  describe "readWord8 / readInt8" $ do
    it "readWord8 behaves like readByte" $ do
      let st0 = mkState [0x01, 0x02] 0
      (w, st1) <- runVM readWord8 st0
      w `shouldBe` (0x01 :: Word8)
      bytecodeIndex st1 `shouldBe` 1

    it "readInt8 converts correctly for positive values" $ do
      let st0 = mkState [0x7F] 0
      (i, st1) <- runVM readInt8 st0
      i `shouldBe` (127 :: Int8)
      bytecodeIndex st1 `shouldBe` 1

    it "readInt8 converts correctly for negative values (two's complement)" $ do
      let st0 = mkState [0xFF] 0
      (i, _) <- runVM readInt8 st0
      i `shouldBe` (-1 :: Int8)

  describe "readWord16 / readInt16" $ do
    it "reads Word16 in Big Endian" $ do
      let st0 = mkState [0x12, 0x34, 0x99] 0
      (w, st1) <- runVM readWord16 st0
      w `shouldBe` (0x1234 :: Word16)
      bytecodeIndex st1 `shouldBe` 2

    it "reads Int16 with correct sign" $ do
      let st0 = mkState [0xFF, 0xFE] 0
      (i, _) <- runVM readInt16 st0
      i `shouldBe` (-2 :: Int16)

    it "throws if there is not enough bytecode for Word16" $ do
      let st0 = mkState [0x12] 0
      evalVM readWord16 st0 `shouldThrow` errorCall eobMsg

  describe "readWord32 / readInt32" $ do
    it "reads Word32 in Big Endian" $ do
      let st0 = mkState [0x01, 0x02, 0x03, 0x04, 0xFF] 0
      (w, st1) <- runVM readWord32 st0
      w `shouldBe` (0x01020304 :: Word32)
      bytecodeIndex st1 `shouldBe` 4

    it "reads Int32 (returned as native Int) for a positive value" $ do
      -- 0x7FFFFFFF = 2147483647
      let st0 = mkState [0x7F, 0xFF, 0xFF, 0xFF] 0
      (i, _) <- runVM readInt32 st0
      i `shouldBe` (2147483647 :: Int)

    it "reads Int32 (returned as native Int) for a negative value" $ do
      -- 0xFFFFFFFE = -2 as Int32
      let st0 = mkState [0xFF, 0xFF, 0xFF, 0xFE] 0
      (i, _) <- runVM readInt32 st0
      i `shouldBe` (-2 :: Int)

    it "reads Int32 minimum correctly (sign extension)" $ do
      -- 0x80000000 = -2147483648 as Int32
      let st0 = mkState [0x80, 0x00, 0x00, 0x00] 0
      (i, _) <- runVM readInt32 st0
      i `shouldBe` (-2147483648 :: Int)

    it "throws if there is not enough bytecode for Word32" $ do
      let st0 = mkState [0x01, 0x02, 0x03] 0
      evalVM readWord32 st0 `shouldThrow` errorCall eobMsg

  describe "readWord64 / readInt64" $ do
    it "reads Word64 in Big Endian" $ do
      let st0 = mkState [0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0xFF] 0
      (w, st1) <- runVM readWord64 st0
      w `shouldBe` (0x0102030405060708 :: Word64)
      bytecodeIndex st1 `shouldBe` 8

    it "reads Int64 with correct sign (-1)" $ do
      let st0 = mkState [0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF] 0
      (i, _) <- runVM readInt64 st0
      i `shouldBe` (-1 :: Int64)

    it "reads Int64 minimum correctly" $ do
      -- 0x8000000000000000 = minBound :: Int64
      let st0 = mkState [0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00] 0
      (i, _) <- runVM readInt64 st0
      i `shouldBe` (minBound :: Int64)

    it "throws if there is not enough bytecode for Word64" $ do
      let st0 = mkState [0x01,0x02,0x03,0x04,0x05,0x06,0x07] 0
      evalVM readWord64 st0 `shouldThrow` errorCall eobMsg

  describe "index progression across mixed reads" $ do
    it "increments index by the correct amount when chaining reads" $ do
      -- Word16 consumes 2 bytes, then Word8 consumes 1 => total 3
      let st0 = mkState [0x00, 0x10, 0xAB, 0xCD] 0
      (w16, st1) <- runVM readWord16 st0
      w16 `shouldBe` (0x0010 :: Word16)
      bytecodeIndex st1 `shouldBe` 2

      (w8, st2) <- runVM readWord8 st1
      w8 `shouldBe` (0xAB :: Word8)
      bytecodeIndex st2 `shouldBe` 3
