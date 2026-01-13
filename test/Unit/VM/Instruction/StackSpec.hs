{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Stack instructions unit tests
-}

{-# LANGUAGE OverloadedStrings #-}

module VM.Instruction.StackSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT)
import Control.Exception (ErrorCall(..))
import Data.List (isInfixOf)

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Bits (shiftR)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

import Common.Type.Integer (IntValue(..))
import VM.VMValue (VMValue(..))
import VM.VMState (VMState(..), createVMState)
import VM.Instruction.Stack (instPush, instPop, instDup, instSwap, instCast)

mkVM :: [Word8] -> VMState
mkVM bytes = createVMState (BS.pack bytes)

be16 :: Word16 -> [Word8]
be16 w = [fromIntegral (w `shiftR` 8), fromIntegral w]

be32 :: Word32 -> [Word8]
be32 w =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

be64 :: Word64 -> [Word8]
be64 w =
  [ fromIntegral (w `shiftR` 56)
  , fromIntegral (w `shiftR` 48)
  , fromIntegral (w `shiftR` 40)
  , fromIntegral (w `shiftR` 32)
  , fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

expectErrorContains :: String -> ErrorCall -> Bool
expectErrorContains needle (ErrorCall msg) = needle `isInfixOf` msg

spec :: Spec
spec = describe "VM.Instruction.Stack" $ do

  describe "instPush" $ do
    it "pushes Bool (TypeID 0x00): 0 -> #f, non-zero -> #t" $ do
      let vm0a = mkVM [0x00, 0x00]
      (_, vm1a) <- runStateT instPush vm0a
      vStack vm1a `shouldBe` V.fromList [VBool False]

      let vm0b = mkVM [0x00, 0x02]
      (_, vm1b) <- runStateT instPush vm0b
      vStack vm1b `shouldBe` V.fromList [VBool True]

    it "pushes Int8 (TypeID 0x01)" $ do
      let vm0 = mkVM [0x01, 0x7F]
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I8 (127 :: Int8))]

    it "pushes UInt8 (TypeID 0x02)" $ do
      let vm0 = mkVM [0x02, 0xFF]
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI8 255)]

    it "pushes Int16 (TypeID 0x03) big-endian" $ do
      let x = (-2 :: Int16)
      let vm0 = mkVM ([0x03] ++ be16 (fromIntegral x :: Word16))
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I16 x)]

    it "pushes UInt16 (TypeID 0x04) big-endian" $ do
      let w = (50000 :: Word16)
      let vm0 = mkVM ([0x04] ++ be16 w)
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI16 w)]

    it "pushes Int32 (TypeID 0x05) using readInt32 then casts into I32" $ do
      let x = (123456 :: Int32)
      let vm0 = mkVM ([0x05] ++ be32 (fromIntegral x :: Word32))
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I32 x)]

    it "pushes UInt32 (TypeID 0x06)" $ do
      let w = (0x01020304 :: Word32)
      let vm0 = mkVM ([0x06] ++ be32 w)
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI32 w)]

    it "pushes Int64 (TypeID 0x07)" $ do
      let x = (42 :: Int64)
      let vm0 = mkVM ([0x07] ++ be64 (fromIntegral x :: Word64))
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 x)]

    it "pushes UInt64 (TypeID 0x08)" $ do
      let w = (0x0102030405060708 :: Word64)
      let vm0 = mkVM ([0x08] ++ be64 w)
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UI64 w)]

    it "pushes Char (TypeID 0x09) as IChar (via readInt8)" $ do
      let c = (65 :: Int8)
      let vm0 = mkVM [0x09, fromIntegral c]
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (IChar c)]

    it "pushes UChar (TypeID 0x10) as UIChar (via readWord8)" $ do
      let vm0 = mkVM [0x10, 200]
      (_, vm1) <- runStateT instPush vm0
      vStack vm1 `shouldBe` V.fromList [VInt (UIChar 200)]

    it "throws on unsupported TypeID" $ do
      let vm0 = mkVM [0x99]
      runStateT instPush vm0 `shouldThrow` expectErrorContains "Unsupported PUSH TypeID"

    it "propagates End of Bytecode error when payload bytes are missing" $ do
      let vm0 = mkVM [0x07] -- says Int64 but missing 8 bytes
      runStateT instPush vm0 `shouldThrow` anyException

  describe "instPop" $ do
    it "removes and discards the top stack value" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VBool True] }
      (_, vm1) <- runStateT instPop vm0
      vStack vm1 `shouldBe` V.empty

    it "propagates Stack Underflow on empty stack" $ do
      let vm0 = (mkVM []) { vStack = V.empty }
      runStateT instPop vm0 `shouldThrow` anyException

  describe "instDup" $ do
    it "duplicates the top value" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VInt (I64 1)] }
      (_, vm1) <- runStateT instDup vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1), VInt (I64 1)]

    it "propagates Stack Underflow on empty stack" $ do
      let vm0 = (mkVM []) { vStack = V.empty }
      runStateT instDup vm0 `shouldThrow` anyException

  describe "instSwap" $ do
    it "swaps the top two values" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VInt (I64 1), VBool True] }
      (_, vm1) <- runStateT instSwap vm0
      vStack vm1 `shouldBe` V.fromList [VBool True, VInt (I64 1)]

    it "throws if stack has < 2 elements" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VInt (I64 1)] }
      runStateT instSwap vm0 `shouldThrow` anyException

  describe "instCast" $ do
    it "casts Int -> Bool (TypeID 0x00)" $ do
      let vm0 = (mkVM [0x00]) { vStack = V.fromList [VInt (I64 0)] }
      (_, vm1) <- runStateT instCast vm0
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "unknown TypeID does NOT throw: value stays unchanged (castValue fallback)" $ do
      let vm0 = (mkVM [0x99]) { vStack = V.fromList [VInt (I64 42)] }
      (_, vm1) <- runStateT instCast vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 42)]

    it "throws if bytecode has no TypeID to read" $ do
      let vm0 = (mkVM []) { vStack = V.fromList [VInt (I64 1)] }
      runStateT instCast vm0 `shouldThrow` anyException

    it "throws if stack is empty (Stack Underflow)" $ do
      let vm0 = (mkVM [0x00]) { vStack = V.empty }
      runStateT instCast vm0 `shouldThrow` anyException
