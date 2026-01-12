{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- SerializerSpec (Updated for ASM_SPEC v0x02)
-}

module Compiler.SerializerSpec (spec) where

import Test.Hspec
import Data.Word()
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS

import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.Bytecode.Serializer (serializeInstruction)
import Common.Type.Integer (IntValue(..))

runBuilder :: B.Builder -> [Int]
runBuilder b = map fromEnum $ LBS.unpack (B.toLazyByteString b)

spec :: Spec
spec = describe "serializeInstruction (Instruction)" $ do
    it "Push (ImmInt (I32 5)): Opcode (0x01) + TypeID (0x05) + Int32" $ do
        let bytes = runBuilder (serializeInstruction (Push (ImmInt (I32 5))))
        -- 0x01 = Push, 0x05 = TypeID i32, 0x00000005 = Value
        bytes `shouldBe` [1, 5, 0, 0, 0, 5]

    it "Push (ImmBool True): Opcode (0x01) + TypeID (0x00) + Bool" $ do
        let bytes = runBuilder (serializeInstruction (Push (ImmBool True)))
        -- 0x01 = Push, 0x00 = TypeID Bool, 0x01 = Value True
        bytes `shouldBe` [1, 0, 1]

    it "Push (ImmBool False): Opcode (0x01) + TypeID (0x00) + Bool" $ do
        let bytes = runBuilder (serializeInstruction (Push (ImmBool False)))
        -- 0x01 = Push, 0x00 = TypeID Bool, 0x00 = Value False
        bytes `shouldBe` [1, 0, 0]

    it "Pop: opcode 0x02" $ do
        let bytes = runBuilder (serializeInstruction Pop)
        bytes `shouldBe` [2]

    it "Dup: opcode 0x03" $ do
        let bytes = runBuilder (serializeInstruction Dup)
        bytes `shouldBe` [3]

    it "Swap: opcode 0x04" $ do
        let bytes = runBuilder (serializeInstruction Swap)
        bytes `shouldBe` [4]

    it "Add: opcode 0x10 (16)" $ do
        let bytes = runBuilder (serializeInstruction Add)
        bytes `shouldBe` [16]

    it "Sub: opcode 0x11 (17)" $ do
        let bytes = runBuilder (serializeInstruction Sub)
        bytes `shouldBe` [17]

    it "Jump 42: opcode 0x30 (48) + int32" $ do
        let bytes = runBuilder (serializeInstruction (Jump 42))
        bytes `shouldBe` [48, 0, 0, 0, 42]

    it "JumpIfFalse 99: opcode 0x31 (49) + int32" $ do
        let bytes = runBuilder (serializeInstruction (JumpIfFalse 99))
        bytes `shouldBe` [49, 0, 0, 0, 99]

    it "Call 55: opcode 0x40 (64) + int32" $ do
        let bytes = runBuilder (serializeInstruction (Call 55))
        bytes `shouldBe` [64, 0, 0, 0, 55]

    it "Ret: opcode 0x43 (67)" $ do
        let bytes = runBuilder (serializeInstruction Ret)
        bytes `shouldBe` [67]

    it "MakeClosure 7 2: opcode 0x60 (96) + addr + count" $ do
        let bytes = runBuilder (serializeInstruction (MakeClosure 7 2))
        bytes `shouldBe` [96, 0, 0, 0, 7, 0, 0, 0, 2]

    it "Halt: opcode 0x71 (113)" $ do
        let bytes = runBuilder (serializeInstruction Halt)
        bytes `shouldBe` [113]
