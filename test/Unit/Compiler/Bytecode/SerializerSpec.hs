{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- SerializerSpec (Updated for ASM_SPEC v0x02)
-}

module Compiler.Bytecode.SerializerSpec (spec) where

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
spec = describe "serializeInstruction Coverage" $ do
    
    describe "Push Operations (Integers & Types)" $ do
        it "Push (ImmBool True): Opcode (0x01) + TypeID (0x00) + 1" $ do
            runBuilder (serializeInstruction (Push (ImmBool True))) `shouldBe` [1, 0, 1]

        it "Push (ImmInt (I8 10)): Opcode (0x01) + TypeID (0x01) + Val" $ do
            runBuilder (serializeInstruction (Push (ImmInt (I8 10)))) `shouldBe` [1, 1, 10]

        it "Push (ImmInt (UI8 255)): Opcode (0x01) + TypeID (0x02) + Val" $ do
            runBuilder (serializeInstruction (Push (ImmInt (UI8 255)))) `shouldBe` [1, 2, 255]

        it "Push (ImmInt (I16 258)): Opcode (0x01) + TypeID (0x03) + Val (BigEndian)" $ do
            -- 258 = 0x0102
            runBuilder (serializeInstruction (Push (ImmInt (I16 258)))) `shouldBe` [1, 3, 1, 2]

        it "Push (ImmInt (I32 5)): Opcode (0x01) + TypeID (0x05) + Val" $ do
            runBuilder (serializeInstruction (Push (ImmInt (I32 5)))) `shouldBe` [1, 5, 0, 0, 0, 5]

        it "Push (ImmInt (I64 1)): Opcode (0x01) + TypeID (0x07) + Val (8 bytes)" $ do
            runBuilder (serializeInstruction (Push (ImmInt (I64 1)))) `shouldBe` [1, 7, 0, 0, 0, 0, 0, 0, 0, 1]

        it "Push (ImmInt (IChar 65)): Opcode (0x01) + TypeID (0x09) + Val" $ do
            runBuilder (serializeInstruction (Push (ImmInt (IChar 65)))) `shouldBe` [1, 9, 65]

        it "Push (ImmInt (UI16 50000)): Opcode (0x01) + TypeID (0x04) + Val (0xC350 BigEndian)" $ do
            -- 50000 = 0xC350 -> [195, 80]
            runBuilder (serializeInstruction (Push (ImmInt (UI16 50000)))) `shouldBe` [1, 4, 195, 80]

        it "Push (ImmInt (UI32 3735928559)): Opcode (0x01) + TypeID (0x06) + Val (0xDEADBEEF BigEndian)" $ do
            -- 0xDEADBEEF -> [222, 173, 190, 239]
            runBuilder (serializeInstruction (Push (ImmInt (UI32 3735928559)))) `shouldBe` [1, 6, 222, 173, 190, 239]

        it "Push (ImmInt (UI64 10)): Opcode (0x01) + TypeID (0x08) + Val (BigEndian)" $ do
            runBuilder (serializeInstruction (Push (ImmInt (UI64 10)))) `shouldBe` [1, 8, 0, 0, 0, 0, 0, 0, 0, 10]

        it "Push (ImmInt (UIChar 200)): Opcode (0x01) + TypeID (0x10) + Val" $ do
            runBuilder (serializeInstruction (Push (ImmInt (UIChar 200)))) `shouldBe` [1, 16, 200]

    describe "Stack Manipulation" $ do
        it "Pop: opcode 0x02" $ do
            runBuilder (serializeInstruction Pop) `shouldBe` [2]
        it "Dup: opcode 0x03" $ do
            runBuilder (serializeInstruction Dup) `shouldBe` [3]
        it "Swap: opcode 0x04" $ do
            runBuilder (serializeInstruction Swap) `shouldBe` [4]

    describe "Arithmetic Operations" $ do
        it "Add: opcode 0x10" $ runBuilder (serializeInstruction Add) `shouldBe` [16]
        it "Sub: opcode 0x11" $ runBuilder (serializeInstruction Sub) `shouldBe` [17]
        it "Mul: opcode 0x12" $ runBuilder (serializeInstruction Mul) `shouldBe` [18]
        it "Div: opcode 0x13" $ runBuilder (serializeInstruction Div) `shouldBe` [19]
        it "Mod: opcode 0x14" $ runBuilder (serializeInstruction Mod) `shouldBe` [20]

    describe "Comparison & Logic" $ do
        it "Eq: opcode 0x20"  $ runBuilder (serializeInstruction Eq)  `shouldBe` [32]
        it "Lt: opcode 0x21"  $ runBuilder (serializeInstruction Lt)  `shouldBe` [33]
        it "Not: opcode 0x22" $ runBuilder (serializeInstruction Not) `shouldBe` [34]
        it "And: opcode 0x23" $ runBuilder (serializeInstruction And) `shouldBe` [35]
        it "Or: opcode 0x24"  $ runBuilder (serializeInstruction Or)  `shouldBe` [36]
        it "Le: opcode 0x25"  $ runBuilder (serializeInstruction Le)  `shouldBe` [37]

    describe "Control Flow (Jumps)" $ do
        it "Jump 42: opcode 0x30 + int32" $ do
            runBuilder (serializeInstruction (Jump 42)) `shouldBe` [48, 0, 0, 0, 42]
        it "JumpIfFalse 99: opcode 0x31 + int32" $ do
            runBuilder (serializeInstruction (JumpIfFalse 99)) `shouldBe` [49, 0, 0, 0, 99]
        it "JumpIfTrue 10: opcode 0x32 + int32" $ do
            runBuilder (serializeInstruction (JumpIfTrue 10)) `shouldBe` [50, 0, 0, 0, 10]

    describe "Function Calls" $ do
        it "Call 55: opcode 0x40 + int32" $ do
            runBuilder (serializeInstruction (Call 55)) `shouldBe` [64, 0, 0, 0, 55]
        it "TailCall 60: opcode 0x41 + int32" $ do
            runBuilder (serializeInstruction (TailCall 60)) `shouldBe` [65, 0, 0, 0, 60]
        it "CallIndirect: opcode 0x42" $ do
            runBuilder (serializeInstruction CallIndirect) `shouldBe` [66]
        it "Ret: opcode 0x43" $ do
            runBuilder (serializeInstruction (Ret 0)) `shouldBe` [67, 0, 0, 0, 0]

    describe "Memory Operations (Locals, Globals, Captures)" $ do
        it "LoadLocal 1: opcode 0x50 + index" $ do
            runBuilder (serializeInstruction (LoadLocal 1)) `shouldBe` [80, 0, 0, 0, 1]
        it "StoreLocal 2: opcode 0x51 + index" $ do
            runBuilder (serializeInstruction (StoreLocal 2)) `shouldBe` [81, 0, 0, 0, 2]
        it "LoadGlobal 3: opcode 0x52 + index" $ do
            runBuilder (serializeInstruction (LoadGlobal 3)) `shouldBe` [82, 0, 0, 0, 3]
        it "StoreGlobal 4: opcode 0x53 + index" $ do
            runBuilder (serializeInstruction (StoreGlobal 4)) `shouldBe` [83, 0, 0, 0, 4]
        it "LoadCapture 5: opcode 0x54 + index" $ do
            runBuilder (serializeInstruction (LoadCapture 5)) `shouldBe` [84, 0, 0, 0, 5]
        it "StoreCapture 6: opcode 0x55 + index" $ do
            runBuilder (serializeInstruction (StoreCapture 6)) `shouldBe` [85, 0, 0, 0, 6]

    describe "Closures & Structures" $ do
        it "MakeClosure 7 2: opcode 0x60 + addr + count" $ do
            runBuilder (serializeInstruction (MakeClosure 7 2)) `shouldBe` [96, 0, 0, 0, 7, 0, 0, 0, 2]
        it "GetFuncAddr 8: opcode 0x61 + index" $ do
            runBuilder (serializeInstruction (GetFuncAddr 8)) `shouldBe` [97, 0, 0, 0, 8]
        it "BuildStruct 3: opcode 0x62 + count" $ do
            runBuilder (serializeInstruction (BuildStruct 3)) `shouldBe` [98, 0, 0, 0, 3]
        it "GetStructField 1: opcode 0x63 + index" $ do
            runBuilder (serializeInstruction (GetStructField 1)) `shouldBe` [99, 0, 0, 0, 1]

    describe "Type Casting" $ do
        it "Cast 5: opcode 0x80 + typeID" $ do
            runBuilder (serializeInstruction (Cast 5)) `shouldBe` [128, 5]

    describe "List Operations" $ do
        it "Cons: opcode 0x90" $ runBuilder (serializeInstruction Cons) `shouldBe` [144]
        it "Head: opcode 0x91" $ runBuilder (serializeInstruction Head) `shouldBe` [145]
        it "Tail: opcode 0x92" $ runBuilder (serializeInstruction Tail) `shouldBe` [146]

    describe "Misc & IO" $ do
        it "Print: opcode 0x70" $ do
            runBuilder (serializeInstruction Print) `shouldBe` [112]
        it "Halt: opcode 0x71" $ do
            runBuilder (serializeInstruction Halt) `shouldBe` [113]
        it "CheckStack 10: opcode 0xFE + int32" $ do
            runBuilder (serializeInstruction (CheckStack 10)) `shouldBe` [254, 0, 0, 0, 10]
        it "Nop: opcode 0xFF" $ do
            runBuilder (serializeInstruction Nop) `shouldBe` [255]
