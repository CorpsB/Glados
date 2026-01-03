{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- InstructionSpec
-}

module Compiler.InstructionSpec (spec) where

import Test.Hspec
import Data.Word (Word8)
import Data.List (sort, isPrefixOf)

import Compiler.Instruction
    ( Instruction(..)
    , Immediate(..)
    , immediateToTypeID
    , immediateSize
    , getInstCode
    )

import Common.Type.Integer (IntValue(..))

shouldOpcode :: Instruction -> Word8 -> Expectation
shouldOpcode inst expected = getInstCode inst `shouldBe` expected

allImmediates :: [Immediate]
allImmediates =
    [ ImmBool True
    , ImmBool False
    , ImmInt (I8 0)
    , ImmInt (UI8 0)
    , ImmInt (I16 0)
    , ImmInt (UI16 0)
    , ImmInt (I32 0)
    , ImmInt (UI32 0)
    , ImmInt (I64 0)
    , ImmInt (UI64 0)
    ]

allInstructions :: [Instruction]
allInstructions =
    [ Push (ImmBool True)
    , Push (ImmInt (I8 42))
    , Pop, Dup, Swap
    , Add, Sub, Mul, Div, Mod
    , Eq, Lt, Le, Not, And, Or
    , Jump 0, JumpIfFalse 0, JumpIfTrue 0
    , Call 0, TailCall 0, CallIndirect, Ret
    , LoadLocal 0, StoreLocal 0
    , LoadGlobal 0, StoreGlobal 0
    , LoadCapture 0, StoreCapture 0
    , MakeClosure 0 0, GetFuncAddr 0
    , Cast 0x00
    , Print, Halt
    , CheckStack 0
    , Nop
    ]

instrCtorName :: Instruction -> String
instrCtorName inst = case inst of
    Push _        -> "Push"
    Pop           -> "Pop"
    Dup           -> "Dup"
    Swap          -> "Swap"
    Add           -> "Add"
    Sub           -> "Sub"
    Mul           -> "Mul"
    Div           -> "Div"
    Mod           -> "Mod"
    Eq            -> "Eq"
    Lt            -> "Lt"
    Le            -> "Le"
    Not           -> "Not"
    And           -> "And"
    Or            -> "Or"
    Jump _        -> "Jump"
    JumpIfFalse _ -> "JumpIfFalse"
    JumpIfTrue _  -> "JumpIfTrue"
    Call _        -> "Call"
    TailCall _    -> "TailCall"
    CallIndirect  -> "CallIndirect"
    Ret           -> "Ret"
    LoadLocal _   -> "LoadLocal"
    StoreLocal _  -> "StoreLocal"
    LoadGlobal _  -> "LoadGlobal"
    StoreGlobal _ -> "StoreGlobal"
    LoadCapture _ -> "LoadCapture"
    StoreCapture _-> "StoreCapture"
    MakeClosure _ _ -> "MakeClosure"
    GetFuncAddr _ -> "GetFuncAddr"
    Cast _        -> "Cast"
    Print         -> "Print"
    Halt          -> "Halt"
    CheckStack _  -> "CheckStack"
    Nop           -> "Nop"

immCtorName :: Immediate -> String
immCtorName im = case im of
    ImmBool _ -> "ImmBool"
    ImmInt _  -> "ImmInt"

spec :: Spec
spec = describe "Compiler.Instruction (coverage + exact spec mapping)" $ do

    describe "immediateToTypeID (ASM_SPEC)" $ do
        it "maps all Immediate variants to the correct TypeID byte" $ do
            immediateToTypeID (ImmBool True)  `shouldBe` 0x00
            immediateToTypeID (ImmBool False) `shouldBe` 0x00
            immediateToTypeID (ImmInt (I8  1))   `shouldBe` 0x01
            immediateToTypeID (ImmInt (UI8 1))   `shouldBe` 0x02
            immediateToTypeID (ImmInt (I16 1))   `shouldBe` 0x03
            immediateToTypeID (ImmInt (UI16 1))  `shouldBe` 0x04
            immediateToTypeID (ImmInt (I32 1))   `shouldBe` 0x05
            immediateToTypeID (ImmInt (UI32 1))  `shouldBe` 0x06
            immediateToTypeID (ImmInt (I64 1))   `shouldBe` 0x07
            immediateToTypeID (ImmInt (UI64 1))  `shouldBe` 0x08

    describe "immediateSize" $ do
        it "returns the right payload size for every Immediate" $ do
            immediateSize (ImmBool True)  `shouldBe` 1
            immediateSize (ImmBool False) `shouldBe` 1
            immediateSize (ImmInt (I8  0))   `shouldBe` 1
            immediateSize (ImmInt (UI8 0))   `shouldBe` 1
            immediateSize (ImmInt (I16 0))   `shouldBe` 2
            immediateSize (ImmInt (UI16 0))  `shouldBe` 2
            immediateSize (ImmInt (I32 0))   `shouldBe` 4
            immediateSize (ImmInt (UI32 0))  `shouldBe` 4
            immediateSize (ImmInt (I64 0))   `shouldBe` 8
            immediateSize (ImmInt (UI64 0))  `shouldBe` 8

    describe "getInstCode (EXHAUSTIVE exact opcodes)" $ do
        it "maps every instruction constructor to the opcode byte defined by ASM_SPEC" $ do
            shouldOpcode (Push (ImmBool True)) 0x01
            shouldOpcode Pop                  0x02
            shouldOpcode Dup                  0x03
            shouldOpcode Swap                 0x04
            shouldOpcode Add 0x10
            shouldOpcode Sub 0x11
            shouldOpcode Mul 0x12
            shouldOpcode Div 0x13
            shouldOpcode Mod 0x14
            shouldOpcode Eq  0x20
            shouldOpcode Lt  0x21
            shouldOpcode Not 0x22
            shouldOpcode And 0x23
            shouldOpcode Or  0x24
            shouldOpcode Le  0x25
            shouldOpcode (Jump 0)          0x30
            shouldOpcode (Jump 999)        0x30
            shouldOpcode (JumpIfFalse 7)   0x31
            shouldOpcode (JumpIfFalse 123) 0x31
            shouldOpcode (JumpIfTrue 9)    0x32
            shouldOpcode (JumpIfTrue 456)  0x32
            shouldOpcode (Call 0)        0x40
            shouldOpcode (Call 999)      0x40
            shouldOpcode (TailCall 0)    0x41
            shouldOpcode (TailCall 555)  0x41
            shouldOpcode CallIndirect    0x42
            shouldOpcode Ret             0x43
            shouldOpcode (LoadLocal 0)    0x50
            shouldOpcode (StoreLocal 0)   0x51
            shouldOpcode (LoadGlobal 0)   0x52
            shouldOpcode (StoreGlobal 0)  0x53
            shouldOpcode (LoadCapture 0)  0x54
            shouldOpcode (StoreCapture 0) 0x55
            shouldOpcode (MakeClosure 0 0) 0x60
            shouldOpcode (GetFuncAddr 0)   0x61
            shouldOpcode (Cast 0x00)       0x80
            shouldOpcode (Cast 0x05)       0x80
            shouldOpcode Print           0x70
            shouldOpcode Halt            0x71
            shouldOpcode (CheckStack 0)  0xFE
            shouldOpcode (CheckStack 3)  0xFE
            shouldOpcode Nop             0xFF

    describe "Derived instances (Eq / Ord / Show) - cover ALL ctors" $ do
        it "Eq: every constructor equals itself (and Immediate too) and distinct ctors compare /=" $ do
            mapM_ (\x -> x `shouldBe` x) allInstructions
            mapM_ (\i -> i `shouldBe` i) allImmediates
            and (zipWith (/=) allInstructions (drop 1 allInstructions)) `shouldBe` True
            and (zipWith (/=) allImmediates (drop 1 allImmediates)) `shouldBe` True
        it "Ord: compare x x = EQ for all constructors + sort produces non-decreasing order" $ do
            mapM_ (\x -> compare x x `shouldBe` EQ) allInstructions
            mapM_ (\i -> compare i i `shouldBe` EQ) allImmediates
            let xs = sort allInstructions
            and (zipWith (<=) xs (drop 1 xs)) `shouldBe` True
            let ys = sort allImmediates
            and (zipWith (<=) ys (drop 1 ys)) `shouldBe` True
        it "Show: non-empty and starts with the constructor name for every instruction and immediate" $ do
            mapM_ (\x -> show x `shouldSatisfy` (not . null)) allInstructions
            mapM_ (\x -> (instrCtorName x `isPrefixOf` show x) `shouldBe` True) allInstructions
            mapM_ (\i -> show i `shouldSatisfy` (not . null)) allImmediates
            mapM_ (\i -> (immCtorName i `isPrefixOf` show i) `shouldBe` True) allImmediates
{-# LANGUAGE OverloadedStrings #-}
module Compiler.InstructionSpec (spec) where

import Test.Hspec
import Compiler.Instruction
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS

runBuilder :: B.Builder -> [Int]
runBuilder b = map fromEnum $ LBS.unpack (B.toLazyByteString b)

spec :: Spec
spec = describe "Compiler.Instruction" $ do
    it "Push 5: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Push 5))
        bytes `shouldBe` [1,0,0,0,5]
        length bytes `shouldBe` instructionSize (Push 5)
    it "PushBool True: opcode + bool" $ do
        let bytes = runBuilder (serializeInstruction (PushBool True))
        bytes `shouldBe` [2,1]
        length bytes `shouldBe` instructionSize (PushBool True)
    it "PushBool False: opcode + bool" $ do
        let bytes = runBuilder (serializeInstruction (PushBool False))
        bytes `shouldBe` [2,0]
        length bytes `shouldBe` instructionSize (PushBool False)
    it "Add: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Add)
        bytes `shouldBe` [3]
        length bytes `shouldBe` instructionSize Add
    it "Sub: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Sub)
        bytes `shouldBe` [4]
        length bytes `shouldBe` instructionSize Sub
    it "Jump 42: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Jump 42))
        bytes `shouldBe` [5,0,0,0,42]
        length bytes `shouldBe` instructionSize (Jump 42)
    it "JumpIfFalse 99: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (JumpIfFalse 99))
        bytes `shouldBe` [6,0,0,0,99]
        length bytes `shouldBe` instructionSize (JumpIfFalse 99)
    it "Load 123: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Load 123))
        bytes `shouldBe` [7,0,0,0,123]
        length bytes `shouldBe` instructionSize (Load 123)
    it "Store 77: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Store 77))
        bytes `shouldBe` [8,0,0,0,77]
        length bytes `shouldBe` instructionSize (Store 77)
    it "Call 55: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Call 55))
        bytes `shouldBe` [9,0,0,0,55]
        length bytes `shouldBe` instructionSize (Call 55)
    it "Ret: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Ret)
        bytes `shouldBe` [10]
        length bytes `shouldBe` instructionSize Ret
    it "MakeClosure 7 2: opcode + int32 + int32" $ do
        let bytes = runBuilder (serializeInstruction (MakeClosure 7 2))
        bytes `shouldBe` [11,0,0,0,7,0,0,0,2]
        length bytes `shouldBe` instructionSize (MakeClosure 7 2)
    it "Halt: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Halt)
        bytes `shouldBe` [255]
        length bytes `shouldBe` instructionSize Halt
