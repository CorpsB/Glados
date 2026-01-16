{-# LANGUAGE OverloadedStrings #-}

module Compiler.InstructionSpec (spec) where

import Test.Hspec
import Data.List (sort, isInfixOf, isPrefixOf)
import Data.Word()

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Compiler.Bytecode.Serializer (serializeInstruction)
import Compiler.Instruction
import Common.Type.Integer (IntValue(..))

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
    , ImmInt (IChar 0)
    , ImmInt (UIChar 0)
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
    , BuildStruct 0, GetStructField 0
    , Cast 0x00
    , Cons, Head, Tail
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
    BuildStruct _ -> "BuildStruct"
    GetStructField _ -> "GetStructField"
    Cast _        -> "Cast"
    Cons          -> "Cons"
    Head          -> "Head"
    Tail          -> "Tail"
    Print         -> "Print"
    Halt          -> "Halt"
    CheckStack _  -> "CheckStack"
    Nop           -> "Nop"
    Exit          -> "Exit"
    Nth           -> "Nth"
    BuildList _   -> "BuildList"

immCtorName :: Immediate -> String
immCtorName im = case im of
    ImmBool _ -> "ImmBool"
    ImmInt _  -> "ImmInt"

spec :: Spec
spec = describe "Compiler.Instruction" $ do

    describe "immediateToTypeID" $ do
        it "maps all Immediate payloads to the expected TypeID" $ do
            immediateToTypeID (ImmBool True) `shouldBe` 0x00

            immediateToTypeID (ImmInt (I8 1)) `shouldBe` 0x01
            immediateToTypeID (ImmInt (UI8 1)) `shouldBe` 0x02

            immediateToTypeID (ImmInt (I16 1)) `shouldBe` 0x03
            immediateToTypeID (ImmInt (UI16 1)) `shouldBe` 0x04

            immediateToTypeID (ImmInt (I32 1)) `shouldBe` 0x05
            immediateToTypeID (ImmInt (UI32 1)) `shouldBe` 0x06

            immediateToTypeID (ImmInt (I64 1)) `shouldBe` 0x07
            immediateToTypeID (ImmInt (UI64 1)) `shouldBe` 0x08

            immediateToTypeID (ImmInt (IChar 65)) `shouldBe` 0x09
            immediateToTypeID (ImmInt (UIChar 65)) `shouldBe` 0x10

    describe "immediateSize" $ do
        it "returns the expected payload size for all Immediate variants" $ do
            immediateSize (ImmBool False) `shouldBe` 1

            immediateSize (ImmInt (I8 1)) `shouldBe` 1
            immediateSize (ImmInt (UI8 1)) `shouldBe` 1
            immediateSize (ImmInt (IChar 1)) `shouldBe` 1
            immediateSize (ImmInt (UIChar 1)) `shouldBe` 1

            immediateSize (ImmInt (I16 1)) `shouldBe` 2
            immediateSize (ImmInt (UI16 1)) `shouldBe` 2

            immediateSize (ImmInt (I32 1)) `shouldBe` 4
            immediateSize (ImmInt (UI32 1)) `shouldBe` 4

            immediateSize (ImmInt (I64 1)) `shouldBe` 8
            immediateSize (ImmInt (UI64 1)) `shouldBe` 8

    describe "getInstCode" $ do
        it "maps each instruction to its opcode" $ do
            getInstCode (Push (ImmBool True)) `shouldBe` 0x01
            getInstCode Pop `shouldBe` 0x02
            getInstCode Dup `shouldBe` 0x03
            getInstCode Swap `shouldBe` 0x04

            getInstCode Add `shouldBe` 0x10
            getInstCode Sub `shouldBe` 0x11
            getInstCode Mul `shouldBe` 0x12
            getInstCode Div `shouldBe` 0x13
            getInstCode Mod `shouldBe` 0x14

            getInstCode Eq `shouldBe` 0x20
            getInstCode Lt `shouldBe` 0x21
            getInstCode Not `shouldBe` 0x22
            getInstCode And `shouldBe` 0x23
            getInstCode Or `shouldBe` 0x24
            getInstCode Le `shouldBe` 0x25

            getInstCode (Jump 0) `shouldBe` 0x30
            getInstCode (JumpIfFalse 0) `shouldBe` 0x31
            getInstCode (JumpIfTrue 0) `shouldBe` 0x32

            getInstCode (Call 0) `shouldBe` 0x40
            getInstCode (TailCall 0) `shouldBe` 0x41
            getInstCode CallIndirect `shouldBe` 0x42
            getInstCode Ret `shouldBe` 0x43

            getInstCode (LoadLocal 0) `shouldBe` 0x50
            getInstCode (StoreLocal 0) `shouldBe` 0x51
            getInstCode (LoadGlobal 0) `shouldBe` 0x52
            getInstCode (StoreGlobal 0) `shouldBe` 0x53
            getInstCode (LoadCapture 0) `shouldBe` 0x54
            getInstCode (StoreCapture 0) `shouldBe` 0x55

            getInstCode (MakeClosure 1 2) `shouldBe` 0x60
            getInstCode (GetFuncAddr 0) `shouldBe` 0x61
            getInstCode (BuildStruct 0) `shouldBe` 0x62
            getInstCode (GetStructField 0) `shouldBe` 0x63
            getInstCode (Cast 0xFF) `shouldBe` 0x80

            getInstCode Cons `shouldBe` 0x90
            getInstCode Head `shouldBe` 0x91
            getInstCode Tail `shouldBe` 0x92

            getInstCode Print `shouldBe` 0x70
            getInstCode Halt `shouldBe` 0x71
            getInstCode (CheckStack 3) `shouldBe` 0xFE
            getInstCode Nop `shouldBe` 0xFF

    describe "Derived instances (Show / Ord) coverage" $ do
        it "uses Show and Ord on Immediate" $ do
            show (ImmBool True) `shouldBe` "ImmBool True"
            show (ImmInt (I8 1)) `shouldBe` "ImmInt (I8 1)"

            (ImmBool False < ImmInt (I8 0)) `shouldBe` True
            compare (ImmInt (I8 1)) (ImmInt (I8 2)) `shouldBe` LT

            sort [ImmInt (I8 2), ImmBool True, ImmInt (I8 1)]
                `shouldBe` [ImmBool True, ImmInt (I8 1), ImmInt (I8 2)]

        it "uses Show and Ord on Instruction" $ do
            show Add `shouldBe` "Add"
            show (Push (ImmInt (I16 2))) `shouldSatisfy` ("Push" `isInfixOf`)

            (Push (ImmBool True) < Pop) `shouldBe` True
            (Le < Or) `shouldBe` True

            sort [Nop, Add, Pop] `shouldBe` [Pop, Add, Nop]

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

    describe "instructionSize (Instruction)" $ do
        describe "Stack & Arithmetic & Logic" $ do
            it "Push (ImmInt (I32 5))" $ length (runBuilder (serializeInstruction (Push (ImmInt (I32 5))))) `shouldBe` instructionSize (Push (ImmInt (I32 5)))
            it "Pop" $ length (runBuilder (serializeInstruction Pop)) `shouldBe` instructionSize Pop
            it "Dup" $ length (runBuilder (serializeInstruction Dup)) `shouldBe` instructionSize Dup
            it "Swap" $ length (runBuilder (serializeInstruction Swap)) `shouldBe` instructionSize Swap
            it "Add" $ length (runBuilder (serializeInstruction Add)) `shouldBe` instructionSize Add
            it "Sub" $ length (runBuilder (serializeInstruction Sub)) `shouldBe` instructionSize Sub
            it "Mul" $ length (runBuilder (serializeInstruction Mul)) `shouldBe` instructionSize Mul
            it "Div" $ length (runBuilder (serializeInstruction Div)) `shouldBe` instructionSize Div
            it "Mod" $ length (runBuilder (serializeInstruction Mod)) `shouldBe` instructionSize Mod
            it "Eq" $ length (runBuilder (serializeInstruction Eq)) `shouldBe` instructionSize Eq
            it "Lt" $ length (runBuilder (serializeInstruction Lt)) `shouldBe` instructionSize Lt
            it "Le" $ length (runBuilder (serializeInstruction Le)) `shouldBe` instructionSize Le
            it "Not" $ length (runBuilder (serializeInstruction Not)) `shouldBe` instructionSize Not
            it "And" $ length (runBuilder (serializeInstruction And)) `shouldBe` instructionSize And
            it "Or" $ length (runBuilder (serializeInstruction Or)) `shouldBe` instructionSize Or

        describe "Control Flow" $ do
            it "Jump 42" $ length (runBuilder (serializeInstruction (Jump 42))) `shouldBe` instructionSize (Jump 42)
            it "JumpIfFalse 99" $ length (runBuilder (serializeInstruction (JumpIfFalse 99))) `shouldBe` instructionSize (JumpIfFalse 99)
            it "JumpIfTrue 10" $ length (runBuilder (serializeInstruction (JumpIfTrue 10))) `shouldBe` instructionSize (JumpIfTrue 10)

        describe "Functions" $ do
            it "Call 55" $ length (runBuilder (serializeInstruction (Call 55))) `shouldBe` instructionSize (Call 55)
            it "TailCall 60" $ length (runBuilder (serializeInstruction (TailCall 60))) `shouldBe` instructionSize (TailCall 60)
            it "CallIndirect" $ length (runBuilder (serializeInstruction CallIndirect)) `shouldBe` instructionSize CallIndirect
            it "Ret" $ length (runBuilder (serializeInstruction Ret)) `shouldBe` instructionSize Ret

        describe "Memory" $ do
            it "LoadLocal 1" $ length (runBuilder (serializeInstruction (LoadLocal 1))) `shouldBe` instructionSize (LoadLocal 1)
            it "StoreLocal 2" $ length (runBuilder (serializeInstruction (StoreLocal 2))) `shouldBe` instructionSize (StoreLocal 2)
            it "LoadGlobal 3" $ length (runBuilder (serializeInstruction (LoadGlobal 3))) `shouldBe` instructionSize (LoadGlobal 3)
            it "StoreGlobal 4" $ length (runBuilder (serializeInstruction (StoreGlobal 4))) `shouldBe` instructionSize (StoreGlobal 4)
            it "LoadCapture 5" $ length (runBuilder (serializeInstruction (LoadCapture 5))) `shouldBe` instructionSize (LoadCapture 5)
            it "StoreCapture 6" $ length (runBuilder (serializeInstruction (StoreCapture 6))) `shouldBe` instructionSize (StoreCapture 6)

        describe "Closures & Structs" $ do
            it "MakeClosure 7 2" $ length (runBuilder (serializeInstruction (MakeClosure 7 2))) `shouldBe` instructionSize (MakeClosure 7 2)
            it "GetFuncAddr 8" $ length (runBuilder (serializeInstruction (GetFuncAddr 8))) `shouldBe` instructionSize (GetFuncAddr 8)
            it "BuildStruct 5" $ length (runBuilder (serializeInstruction (BuildStruct 5))) `shouldBe` instructionSize (BuildStruct 5)
            it "GetStructField 2" $ length (runBuilder (serializeInstruction (GetStructField 2))) `shouldBe` instructionSize (GetStructField 2)
            it "Cast 0x01" $ length (runBuilder (serializeInstruction (Cast 0x01))) `shouldBe` instructionSize (Cast 0x01)

        describe "Lists" $ do
            it "Cons" $ length (runBuilder (serializeInstruction Cons)) `shouldBe` instructionSize Cons
            it "Head" $ length (runBuilder (serializeInstruction Head)) `shouldBe` instructionSize Head
            it "Tail" $ length (runBuilder (serializeInstruction Tail)) `shouldBe` instructionSize Tail

        describe "System" $ do
            it "Print" $ length (runBuilder (serializeInstruction Print)) `shouldBe` instructionSize Print
            it "Halt" $ length (runBuilder (serializeInstruction Halt)) `shouldBe` instructionSize Halt
            it "CheckStack 10" $ length (runBuilder (serializeInstruction (CheckStack 10))) `shouldBe` instructionSize (CheckStack 10)
            it "Nop" $ length (runBuilder (serializeInstruction Nop)) `shouldBe` instructionSize Nop
    
    describe "Deep Coverage for Derived Instances" $ do

        describe "Immediate Show & Ord" $ do
            it "forces showsPrec execution" $ do
                show (Just (ImmBool True)) `shouldSatisfy` ("Just (ImmBool True)" `isInfixOf`)
                show [ImmInt (I8 5)] `shouldSatisfy` ("ImmInt (I8 5)" `isInfixOf`)

            it "forces full Ord evaluation" $ do
                (ImmBool False < ImmBool True) `shouldBe` True
                (ImmBool True > ImmBool False) `shouldBe` True
                (ImmBool True >= ImmBool True) `shouldBe` True
                (ImmInt (I8 1) <= ImmInt (I8 1)) `shouldBe` True
                max (ImmBool False) (ImmBool True) `shouldBe` ImmBool True
                min (ImmBool False) (ImmBool True) `shouldBe` ImmBool False

        describe "Instruction Show & Ord" $ do
            it "forces showsPrec execution" $ do
                show (Just Add) `shouldSatisfy` ("Just Add" `isInfixOf`)
                show (Just (Push (ImmBool True))) `shouldSatisfy` ("Just (Push (ImmBool True))" `isInfixOf`)
                show [Pop, Dup] `shouldSatisfy` ("Pop" `isInfixOf`)

            it "forces full Ord evaluation" $ do
                (Add < Sub) `shouldBe` True
                (Sub > Add) `shouldBe` True
                (Add <= Add) `shouldBe` True
                (Add >= Add) `shouldBe` True
                compare Add Sub `shouldBe` LT
                compare Sub Add `shouldBe` GT
                compare Add Add `shouldBe` EQ
                max Add Sub `shouldBe` Sub
                min Add Sub `shouldBe` Add

runBuilder :: B.Builder -> [Int]
runBuilder b = map fromEnum $ LBS.unpack (B.toLazyByteString b)
