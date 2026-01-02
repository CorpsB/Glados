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
