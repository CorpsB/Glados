{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- SerializerSpec
-}

module Compiler.SerializerSpec (spec) where

import Test.Hspec
import Data.Word()
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS

import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.Serializer (serializeInstruction)
import Common.Type.Integer (IntValue(..))

runBuilder :: B.Builder -> [Int]
runBuilder b = map fromEnum $ LBS.unpack (B.toLazyByteString b)

spec :: Spec
spec = describe "serializeInstruction (Instruction)" $ do
    it "Push (ImmInt (I32 5)): opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Push (ImmInt (I32 5))))
        bytes `shouldBe` [1,0,0,0,5]
    it "Push (ImmBool True): opcode + bool" $ do
        let bytes = runBuilder (serializeInstruction (Push (ImmBool True)))
        bytes `shouldBe` [2,1]
    it "Push (ImmBool False): opcode + bool" $ do
        let bytes = runBuilder (serializeInstruction (Push (ImmBool False)))
        bytes `shouldBe` [2,0]
    it "Pop: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Pop)
        bytes `shouldBe` [3]
    it "Dup: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Dup)
        bytes `shouldBe` [4]
    it "Swap: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Swap)
        bytes `shouldBe` [5]
    it "Add: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Add)
        bytes `shouldBe` [6]
    it "Sub: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Sub)
        bytes `shouldBe` [7]
    it "Jump 42: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Jump 42))
        bytes `shouldBe` [8,0,0,0,42]
    it "JumpIfFalse 99: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (JumpIfFalse 99))
        bytes `shouldBe` [9,0,0,0,99]
    it "Call 55: opcode + int32" $ do
        let bytes = runBuilder (serializeInstruction (Call 55))
        bytes `shouldBe` [10,0,0,0,55]
    it "Ret: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Ret)
        bytes `shouldBe` [11]
    it "MakeClosure 7 2: opcode + int32 + int32" $ do
        let bytes = runBuilder (serializeInstruction (MakeClosure 7 2))
        bytes `shouldBe` [12,0,0,0,7,0,0,0,2]
    it "Halt: opcode only" $ do
        let bytes = runBuilder (serializeInstruction Halt)
        bytes `shouldBe` [255]
