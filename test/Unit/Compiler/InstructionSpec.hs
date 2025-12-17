{-# LANGUAGE LambdaCase #-}

module Compiler.InstructionSpec (spec) where

import Test.Hspec
import Data.Word (Word8)

import Compiler.Instruction
  ( Instruction(..)
  , Immediate(..)
  , immediateToTypeID
  , immediateSize
  , getInstCode
  )

import Common.Type.Integer (IntValue(..))

isTypeBool :: Word8 -> Bool
isTypeBool = \case
  0x00 -> True
  _    -> False

isTypeI8 :: Word8 -> Bool
isTypeI8 = \case
  0x01 -> True
  _    -> False

isOpcodePush :: Word8 -> Bool
isOpcodePush = \case
  0x01 -> True
  _    -> False

isOpcodeAdd :: Word8 -> Bool
isOpcodeAdd = \case
  0x10 -> True
  _    -> False

isOpcodeJump :: Word8 -> Bool
isOpcodeJump = \case
  0x30 -> True
  _    -> False

spec :: Spec
spec = describe "Compiler.Instruction (with meaningful alternatives)" $ do

  describe "immediateToTypeID" $ do
    it "ImmBool maps to bool TypeID (match branch)" $ do
      isTypeBool (immediateToTypeID (ImmBool True)) `shouldBe` True

    it "ImmInt is NOT bool TypeID (non-match branch)" $ do
      isTypeBool (immediateToTypeID (ImmInt (I8 1))) `shouldBe` False

    it "ImmInt I8 maps to 0x01 (match branch)" $ do
      isTypeI8 (immediateToTypeID (ImmInt (I8 1))) `shouldBe` True

    it "ImmBool is NOT I8 TypeID (non-match branch)" $ do
      isTypeI8 (immediateToTypeID (ImmBool False)) `shouldBe` False

  describe "immediateSize" $ do
    it "Bool payload is 1 byte" $
      immediateSize (ImmBool False) `shouldBe` 1

    it "Int sizes are correct" $ do
      immediateSize (ImmInt (I8 0))   `shouldBe` 1
      immediateSize (ImmInt (UI8 0))  `shouldBe` 1
      immediateSize (ImmInt (I16 0))  `shouldBe` 2
      immediateSize (ImmInt (UI16 0)) `shouldBe` 2
      immediateSize (ImmInt (I32 0))  `shouldBe` 4
      immediateSize (ImmInt (UI32 0)) `shouldBe` 4
      immediateSize (ImmInt (I64 0))  `shouldBe` 8
      immediateSize (ImmInt (UI64 0)) `shouldBe` 8

  describe "getInstCode" $ do
    it "Push opcode matches (match branch)" $ do
      isOpcodePush (getInstCode (Push (ImmBool True))) `shouldBe` True

    it "Add is NOT Push opcode (non-match branch)" $ do
      isOpcodePush (getInstCode Add) `shouldBe` False

    it "Add opcode matches (match branch)" $ do
      isOpcodeAdd (getInstCode Add) `shouldBe` True

    it "Sub is NOT Add opcode (non-match branch)" $ do
      isOpcodeAdd (getInstCode Sub) `shouldBe` False

    it "Jump opcode matches (match branch)" $ do
      isOpcodeJump (getInstCode (Jump 12)) `shouldBe` True

    it "JumpIfFalse is NOT Jump opcode (non-match branch)" $ do
      isOpcodeJump (getInstCode (JumpIfFalse 12)) `shouldBe` False

  describe "Existing exact opcode tests (still useful)" $ do
    it "Exact opcodes sanity (a few)" $ do
      getInstCode (Push (ImmBool True)) `shouldBe` 0x01
      getInstCode Add                   `shouldBe` 0x10
      getInstCode (Jump 0)              `shouldBe` 0x30
      getInstCode Halt                  `shouldBe` 0x71
