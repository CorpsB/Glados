{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- InstructionSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.InstructionSpec (spec) where

import Test.Hspec
import Data.List (sort, isInfixOf)

import Compiler.Instruction
import Common.Type.Integer (IntValue(..))

spec :: Spec
spec = describe "Compiler.Instruction" $ do

  describe "immediateToTypeID" $ do
    it "maps all Immediate payloads to the expected TypeID" $ do
      immediateToTypeID (ImmBool True) `shouldBe` 0x00

      immediateToTypeID (ImmInt (I8 1)) `shouldBe` 0x01
      immediateToTypeID (ImmInt (UI8 1)) `shouldBe` 0x02

      immediateToTypeID (ImmInt (IChar 65)) `shouldBe` 0x09
      immediateToTypeID (ImmInt (UIChar 65)) `shouldBe` 0x10

      immediateToTypeID (ImmInt (I16 1)) `shouldBe` 0x03
      immediateToTypeID (ImmInt (UI16 1)) `shouldBe` 0x04

      immediateToTypeID (ImmInt (I32 1)) `shouldBe` 0x05
      immediateToTypeID (ImmInt (UI32 1)) `shouldBe` 0x06

      immediateToTypeID (ImmInt (I64 1)) `shouldBe` 0x07
      immediateToTypeID (ImmInt (UI64 1)) `shouldBe` 0x08

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
      getInstCode (Cast 0xFF) `shouldBe` 0x80

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
