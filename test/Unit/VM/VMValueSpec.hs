{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM.VMValue unit tests
-}

{-# LANGUAGE OverloadedStrings #-}

module VM.VMValueSpec (spec) where

import Test.Hspec
import Data.Word (Word8)
import qualified Data.Vector as V
import qualified Data.Text as T

import VM.VMValue
  ( VMValue(..)
  , castValue
  , getValueName
  , eqValue
  , valueToInt
  )
import Common.Type.Integer (IntValue(..))

i8 :: Int -> VMValue
i8 n = VInt (I8 (fromIntegral n))

i16 :: Int -> VMValue
i16 n = VInt (I16 (fromIntegral n))

i32 :: Int -> VMValue
i32 n = VInt (I32 (fromIntegral n))

i64 :: Int -> VMValue
i64 n = VInt (I64 (fromIntegral n))

u8 :: Int -> VMValue
u8 n = VInt (UI8 (fromIntegral n))

u16 :: Int -> VMValue
u16 n = VInt (UI16 (fromIntegral n))

u32 :: Int -> VMValue
u32 n = VInt (UI32 (fromIntegral n))

u64 :: Int -> VMValue
u64 n = VInt (UI64 (fromIntegral n))

ichar :: Char -> VMValue
ichar c = VInt (IChar (fromIntegral (fromEnum c)))

uichar :: Char -> VMValue
uichar c = VInt (UIChar (fromIntegral (fromEnum c)))

spec :: Spec
spec = describe "VM.VMValue" $ do

  describe "castValue" $ do
    it "0x00 -> bool (non-zero => True, zero => False)" $ do
      castValue 0x00 (i32 0) `shouldBe` VBool False
      castValue 0x00 (i32 42) `shouldBe` VBool True
      castValue 0x00 (VBool True) `shouldBe` VBool True
      castValue 0x00 (VBool False) `shouldBe` VBool False

    it "0x01 -> I8" $ do
      castValue 0x01 (i32 12) `shouldBe` VInt (I8 12)

    it "0x02 -> UI8" $ do
      castValue 0x02 (i32 12) `shouldBe` VInt (UI8 12)

    it "0x03 -> I16" $ do
      castValue 0x03 (i32 12) `shouldBe` VInt (I16 12)

    it "0x04 -> UI16" $ do
      castValue 0x04 (i32 12) `shouldBe` VInt (UI16 12)

    it "0x05 -> I32" $ do
      castValue 0x05 (i8 12) `shouldBe` VInt (I32 12)

    it "0x06 -> UI32" $ do
      castValue 0x06 (i32 12) `shouldBe` VInt (UI32 12)

    it "0x07 -> I64" $ do
      castValue 0x07 (i32 12) `shouldBe` VInt (I64 12)

    it "0x08 -> UI64" $ do
      castValue 0x08 (i32 12) `shouldBe` VInt (UI64 12)

    it "0x09 -> IChar" $ do
      castValue 0x09 (i32 65) `shouldBe` VInt (IChar 65)

    it "0x10 -> UIChar" $ do
      castValue 0x10 (i32 65) `shouldBe` VInt (UIChar 65)

    it "default -> returns the original value unchanged" $ do
      let v = VStruct (V.fromList [i32 1])
      castValue 0xFF v `shouldBe` v

  describe "getValueName" $ do
    it "VVoid -> \"void\"" $ do
      getValueName VVoid `shouldBe` "void"

    it "VBool -> \"bool\"" $ do
      getValueName (VBool True) `shouldBe` "bool"
      getValueName (VBool False) `shouldBe` "bool"

    it "VClosure -> \"function\"" $ do
      getValueName (VClosure 10 V.empty) `shouldBe` "function"

    it "VFuncPtr -> \"function\"" $ do
      getValueName (VFuncPtr 42) `shouldBe` "function"

    it "IChar / UIChar -> \"char\"" $ do
      getValueName (VInt (IChar 65)) `shouldBe` "char"
      getValueName (VInt (UIChar 65)) `shouldBe` "char"

    it "other ints -> \"int\"" $ do
      getValueName (VInt (I32 10)) `shouldBe` "int"
      getValueName (VInt (UI64 10)) `shouldBe` "int"

    it "VStruct -> \"[struct]\"" $ do
      getValueName (VStruct (V.fromList [i32 1])) `shouldBe` "[struct]"

    it "VList empty -> \"[void]\" (guard branch)" $ do
      getValueName (VList V.empty) `shouldBe` "[void]"

    it "VList non-empty -> \"[<elemType>]\" (guard branch)" $ do
      getValueName (VList (V.fromList [i32 1, i32 2])) `shouldBe` "[int]"
      getValueName (VList (V.fromList [VBool True])) `shouldBe` "[bool]"

    it "nested list builds recursive type, e.g. [[char]]" $ do
      let inner = VList (V.fromList [VInt (IChar 65)])
      let outer = VList (V.fromList [inner])
      getValueName outer `shouldBe` "[[char]]"

  describe "eqValue (covers checkVectorEq branches)" $ do
    it "returns False when list lengths differ (length check branch)" $ do
      eqValue (VList (V.fromList [i32 1])) (VList (V.fromList [i32 1, i32 2])) `shouldBe` False

    it "returns True for same-length lists when elements are equal with loose int equality" $ do
      eqValue
        (VList (V.fromList [VInt (I8 5)]))
        (VList (V.fromList [VInt (I64 5)]))
        `shouldBe` True

    it "returns False when a zipped element differs (zipWith/and false branch)" $ do
      eqValue
        (VStruct (V.fromList [i32 1, VBool True]))
        (VStruct (V.fromList [i32 1, VBool False]))
        `shouldBe` False
