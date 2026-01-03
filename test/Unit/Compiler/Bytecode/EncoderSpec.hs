{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- EncoderSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.Bytecode.EncoderSpec (spec) where

import Test.Hspec

import Compiler.Bytecode.Encoder
  ( encodeInt32BE
  , encodeWord8
  , encodeBool
  , encodeString
  )

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)

runBuilder :: B.Builder -> [Word8]
runBuilder b = LBS.unpack (B.toLazyByteString b)

-- Predicats "case" (créent des alternatives HPC)
isEncodedInt32_42 :: [Word8] -> Bool
isEncodedInt32_42 = \case
  [0, 0, 0, 42] -> True
  _             -> False

isEncodedInt32_big :: [Word8] -> Bool
isEncodedInt32_big = \case
  [1, 2, 3, 4] -> True
  _            -> False

isEncodedInt32_neg1 :: [Word8] -> Bool
isEncodedInt32_neg1 = \case
  [255, 255, 255, 255] -> True
  _                    -> False

isEncodedWord8_128 :: [Word8] -> Bool
isEncodedWord8_128 = \case
  [128] -> True
  _     -> False

isEncodedWord8_255 :: [Word8] -> Bool
isEncodedWord8_255 = \case
  [255] -> True
  _     -> False

isEncodedBoolTrue :: [Word8] -> Bool
isEncodedBoolTrue = \case
  [1] -> True
  _   -> False

isEncodedBoolFalse :: [Word8] -> Bool
isEncodedBoolFalse = \case
  [0] -> True
  _   -> False

isEncodedStringHello :: [Word8] -> Bool
isEncodedStringHello = \case
  [0,0,0,5,104,101,108,108,111] -> True
  _                             -> False

isEncodedStringEmpty :: [Word8] -> Bool
isEncodedStringEmpty = \case
  [0,0,0,0] -> True
  _         -> False

isEncodedStringEAcute :: [Word8] -> Bool
isEncodedStringEAcute = \case
  -- "é" UTF-8 = C3 A9 => [195,169], length 2
  [0,0,0,2,195,169] -> True
  _                 -> False

spec :: Spec
spec = describe "BinaryEncoder unit tests (with real alternatives covered)" $ do

  describe "Encoder - Int32BE" $ do
    it "Encodes 42: predicate True (matches)" $ do
      isEncodedInt32_42 (runBuilder (encodeInt32BE 42)) `shouldBe` True

    it "Encodes 41: predicate False (non-match)" $ do
      isEncodedInt32_42 (runBuilder (encodeInt32BE 41)) `shouldBe` False

    it "Encodes 16909060: predicate True (matches)" $ do
      isEncodedInt32_big (runBuilder (encodeInt32BE 16909060)) `shouldBe` True

    it "Encodes 16909061: predicate False (non-match)" $ do
      isEncodedInt32_big (runBuilder (encodeInt32BE 16909061)) `shouldBe` False

    it "Encodes -1: predicate True (matches)" $ do
      isEncodedInt32_neg1 (runBuilder (encodeInt32BE (-1))) `shouldBe` True

    it "Encodes -2: predicate False (non-match)" $ do
      isEncodedInt32_neg1 (runBuilder (encodeInt32BE (-2))) `shouldBe` False

  describe "Encoder - Word8" $ do
    it "Encodes 128: predicate True (matches)" $ do
      isEncodedWord8_128 (runBuilder (encodeWord8 128)) `shouldBe` True

    it "Encodes 127: predicate False (non-match)" $ do
      isEncodedWord8_128 (runBuilder (encodeWord8 127)) `shouldBe` False

    it "Encodes 255: predicate True (matches)" $ do
      isEncodedWord8_255 (runBuilder (encodeWord8 255)) `shouldBe` True

    it "Encodes 254: predicate False (non-match)" $ do
      isEncodedWord8_255 (runBuilder (encodeWord8 254)) `shouldBe` False

  describe "Encoder - Bool" $ do
    it "Encodes True: predicate True (matches)" $ do
      isEncodedBoolTrue (runBuilder (encodeBool True)) `shouldBe` True

    it "Encodes False: predicate False (non-match)" $ do
      isEncodedBoolTrue (runBuilder (encodeBool False)) `shouldBe` False

    it "Encodes False: predicate True (matches)" $ do
      isEncodedBoolFalse (runBuilder (encodeBool False)) `shouldBe` True

    it "Encodes True: predicate False (non-match)" $ do
      isEncodedBoolFalse (runBuilder (encodeBool True)) `shouldBe` False

  describe "Encoder - String" $ do
    it "Encodes \"hello\": predicate True (matches)" $ do
      isEncodedStringHello (runBuilder (encodeString "hello")) `shouldBe` True

    it "Encodes \"hellO\": predicate False (non-match)" $ do
      isEncodedStringHello (runBuilder (encodeString "hellO")) `shouldBe` False

    it "Encodes empty string: predicate True (matches)" $ do
      isEncodedStringEmpty (runBuilder (encodeString "")) `shouldBe` True

    it "Encodes \"a\": predicate False (non-match)" $ do
      isEncodedStringEmpty (runBuilder (encodeString "a")) `shouldBe` False

    it "Encodes \"é\": predicate True (matches)" $ do
      isEncodedStringEAcute (runBuilder (encodeString "é")) `shouldBe` True

    it "Encodes \"e\": predicate False (non-match)" $ do
      isEncodedStringEAcute (runBuilder (encodeString "e")) `shouldBe` False
