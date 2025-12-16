{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- EncoderSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.Bytecode.EncoderSpec (spec) where

import Test.Hspec
import Compiler.Bytecode.Encoder (encodeInt32BE, encodeWord8, encodeBool, encodeString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)

runBuilder :: B.Builder -> [Word8]
runBuilder b = LBS.unpack (B.toLazyByteString b)

spec :: Spec
spec = describe "BinaryEncoder unit tests" $ do

    describe "Encoder - Int32BE" $ do
        it "Encodes positive integer (42)" $ do
            encodeInt32BE 42 `shouldSatisfy` \b -> case runBuilder b of
                [0, 0, 0, 42] -> True
                _ -> False
        it "Encodes large integer (16909060)" $ do
            encodeInt32BE 16909060 `shouldSatisfy` \b -> case runBuilder b of
                [1, 2, 3, 4] -> True
                _ -> False
        it "Encodes negative integer (-1)" $ do
            encodeInt32BE (-1) `shouldSatisfy` \b -> case runBuilder b of
                [255, 255, 255, 255] -> True
                _ -> False

    describe "Encoder - Word8" $ do
        it "Encodes standard byte" $ do
            encodeWord8 128 `shouldSatisfy` \b -> case runBuilder b of
                [128] -> True
                _ -> False
        it "Encodes max byte" $ do
            encodeWord8 255 `shouldSatisfy` \b -> case runBuilder b of
                [255] -> True
                _ -> False

    describe "Encoder - Bool" $ do
        it "Encodes True" $ do
            encodeBool True `shouldSatisfy` \b -> case runBuilder b of
                [1] -> True
                _ -> False
        it "Encodes False" $ do
            encodeBool False `shouldSatisfy` \b -> case runBuilder b of
                [0] -> True
                _ -> False

    describe "Encoder - String" $ do
        it "Encodes simple string ('hello')" $ do
            encodeString "hello" `shouldSatisfy` \b -> case runBuilder b of
                [0, 0, 0, 5, 104, 101, 108, 108, 111] -> True
                _ -> False
        it "Encodes empty string" $ do
            encodeString "" `shouldSatisfy` \b -> case runBuilder b of
                [0, 0, 0, 0] -> True
                _ -> False
        it "Encodes special characters" $ do
            encodeString "€" `shouldSatisfy` \b -> case runBuilder b of
                [0, 0, 0, 3, 226, 130, 172] -> True
                _ -> False
