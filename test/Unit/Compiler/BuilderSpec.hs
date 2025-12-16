{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- BuilderSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.BuilderSpec (spec) where

import Test.Hspec
import Compiler.Builder (writeInt32BE, writeWord8, writeBoolByte)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)
import Data.Int (Int32)

runBuilder :: B.Builder -> [Word8]
runBuilder b = LBS.unpack (B.toLazyByteString b)

spec :: Spec
spec = describe "Builder stubs unit tests" $ do

    describe "writeInt32BE" $ do
        it "Encodes 42" $ do
            writeInt32BE 42 `shouldSatisfy` \b -> case runBuilder b of
                [0,0,0,42] -> True
                _ -> False
        it "Encodes 16909060 -> [1,2,3,4]" $ do
            writeInt32BE 16909060 `shouldSatisfy` \b -> case runBuilder b of
                [1,2,3,4] -> True
                _ -> False

    describe "writeWord8" $ do
        it "Encodes 255" $ do
            writeWord8 255 `shouldSatisfy` \b -> case runBuilder b of
                [255] -> True
                _ -> False

    describe "writeBoolByte" $ do
        it "True -> 1" $ do
            writeBoolByte True `shouldSatisfy` \b -> case runBuilder b of
                [1] -> True
                _ -> False
        it "False -> 0" $ do
            writeBoolByte False `shouldSatisfy` \b -> case runBuilder b of
                [0] -> True
                _ -> False
