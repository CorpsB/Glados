{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Encoder.hs
-}

module Compiler.Encoder (encodeInt32BE, encodeWord8,
    encodeBool, encodeString) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Int (Int32)
import Data.Word (Word8)

encodeInt32BE :: Int32 -> B.Builder
encodeInt32BE = B.int32BE

encodeWord8 :: Word8 -> B.Builder
encodeWord8 = B.word8

encodeBool :: Bool -> B.Builder
encodeBool True  = B.word8 1
encodeBool False = B.word8 0

encodeString :: String -> B.Builder
encodeString = ((B.int32BE . fromIntegral . BS.length)
    <> B.byteString) . TE.encodeUtf8 . T.pack
