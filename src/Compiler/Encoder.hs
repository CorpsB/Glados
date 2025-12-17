{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Encoder.hs
-}

{-|
Module      : Compiler.Encoder
Description : Low-level binary encoding utilities for bytecode generation.
Stability   : stable

This module provides helper functions to encode primitive values into
binary form using 'Data.ByteString.Builder'. These functions are used by
the assembler and encoder stages to produce the final bytecode stream.

Supported encodings:
- 32-bit signed integers (big-endian)
- Single bytes (Word8)
- Booleans (encoded as 0 or 1)
- UTF-8 strings with length prefix
-}
module Compiler.Encoder
    ( encodeInt32BE
    , encodeWord8
    , encodeBool
    , encodeString
    ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Int (Int32)
import Data.Word (Word8)

-- | Encode a signed 32-bit integer in big-endian format.
--
-- This function emits exactly 4 bytes corresponding to the given 'Int32'
-- value, encoded in network byte order (big-endian).
--
-- It is primarily used for instruction operands such as relative offsets,
-- indices, and immediate integer values.
encodeInt32BE :: Int32 -> B.Builder
encodeInt32BE = B.int32BE

-- | Encode a single byte.
--
-- Writes the given 'Word8' value as-is into the output stream.
-- This function is typically used to encode opcodes and small immediate
-- values.
encodeWord8 :: Word8 -> B.Builder
encodeWord8 = B.word8

-- | Encode a boolean value as a single byte.
--
-- Encoding convention:
--
-- * 'True'  is encoded as @1@
-- * 'False' is encoded as @0@
--
-- This representation is used consistently throughout the bytecode format.
encodeBool :: Bool -> B.Builder
encodeBool True  = B.word8 1
encodeBool False = B.word8 0

-- | Encode a UTF-8 string with a length prefix.
--
-- The string is first encoded as UTF-8, then preceded by a 32-bit
-- big-endian integer representing the number of bytes in the encoded
-- string.
--
-- Layout:
--
-- > [length: Int32][utf8-bytes...]
--
-- This encoding is suitable for constant strings, symbol names, or
-- metadata embedded in the bytecode.
encodeString :: String -> B.Builder
encodeString = ((B.int32BE . fromIntegral . BS.length)
    <> B.byteString) . TE.encodeUtf8 . T.pack
