{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Builder
-}

module Compiler.Builder (
    writeInt32BE,
    writeWord8,
    writeBoolByte,
    writeStringUTF8
) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))

writeInt32BE :: Int32 -> B.Builder
writeInt32BE = B.int32BE

writeWord8 :: Word8 -> B.Builder
writeWord8 = B.word8

writeBoolByte :: Bool -> B.Builder
writeBoolByte True = B.word8 1
writeBoolByte False = B.word8 0

writeStringUTF8 :: ByteString -> B.Builder
writeStringUTF8 bs = B.int32BE (fromIntegral $ BS.length bs) <> B.byteString bs
