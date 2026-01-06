{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Bytecode Reader
-}

{-|
Module      : VM.Bytecode.Reader
Description : Binary reading primitives for all integer sizes.
Stability   : stable

This module provides low-level functions to consume bytes from the VM's bytecode stream.
It supports reading signed and unsigned integers of 8, 16, 32, and 64 bits in 
Big Endian format (Network Byte Order).
-}
module VM.Bytecode.Reader
    ( readByte
    , readInt8
    , readWord8
    , readInt16
    , readWord16
    , readInt32
    , readWord32
    , readInt64
    , readWord64
    ) where

import Control.Monad.State.Strict (get, put)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

import VM.VMState (VMState(..), VirtualMachine)

-- | Reads a single raw byte (8 bits) from the bytecode.
--
-- @details
--   Retrieves the byte at the current 'bytecodeIndex' and increments the index.
--   Performs a bounds check to prevent reading past the end of the bytecode.
--
-- @throws
--   Error "VM Error: Unexpected End of Bytecode" if the index is out of bounds.
--
-- @return
--   The read 'Word8'.
--
readByte :: VirtualMachine Word8
readByte = do
    vm <- get
    let code = bytecode vm
    let idx = bytecodeIndex vm
    case idx >= BS.length code of
        True ->
            error "VM Error: Unexpected End of Bytecode (Segmentation Fault)"
        False -> put (vm { bytecodeIndex = idx + 1 }) >>
            return (BS.index code idx)

-- | Reads an unsigned 8-bit integer.
--
-- @return The value as 'Word8'.
--
readWord8 :: VirtualMachine Word8
readWord8 = readByte

-- | Reads a signed 8-bit integer.
--
-- @return The value as 'Int8'.
--
readInt8 :: VirtualMachine Int8
readInt8 = fromIntegral <$> readByte

-- | Reads an unsigned 16-bit integer (Big Endian).
--
-- @return The value as 'Word16'.
--
readWord16 :: VirtualMachine Word16
readWord16 = do
    b1 <- fromIntegral <$> readByte
    b2 <- fromIntegral <$> readByte
    return $ (b1 `shiftL` 8) .|. b2

-- | Reads a signed 16-bit integer (Big Endian).
--
-- @return The value as 'Int16'.
--
readInt16 :: VirtualMachine Int16
readInt16 = fromIntegral <$> readWord16

-- | Reads an unsigned 32-bit integer (Big Endian).
--
-- @return The value as 'Word32'.
--
readWord32 :: VirtualMachine Word32
readWord32 = do
    b1 <- fromIntegral <$> readByte
    b2 <- fromIntegral <$> readByte
    b3 <- fromIntegral <$> readByte
    b4 <- fromIntegral <$> readByte
    return $ (b1 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 8) .|. b4

-- | Reads a signed 32-bit integer (Big Endian) and returns it as a native 'Int'.
--
-- @details
--   This function is optimized for general usage (like jumping or vector indexing).
--   It reads 4 bytes, interprets them as a signed Int32, and casts to the native 'Int' type.
--   On 64-bit systems, this preserves the sign correctly.
--
-- @return
--   The value as a standard 'Int'.
--
readInt32 :: VirtualMachine Int
readInt32 = do
    w32 <- readWord32
    return (fromIntegral (fromIntegral w32 :: Int32))

-- | Reads an unsigned 64-bit integer (Big Endian).
--
-- @return The value as 'Word64'.
--
readWord64 :: VirtualMachine Word64
readWord64 = do
    b1 <- fromIntegral <$> readByte; b2 <- fromIntegral <$> readByte
    b3 <- fromIntegral <$> readByte; b4 <- fromIntegral <$> readByte
    b5 <- fromIntegral <$> readByte; b6 <- fromIntegral <$> readByte
    b7 <- fromIntegral <$> readByte; b8 <- fromIntegral <$> readByte
    return $ (b1 `shiftL` 56) .|. (b2 `shiftL` 48) .|. (b3 `shiftL` 40) .|.
        (b4 `shiftL` 32) .|. (b5 `shiftL` 24) .|. (b6 `shiftL` 16) .|.
        (b7 `shiftL` 8)  .|. b8

-- | Reads a signed 64-bit integer (Big Endian).
--
-- @return The value as 'Int64'.
--
readInt64 :: VirtualMachine Int64
readInt64 = fromIntegral <$> readWord64
