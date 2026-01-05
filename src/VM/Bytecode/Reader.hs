{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Bytecode Reader
-}

module VM.BytecodeReader
    ( readByte
    , readInt32
    ) where

import Control.Monad.State.Strict (get, put)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Word (Word8)

import VM.VMState (VMState(..), VirtualMachine)

-- | Reads a single byte (Word8) from the bytecode and advances the program index.
--
-- @details
--   This function checks if the instruction pointer is within bounds.
--   If successful, it returns the byte and increments 'programIndex' by 1.
--   If end of code is reached, it throws a runtime error.
--
-- @return
--   The 'Word8' at the current program index.
readByte :: VirtualMachine Word8
readByte = do
    vm <- get
    let code = bytecode vm
    let idx = programIndex vm
    if idx >= BS.length code
        then error "VM Error: Unexpected End of Bytecode (Segmentation Fault)"
        else let byte = BS.index code idx >>
            put $ vm { programIndex = idx + 1 } >>
            return byte

-- | Reads 4 bytes representing a 32-bit Integer (Big Endian) and advances index.
--
-- @details
--   This function calls 'readByte' 4 times to reconstruct a 32-bit integer.
--   We assume Big Endian encoding (Standard Network Order) as generated
--   by the assembler.
--
--   Note: We return 'Int' (native size) instead of 'Int32' to facilitate
--   usage with Vector indices and jump offsets without constant casting.
--
-- @return
--   The decoded 'Int' value.
readInt32 :: VirtualMachine Int
readInt32 = do
    b1 <- fromIntegral <$> readByte
    b2 <- fromIntegral <$> readByte
    b3 <- fromIntegral <$> readByte
    b4 <- fromIntegral <$> readByte
    let result :: Int32
        result = (b1 `shiftL` 24) .|. 
                 (b2 `shiftL` 16) .|. 
                 (b3 `shiftL` 8)  .|. 
                 b4
    return (fromIntegral result)
