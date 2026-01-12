{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- SerializeInstruction
-}

module Compiler.Serializer (serializeInstruction) where

import Compiler.Instruction
import qualified Data.ByteString.Builder as B
import Compiler.Bytecode.Encoder (encodeInt32BE, encodeWord8, encodeBool)
import Common.Type.Integer (IntValue(..))

-- | Serializes an 'Instruction' to a 'Builder' for binary encoding.
--
-- @args
--   - instr: the 'Instruction' to serialize
--
-- @details
--   Converts the instruction into its binary representation using the
--   appropriate opcode and payload encoding. Used for writing bytecode
--   to output files or buffers.
--
-- @return
--   The encoded instruction as a 'Builder'.
--
serializeInstruction :: Instruction -> B.Builder
serializeInstruction (Push (ImmInt (I32 n))) =
    encodeWord8 0x01 <> encodeInt32BE n
serializeInstruction (Push (ImmBool b)) =
    encodeWord8 0x02 <> encodeBool b
serializeInstruction Pop =
    encodeWord8 0x03
serializeInstruction Dup =
    encodeWord8 0x04
serializeInstruction Swap =
    encodeWord8 0x05
serializeInstruction Add =
    encodeWord8 0x06
serializeInstruction Sub =
    encodeWord8 0x07
serializeInstruction (Jump addr) =
    encodeWord8 0x08 <> encodeInt32BE (fromIntegral addr)
serializeInstruction (JumpIfFalse addr) =
    encodeWord8 0x09 <> encodeInt32BE (fromIntegral addr)
serializeInstruction (Call addr) =
    encodeWord8 0x0A <> encodeInt32BE (fromIntegral addr)
serializeInstruction Ret =
    encodeWord8 0x0B
serializeInstruction (MakeClosure addr n) =
    encodeWord8 0x0C <> encodeInt32BE (fromIntegral addr)
                     <> encodeInt32BE (fromIntegral n)
serializeInstruction Halt =
    encodeWord8 0xFF
serializeInstruction _ = encodeWord8 0xFF -- JASCO GAMING DOIT FIX CA
