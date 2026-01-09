{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- SerializerImpl: Full instruction serialization (all opcodes)
-}

module Compiler.SerializerImpl (serializeInstructionFull) where

import Compiler.Instruction
import qualified Data.ByteString.Builder as B
import Compiler.Bytecode.Encoder (encodeInt32BE, encodeWord8, encodeBool)
import Common.Type.Integer (IntValue(..))
import Data.Word (Word8)

serializeInstructionFull :: Instruction -> B.Builder
serializeInstructionFull (Push (ImmInt (I8 n))) =
    encodeWord8 0x01 <> encodeWord8 0x01 <> encodeWord8 (fromIntegral n)
serializeInstructionFull (Push (ImmInt (UI8 n))) =
    encodeWord8 0x01 <> encodeWord8 0x02 <> encodeWord8 (fromIntegral n)
serializeInstructionFull (Push (ImmInt (I16 n))) =
    encodeWord8 0x01 <> encodeWord8 0x03 <> B.int16BE n
serializeInstructionFull (Push (ImmInt (UI16 n))) =
    encodeWord8 0x01 <> encodeWord8 0x04 <> B.word16BE n
serializeInstructionFull (Push (ImmInt (UI32 n))) =
    encodeWord8 0x01 <> encodeWord8 0x06 <> B.word32BE n
serializeInstructionFull (Push (ImmInt (I64 n))) =
    encodeWord8 0x01 <> encodeWord8 0x07 <> B.int64BE n
serializeInstructionFull (Push (ImmInt (UI64 n))) =
    encodeWord8 0x01 <> encodeWord8 0x08 <> B.word64BE n
serializeInstructionFull Mul =
    encodeWord8 0x12
serializeInstructionFull Div =
    encodeWord8 0x13
serializeInstructionFull Mod =
    encodeWord8 0x14
serializeInstructionFull Eq =
    encodeWord8 0x20
serializeInstructionFull Lt =
    encodeWord8 0x21
serializeInstructionFull Le =
    encodeWord8 0x25
serializeInstructionFull Not =
    encodeWord8 0x22
serializeInstructionFull And =
    encodeWord8 0x23
serializeInstructionFull Or =
    encodeWord8 0x24
serializeInstructionFull (JumpIfTrue addr) =
    encodeWord8 0x32 <> encodeInt32BE (fromIntegral addr)
serializeInstructionFull (TailCall addr) =
    encodeWord8 0x41 <> encodeInt32BE (fromIntegral addr)
serializeInstructionFull CallIndirect =
    encodeWord8 0x42
serializeInstructionFull (LoadLocal idx) =
    encodeWord8 0x50 <> encodeInt32BE (fromIntegral idx)
serializeInstructionFull (StoreLocal idx) =
    encodeWord8 0x51 <> encodeInt32BE (fromIntegral idx)
serializeInstructionFull (LoadGlobal idx) =
    encodeWord8 0x52 <> encodeInt32BE (fromIntegral idx)
serializeInstructionFull (StoreGlobal idx) =
    encodeWord8 0x53 <> encodeInt32BE (fromIntegral idx)
serializeInstructionFull (LoadCapture idx) =
    encodeWord8 0x54 <> encodeInt32BE (fromIntegral idx)
serializeInstructionFull (StoreCapture idx) =
    encodeWord8 0x55 <> encodeInt32BE (fromIntegral idx)
serializeInstructionFull (GetFuncAddr idx) =
    encodeWord8 0x61 <> encodeInt32BE (fromIntegral idx)
serializeInstructionFull (Cast tid) =
    encodeWord8 0x80 <> encodeWord8 tid
serializeInstructionFull Print =
    encodeWord8 0x70
serializeInstructionFull (CheckStack n) =
    encodeWord8 0xFE <> encodeInt32BE (fromIntegral n)
serializeInstructionFull Nop =
    encodeWord8 0xFF
