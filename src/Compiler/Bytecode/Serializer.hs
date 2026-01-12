{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- SerializeInstruction
-}

module Compiler.Bytecode.Serializer (serializeInstruction) where

import Compiler.Instruction
import qualified Data.ByteString.Builder as B
import Compiler.Bytecode.Encoder (encodeInt32BE, encodeWord8, encodeBool)
import Common.Type.Integer (IntValue(..))
import Data.Word (Word8)

-- | Serializes an 'Instruction' to a 'Builder' for binary encoding.
--
-- @args
--   - instr: the 'Instruction' to serialize
--
-- @details
--   Converts the instruction into its binary representation using the
--   appropriate opcode and payload encoding defined in ASM_SPEC.md.
--
-- @return
--   The encoded instruction as a 'Builder'.
--
serializeInstruction :: Instruction -> B.Builder
serializeInstruction (Push (ImmBool b)) =
    encodeWord8 0x01 <>
    encodeWord8 0x00 <>
    encodeBool b
serializeInstruction (Push (ImmInt (I8 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x01 <>
    encodeWord8 (fromIntegral n)
serializeInstruction (Push (ImmInt (UI8 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x02 <>
    encodeWord8 (fromIntegral n)
serializeInstruction (Push (ImmInt (I16 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x03 <>
    B.int16BE n
serializeInstruction (Push (ImmInt (UI16 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x04 <>
    B.word16BE n
serializeInstruction (Push (ImmInt (I32 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x05 <>
    B.int32BE (fromIntegral n)
serializeInstruction (Push (ImmInt (UI32 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x06 <>
    B.word32BE n
serializeInstruction (Push (ImmInt (I64 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x07 <>
    B.int64BE n
serializeInstruction (Push (ImmInt (UI64 n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x08 <>
    B.word64BE n
serializeInstruction (Push (ImmInt (IChar n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x09 <>
    encodeWord8 (fromIntegral n)
serializeInstruction (Push (ImmInt (UIChar n))) =
    encodeWord8 0x01 <>
    encodeWord8 0x10 <>
    encodeWord8 (fromIntegral n)

serializeInstruction Pop = encodeWord8 0x02
serializeInstruction Dup = encodeWord8 0x03
serializeInstruction Swap = encodeWord8 0x04

serializeInstruction Add = encodeWord8 0x10
serializeInstruction Sub = encodeWord8 0x11
serializeInstruction Mul = encodeWord8 0x12
serializeInstruction Div = encodeWord8 0x13
serializeInstruction Mod = encodeWord8 0x14

serializeInstruction Eq = encodeWord8 0x20
serializeInstruction Lt = encodeWord8 0x21
serializeInstruction Not = encodeWord8 0x22
serializeInstruction And = encodeWord8 0x23
serializeInstruction Or = encodeWord8 0x24
serializeInstruction Le = encodeWord8 0x25

serializeInstruction (Jump addr) =
    encodeWord8 0x30 <>
    encodeInt32BE (fromIntegral addr)
serializeInstruction (JumpIfFalse addr) =
    encodeWord8 0x31 <>
    encodeInt32BE (fromIntegral addr)
serializeInstruction (JumpIfTrue addr) =
    encodeWord8 0x32 <>
    encodeInt32BE (fromIntegral addr)

serializeInstruction (Call addr) =
    encodeWord8 0x40 <>
    encodeInt32BE (fromIntegral addr)
serializeInstruction (TailCall addr) =
    encodeWord8 0x41 <>
    encodeInt32BE (fromIntegral addr)
serializeInstruction CallIndirect = encodeWord8 0x42
serializeInstruction Ret = encodeWord8 0x43

serializeInstruction (LoadLocal idx) =
    encodeWord8 0x50 <>
    encodeInt32BE (fromIntegral idx)
serializeInstruction (StoreLocal idx) =
    encodeWord8 0x51 <>
    encodeInt32BE (fromIntegral idx)
serializeInstruction (LoadGlobal idx) =
    encodeWord8 0x52 <>
    encodeInt32BE (fromIntegral idx)
serializeInstruction (StoreGlobal idx) =
    encodeWord8 0x53 <>
    encodeInt32BE (fromIntegral idx)
serializeInstruction (LoadCapture idx) =
    encodeWord8 0x54 <>
    encodeInt32BE (fromIntegral idx)
serializeInstruction (StoreCapture idx) =
    encodeWord8 0x55 <>
    encodeInt32BE (fromIntegral idx)

serializeInstruction (MakeClosure addr n) =
    encodeWord8 0x60 <>
    encodeInt32BE (fromIntegral addr) <>
    encodeInt32BE (fromIntegral n)
serializeInstruction (GetFuncAddr idx) =
    encodeWord8 0x61 <>
    encodeInt32BE (fromIntegral idx)
serializeInstruction (BuildStruct n) =
    encodeWord8 0x62 <>
    encodeInt32BE (fromIntegral n)
serializeInstruction (Cast typeId) =
    encodeWord8 0x80 <>
    encodeWord8 typeId

serializeInstruction Print = encodeWord8 0x70
serializeInstruction Halt = encodeWord8 0x71
serializeInstruction (CheckStack n) =
    encodeWord8 0xFE <>
    encodeInt32BE (fromIntegral n)
serializeInstruction Nop = encodeWord8 0xFF
