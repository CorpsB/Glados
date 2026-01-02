{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Instruction
-}

{-# LANGUAGE DeriveGeneric #-}
module Compiler.Instruction (
    Instruction(..),
    instructionSize,
    serializeInstruction
) where

import Data.Int (Int32)
import Data.Word (Word8)
import qualified Data.ByteString.Builder as B
import Compiler.Encoder (encodeInt32BE, encodeWord8, encodeBool)

-- | Compiler-side VM Instruction set
-- Each constructor matches a primitive VM instruction.
data Instruction
    = Push Int32
    | PushBool Bool
    | Add
    | Sub
    | Jump Int32
    | JumpIfFalse Int32
    | Load Int32
    | Store Int32
    | Call Int32
    | Ret
    | MakeClosure Int32 Int32
    | Halt
    deriving (Show, Eq)

-- | Returns the size in bytes of the encoded instruction
instructionSize :: Instruction -> Int
instructionSize (Push _) = 1 + 4
instructionSize (PushBool _) = 1 + 1
instructionSize Add = 1
instructionSize Sub = 1
instructionSize (Jump _) = 1 + 4
instructionSize (JumpIfFalse _) = 1 + 4
instructionSize (Load _) = 1 + 4
instructionSize (Store _) = 1 + 4
instructionSize (Call _) = 1 + 4
instructionSize Ret = 1
instructionSize (MakeClosure _ _) = 1 + 4 + 4
instructionSize Halt = 1

-- | Serializes an instruction to a Builder
serializeInstruction :: Instruction -> B.Builder
serializeInstruction (Push n) =
    encodeWord8 0x01 <> encodeInt32BE n
serializeInstruction (PushBool b) =
    encodeWord8 0x02 <> encodeBool b
serializeInstruction Add =
    encodeWord8 0x03
serializeInstruction Sub =
    encodeWord8 0x04
serializeInstruction (Jump addr) =
    encodeWord8 0x05 <> encodeInt32BE addr
serializeInstruction (JumpIfFalse a) =
    encodeWord8 0x06 <> encodeInt32BE a
serializeInstruction (Load i) =
    encodeWord8 0x07 <> encodeInt32BE i
serializeInstruction (Store i) =
    encodeWord8 0x08 <> encodeInt32BE i
serializeInstruction (Call addr) =
    encodeWord8 0x09 <> encodeInt32BE addr
serializeInstruction Ret =
    encodeWord8 0x0A
serializeInstruction (MakeClosure a c) =
    encodeWord8 0x0B <> encodeInt32BE a <> encodeInt32BE c
serializeInstruction Halt =
    encodeWord8 0xFF
