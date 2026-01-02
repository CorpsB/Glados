{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Instruction
-}

{-|
Module : Compiler.Instruction
Description : Defines the bytecode instructions shared between Compiler and VM.
Stability : stable
-}
module Compiler.Instruction
    ( Instruction(..)
    , Immediate(..)
    , immediateToTypeID
    , immediateSize
    , getInstCode
    ) where

import Data.Word (Word8)
import Common.Type.Integer (IntValue(..))

-- | Immediate values pushed by PUSH.
-- 
-- @details
--   Immediate represents the different immediate payloads that can be encoded
--   after a PUSH opcode. The TypeID encoding follows ASM_SPEC.md (version 0x02).
--
-- @return
--   N/A (this documents the type)
--
data Immediate
    = ImmBool Bool      -- ^ TypeID 0x00
    | ImmInt IntValue   -- ^ TypeID 0x01, 0x03, 0x05, 0x07 (i8..i64)
    deriving (Show, Eq, Ord)

-- | Map an Immediate to the TypeID byte used by PUSH.
--
-- @args
--   - imm: the Immediate value whose TypeID is requested
--
-- @details
--   Returns the one-byte TypeID used by the binary format to encode immediates
--   as specified in the PUSH Type Table of ASM_SPEC.md.
--
-- @return
--   The TypeID as a 'Word8'.
--
immediateToTypeID :: Immediate -> Word8
immediateToTypeID (ImmBool _)       = 0x00
immediateToTypeID (ImmInt (I8 _))   = 0x01
immediateToTypeID (ImmInt (UI8 _))  = 0x02
immediateToTypeID (ImmInt (I16 _))  = 0x03
immediateToTypeID (ImmInt (UI16 _)) = 0x04
immediateToTypeID (ImmInt (I32 _))  = 0x05
immediateToTypeID (ImmInt (UI32 _)) = 0x06
immediateToTypeID (ImmInt (I64 _))  = 0x07
immediateToTypeID (ImmInt (UI64 _)) = 0x08

-- | Size in bytes of the immediate payload (not counting the TypeID byte).
--
-- @args
--   - imm: the Immediate whose payload size is requested
--
-- @details
--   This helper returns the number of bytes that follow the TypeID for the
--   immediate payload. It is useful for computing instruction sizes and label
--   offsets during assembly.
--
-- @return
--   The payload size in bytes as 'Int'.
--
immediateSize :: Immediate -> Int
immediateSize (ImmBool _)       = 1
immediateSize (ImmInt (I8 _))   = 1
immediateSize (ImmInt (UI8 _))  = 1
immediateSize (ImmInt (I16 _))  = 2
immediateSize (ImmInt (UI16 _)) = 2
immediateSize (ImmInt (I32 _))  = 4
immediateSize (ImmInt (UI32 _)) = 4
immediateSize (ImmInt (I64 _))  = 8
immediateSize (ImmInt (UI64 _)) = 8

-- | The Instruction set used by compiler/VM
--
-- @details
--   Each constructor represents a single concrete virtual machine instruction.
--   Numeric opcodes and layout are defined in ASM_SPEC.md (version 0x02).
--
-- @return
--   N/A (this documents the type)
--
data Instruction
    -- 1. Stack Operations
    = Push Immediate          -- ^ 0x01 PUSH [Type] [Value]
    | Pop                     -- ^ 0x02 POP
    | Dup                     -- ^ 0x03 DUP
    | Swap                    -- ^ 0x04 SWAP

    -- 2. Arithmetic
    | Add                     -- ^ 0x10 ADD
    | Sub                     -- ^ 0x11 SUB
    | Mul                     -- ^ 0x12 MUL
    | Div                     -- ^ 0x13 DIV
    | Mod                     -- ^ 0x14 MOD

    -- 3. Logic & Comparison
    | Eq                      -- ^ 0x20 EQ
    | Lt                      -- ^ 0x21 LT
    | Le                      -- ^ 0x25 LE
    | Not                     -- ^ 0x22 NOT
    | And                     -- ^ 0x23 AND
    | Or                      -- ^ 0x24 OR

    -- 4. Flow Control
    | Jump Int                -- ^ 0x30 JUMP [off]
    | JumpIfFalse Int         -- ^ 0x31 JUMP_IF_FALSE [off]
    | JumpIfTrue Int          -- ^ 0x32 JUMP_IF_TRUE [off]

    -- 5. Functions & Calls
    | Call Int                -- ^ 0x40 CALL [off]
    | TailCall Int            -- ^ 0x41 TAILCALL [off]
    | CallIndirect            -- ^ 0x42 CALL_INDIRECT (address on stack)
    | Ret                     -- ^ 0x43 RET

    -- 6. Memory (Variables)
    | LoadLocal Int           -- ^ 0x50 LOAD_LOCAL [idx]
    | StoreLocal Int          -- ^ 0x51 STORE_LOCAL [idx]
    | LoadGlobal Int          -- ^ 0x52 LOAD_GLOBAL [idx]
    | StoreGlobal Int         -- ^ 0x53 STORE_GLOBAL [idx]
    | LoadCapture Int         -- ^ 0x54 LOAD_CAPTURE [idx]
    | StoreCapture Int        -- ^ 0x55 STORE_CAPTURE [idx]

    -- 7. Closures & Types
    | MakeClosure Int Int     -- ^ 0x60 MAKE_CLOSURE [addr] [n_captures]
    | GetFuncAddr Int         -- ^ 0x61 GET_FUNC_ADDR [id]
    | Cast Word8              -- ^ 0x80 CAST [TypeID]

    -- 8. System / Debug
    | Print                   -- ^ 0x70 PRINT
    | Halt                    -- ^ 0x71 HALT
    | CheckStack Int          -- ^ 0xFE CHECK_STACK [N]
    | Nop                     -- ^ 0xFF NOP
    deriving (Show, Eq, Ord)

-- | Opcode mapping for each instruction as defined in doc/ASM_SPEC.md.
--
-- @args
--   - instr: the 'Instruction' to map to its opcode byte
--
-- @details
--   This function centralizes the numeric opcode mapping, ensuring the
--   compiler and assembler use the same byte values when serializing to the
--   .gla format. It returns the opcode byte for a given concrete instruction.
--
-- @return
--   The opcode as a 'Word8'.
--
getInstCode :: Instruction -> Word8
getInstCode (Push _)             = 0x01
getInstCode Pop                  = 0x02
getInstCode Dup                  = 0x03
getInstCode Swap                 = 0x04

getInstCode Add                  = 0x10
getInstCode Sub                  = 0x11
getInstCode Mul                  = 0x12
getInstCode Div                  = 0x13
getInstCode Mod                  = 0x14

getInstCode Eq                   = 0x20
getInstCode Lt                   = 0x21
getInstCode Not                  = 0x22
getInstCode And                  = 0x23
getInstCode Or                   = 0x24
getInstCode Le                   = 0x25

getInstCode (Jump _)             = 0x30
getInstCode (JumpIfFalse _)      = 0x31
getInstCode (JumpIfTrue _)       = 0x32

getInstCode (Call _)             = 0x40
getInstCode (TailCall _)         = 0x41
getInstCode CallIndirect         = 0x42
getInstCode Ret                  = 0x43

getInstCode (LoadLocal _)        = 0x50
getInstCode (StoreLocal _)       = 0x51
getInstCode (LoadGlobal _)       = 0x52
getInstCode (StoreGlobal _)      = 0x53
getInstCode (LoadCapture _)      = 0x54
getInstCode (StoreCapture _)     = 0x55

getInstCode (MakeClosure _ _)    = 0x60
getInstCode (GetFuncAddr _)      = 0x61
getInstCode (Cast _)             = 0x80

getInstCode Print                = 0x70
getInstCode Halt                 = 0x71
getInstCode (CheckStack _)       = 0xFE
getInstCode Nop                  = 0xFF
