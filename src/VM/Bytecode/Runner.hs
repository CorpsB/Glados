{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Execution Engine
-}

{-|
Module      : VM.Bytecode.Runner
Description : The central Fetch-Decode-Execute loop.
Stability   : stable

This module orchestrates the execution of the Virtual Machine.
It fetches opcodes from the bytecode and dispatches them to the appropriate
instruction handlers defined in the Instruction modules.
-}
module VM.Bytecode.Runner
    ( runBytecode
    , executeInstruction
    ) where

import Control.Monad.State.Strict (get)
import Data.Word (Word8)

import VM.VMState (VMState(..), VirtualMachine)
import VM.Bytecode.Reader (readByte)
import VM.Instruction.Index
import VM.Instruction.Stack
import VM.Instruction.Arithmetic
import VM.Instruction.Logic
import VM.Instruction.Struct
import VM.Instruction.List
import VM.Instruction.Function
import VM.Instruction.Variable
import VM.Instruction.System

-- | Maps a raw opcode byte to its corresponding instruction function.
--
-- @args
--   - op: The 8-bit instruction code fetched from bytecode.
--
-- @details
--   This pattern matching acts as the specific "Decode" step of the CPU cycle.
--   Any unknown opcode will trigger a runtime error.
--
executeInstruction :: Word8 -> VirtualMachine ()
executeInstruction 0x01 = instPush
executeInstruction 0x02 = instPop
executeInstruction 0x03 = instDup
executeInstruction 0x04 = instSwap

executeInstruction 0x10 = instAdd
executeInstruction 0x11 = instSub
executeInstruction 0x12 = instMul
executeInstruction 0x13 = instDiv
executeInstruction 0x14 = instMod

executeInstruction 0x20 = instEq
executeInstruction 0x21 = instLt
executeInstruction 0x22 = instNot
executeInstruction 0x23 = instAnd
executeInstruction 0x24 = instOr
executeInstruction 0x25 = instLe

executeInstruction 0x30 = instJump
executeInstruction 0x31 = instJumpIfFalse
executeInstruction 0x32 = instJumpIfTrue

executeInstruction 0x40 = instCall
executeInstruction 0x41 = instTailCall
executeInstruction 0x42 = instCallIndirect
executeInstruction 0x43 = instRet

executeInstruction 0x50 = instLoadLocal
executeInstruction 0x51 = instStoreLocal
executeInstruction 0x52 = instLoadGlobal
executeInstruction 0x53 = instStoreGlobal
executeInstruction 0x54 = instLoadCapture
executeInstruction 0x55 = instStoreCapture

executeInstruction 0x60 = instMakeClosure
executeInstruction 0x61 = instGetFuncAddr
executeInstruction 0x62 = instBuildStruct
executeInstruction 0x63 = instGetStructField

executeInstruction 0x70 = instPrint
executeInstruction 0x71 = instHalt
executeInstruction 0x72 = instExit
executeInstruction 0x80 = instCast

executeInstruction 0x90 = instCons
executeInstruction 0x91 = instHead
executeInstruction 0x92 = instTail

executeInstruction 0xFE = instCheckStack
executeInstruction 0xFF = return () -- NOP
executeInstruction op = error $ "VM Error: Unknown Opcode 0x" ++ show op

-- | The main execution loop of the Virtual Machine.
--
-- @details
--   This function operates in a continuous cycle:
--   1. Checks if the VM 'isRunning' flag is True.
--   2. Fetches the next opcode using 'readByte'.
--   3. Calls 'executeInstruction' to run the associated logic.
--   4. Recursively calls itself to process the next instruction.
--
--   The loop terminates when 'instHalt' sets 'isRunning' to False
--   or if an error occurs (e.g., Stack Underflow).
--
runBytecode :: VirtualMachine ()
runBytecode = do
    vm <- get
    case isRunning vm of
        False -> return ()
        True -> do
            opcode <- readByte
            executeInstruction opcode
            runBytecode
