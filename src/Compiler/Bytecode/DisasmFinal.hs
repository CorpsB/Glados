{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- DisasmFinal
-}

{-|
Module      : Compiler.Bytecode.DisasmFinal
Description : Disassembler for the Glados binary format.
Stability   : stable

This module provides functionality to decode a binary bytecode stream into a
human-readable assembly format. It handles the complete instruction set,
including variable-length payloads, immediate values, and system operations.
-}
module Compiler.Bytecode.DisasmFinal
    ( disasmFinal
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Bits
import Data.Word (Word8, Word16)
import Data.Int (Int8, Int16)
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import Compiler.Instruction (Immediate(..), immediateSize)
import Common.Type.Integer (IntValue(..))

-- | Disassemble final bytecode into (offset, pretty-instruction) pairs.
--
-- @args
--   - bs: The raw ByteString containing the bytecode (header should be stripped).
--
-- @details
--   This function runs the binary parser 'disasmAll' over the input string.
--   It catches parsing errors (e.g., truncated files) and returns a list of
--   tuples containing the byte offset and the string representation of the
--   instruction.
--
-- @return
--   Either an error message (Text) or a list of (Offset, InstructionString).
--
disasmFinal :: ByteString -> Either Text [(Int, String)]
disasmFinal bs =
    case runGetOrFail disasmAll (BL.fromStrict bs) of
        Left (_, off, msg) ->
            Left (T.pack $ "Disasm error at " ++ show off ++ ": " ++ msg)
        Right (_, _, xs)   -> Right xs

-- | Recursive parser to disassemble all instructions.
--
-- @details
--   Loops until the input stream is empty. For each iteration, it records
--   the current offset, parses one instruction via 'disasmInstr', and
--   accumulates the result.
--
disasmAll :: Get [(Int, String)]
disasmAll = do
    done <- isEmpty
    if done then return [] else do
        off <- fromIntegral <$> bytesRead
        instr <- disasmInstr
        rest <- disasmAll
        return ((off, instr) : rest)

-- | Parse a single instruction.
--
-- @details
--   Reads the opcode (Word8) and delegates to 'decodeOp' to parse the
--   operands and format the string.
--
disasmInstr :: Get String
disasmInstr = getWord8 >>= decodeOp

-- | Decodes an Opcode into a formatted string.
--
-- @args
--   - op: The opcode byte to decode.
--
-- @details
--   Dispatches based on the opcode ranges defined in ASM_SPEC.md.
--   Handles PUSH, Control Flow, Memory, Structs, IO, and System ops.
--   Returns "UNKNOWN" for undefined opcodes and "PADDING" for 0x00.
--
decodeOp :: Word8 -> Get String
decodeOp 0x00 = pure "PADDING"
decodeOp 0xFF = pure "NOP"

-- Stack
decodeOp 0x01 = decodePush
decodeOp 0x60 = decodeMakeClosure
decodeOp 0xFE = fmap (("CHECK_STACK " ++) . show) getInt32be

-- Jumps (0x30 - 0x39)
decodeOp op | op >= 0x30 && op <= 0x39 = decodeJump op

-- Calls (0x40 - 0x49)
decodeOp op | op >= 0x40 && op <= 0x49 = decodeCall op

-- Memory (0x50 - 0x59)
decodeOp op | op >= 0x50 && op <= 0x59 = decodeMem op

-- Structs & Reflection (0x61 - 0x69, 0x80 - 0x81)
decodeOp op | op >= 0x61 && op <= 0x69 = decodeStruct op
decodeOp 0x80 = fmap (("CAST " ++) . show) getWord8
decodeOp 0x81 = pure "TYPEOF"

-- IO System (0xB0 - 0xB4)
decodeOp op | op >= 0xB0 && op <= 0xB4 = decodeIO op

-- System
decodeOp 0x70 = pure "PRINT"
decodeOp 0x71 = pure "HALT"
decodeOp 0x72 = pure "EXIT"

-- Fallback
decodeOp op = return $ fromMaybe
    ("<UNKNOWN 0x" ++ showHex2 op ++ ">") (lookupSimple op)

-- === specific Decoders ===

-- | Decodes a PUSH instruction and its typed payload.
--
-- @details
--   Reads the TypeID byte following the opcode and parses the
--   subsequent bytes according to that type (Bool, Int8..64, Char).
--
decodePush :: Get String
decodePush = getWord8 >>= decodePushVal

decodePushVal :: Word8 -> Get String
decodePushVal 0x00 = do
    b <- getWord8
    pure $ "PUSH BOOL " ++ (if b == 0 then "False" else "True")

-- Integers (Signed & Unsigned)
decodePushVal 0x01 = fmap (("PUSH I8 " ++) . show . (fromIntegral :: Word8 -> Int8)) getWord8
decodePushVal 0x02 = fmap (("PUSH UI8 " ++) . show) getWord8
decodePushVal 0x03 = fmap (("PUSH I16 " ++) . show . (fromIntegral :: Word16 -> Int16)) getWord16be
decodePushVal 0x04 = fmap (("PUSH UI16 " ++) . show) getWord16be
decodePushVal 0x05 = fmap (("PUSH I32 " ++) . show) getInt32be
decodePushVal 0x06 = fmap (("PUSH UI32 " ++) . show) getWord32be
decodePushVal 0x07 = fmap (("PUSH I64 " ++) . show) getInt64be
decodePushVal 0x08 = fmap (("PUSH UI64 " ++) . show) getWord64be

-- Char (explicit cast to avoid ambiguity)
decodePushVal 0x09 = fmap (("PUSH CHAR " ++) . show . (toEnum :: Int -> Char) . fromIntegral) getWord8

-- Unknown types (skips payload based on immediateSize)
decodePushVal t = skip (immSize t) >> pure ("PUSH (Type " ++ show t ++ ")")

-- | Decodes Jump instructions.
decodeJump :: Word8 -> Get String
decodeJump 0x30 = fmap (("JUMP " ++) . show) getInt32be
decodeJump 0x31 = fmap (("JUMP_IF_FALSE " ++) . show) getInt32be
decodeJump 0x32 = fmap (("JUMP_IF_TRUE " ++) . show) getInt32be
decodeJump _    = pure "UNKNOWN_JUMP"

-- | Decodes Call instructions.
decodeCall :: Word8 -> Get String
decodeCall 0x40 = fmap (("CALL " ++) . show) getInt32be
decodeCall 0x41 = fmap (("TAILCALL " ++) . show) getInt32be
decodeCall 0x42 = pure "CALL_INDIRECT"
decodeCall 0x43 = pure "RET"
decodeCall _    = pure "UNKNOWN_CALL"

-- | Decodes Memory operations (Load/Store).
decodeMem :: Word8 -> Get String
decodeMem 0x50 = fmap (("LOAD_LOCAL " ++) . show) getInt32be
decodeMem 0x51 = fmap (("STORE_LOCAL " ++) . show) getInt32be
decodeMem 0x52 = fmap (("LOAD_GLOBAL " ++) . show) getInt32be
decodeMem 0x53 = fmap (("STORE_GLOBAL " ++) . show) getInt32be
decodeMem 0x54 = fmap (("LOAD_CAPTURE " ++) . show) getInt32be
decodeMem 0x55 = fmap (("STORE_CAPTURE " ++) . show) getInt32be
decodeMem _    = pure "UNKNOWN_MEM"

-- | Decodes Struct and Closure operations.
decodeStruct :: Word8 -> Get String
decodeStruct 0x61 = fmap (("GET_FUNC_ADDR " ++) . show) getInt32be
decodeStruct 0x62 = fmap (("BUILD_STRUCT " ++) . show) getInt32be
decodeStruct 0x63 = fmap (("GET_FIELD " ++) . show) getInt32be
decodeStruct 0x64 = fmap (("SET_FIELD " ++) . show) getInt32be
decodeStruct _    = pure "UNKNOWN_STRUCT"

-- | Decodes IO operations.
decodeIO :: Word8 -> Get String
decodeIO 0xB0 = pure "OPEN"
decodeIO 0xB1 = pure "READ"
decodeIO 0xB2 = pure "WRITE"
decodeIO 0xB3 = pure "CLOSE"
decodeIO 0xB4 = pure "INPUT"
decodeIO _    = pure "UNKNOWN_IO"

-- | Decodes MAKE_CLOSURE (has 2 operands).
decodeMakeClosure :: Get String
decodeMakeClosure = do
    a <- getInt32be
    n <- getInt32be
    pure $ "MAKE_CLOSURE @" ++ show a ++ ", captures=" ++ show n

-- === Helpers ===

-- | Lookup simple Stack/Arithmetic/Logic instructions.
--
-- @args
--   - op: The opcode byte.
--
-- @return
--   Just String if found, Nothing otherwise.
--
lookupSimple :: Word8 -> Maybe String
lookupSimple op = lookupStack op >>= return
  where
    lookupStack o | o >= 0x02 && o <= 0x04 = simpleStack o
                  | o >= 0x10 && o <= 0x14 = simpleArith o
                  | o >= 0x20 && o <= 0x25 = simpleLogic o
                  | otherwise = Nothing

simpleStack :: Word8 -> Maybe String
simpleStack 0x02 = Just "POP"
simpleStack 0x03 = Just "DUP"
simpleStack 0x04 = Just "SWAP"
simpleStack _    = Nothing

simpleArith :: Word8 -> Maybe String
simpleArith 0x10 = Just "ADD"
simpleArith 0x11 = Just "SUB"
simpleArith 0x12 = Just "MUL"
simpleArith 0x13 = Just "DIV"
simpleArith 0x14 = Just "MOD"
simpleArith _    = Nothing

simpleLogic :: Word8 -> Maybe String
simpleLogic 0x20 = Just "EQ"
simpleLogic 0x21 = Just "LT"
simpleLogic 0x22 = Just "NOT"
simpleLogic 0x23 = Just "AND"
simpleLogic 0x24 = Just "OR"
simpleLogic 0x25 = Just "LE"
simpleLogic _    = Nothing

-- | Format a byte as a 2-digit hex string.
showHex2 :: Word8 -> String
showHex2 w = let h = "0123456789ABCDEF"
                 hi = fromIntegral ((w `shiftR` 4) .&. 0xF)
                 lo = fromIntegral (w .&. 0xF)
             in [h !! hi, h !! lo]

-- | Determines payload size for unknown PUSH types (to skip them safely).
immSize :: Word8 -> Int
immSize op = maybe 1 immediateSize (lookupTypeID op)

-- | Helper to map raw TypeID bytes back to Immediate constructors.
-- Note: Values (0) are dummies, we only care about the type for size lookup.
lookupTypeID :: Word8 -> Maybe Immediate
lookupTypeID 0x00 = Just (ImmBool False)
lookupTypeID 0x01 = Just (ImmInt (I8 0))
lookupTypeID 0x02 = Just (ImmInt (UI8 0))
lookupTypeID 0x03 = Just (ImmInt (I16 0))
lookupTypeID 0x04 = Just (ImmInt (UI16 0))
lookupTypeID op   = lookupLarge op

lookupLarge :: Word8 -> Maybe Immediate
lookupLarge 0x05 = Just (ImmInt (I32 0))
lookupLarge 0x06 = Just (ImmInt (UI32 0))
lookupLarge 0x07 = Just (ImmInt (I64 0))
lookupLarge 0x08 = Just (ImmInt (UI64 0))
lookupLarge _    = Nothing
