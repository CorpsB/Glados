{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- DisasmFinal
-}

module Compiler.Bytecode.DisasmFinal (disasmFinal) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Bits
import Data.Word (Word8)
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import Compiler.Instruction (Immediate(..), immediateSize)
import Common.Type.Integer (IntValue(..))

-- | Disassemble final bytecode into (offset, pretty-instruction) pairs.
disasmFinal :: ByteString -> Either Text [(Int, String)]
disasmFinal bs =
    case runGetOrFail disasmAll (BL.fromStrict bs) of
        Left (_, off, msg) ->
            Left (T.pack $ "Disasm error at "++show off++": "++msg)
        Right (_, _, xs)   -> Right xs

-- | Disassemble all instructions until end of input
disasmAll :: Get [(Int, String)]
disasmAll = do
    done <- isEmpty
    if done
        then return []
        else do
            off <- fromIntegral <$> bytesRead
            instr <- disasmInstr
            rest <- disasmAll
            return ((off, instr) : rest)

-- | Disassemble a single instruction, returning pretty string
disasmInstr :: Get String
disasmInstr = getWord8 >>= decodeOp

decodeOp :: Word8 -> Get String
decodeOp 0x01 = decodePush
decodeOp 0x60 = decodeMakeClosure
decodeOp 0xFE = fmap (("CHECK_STACK " ++) . show) getInt32be
decodeOp op | op >= 0x30 && op <= 0x39 = decodeJump op
decodeOp op | op >= 0x40 && op <= 0x49 = decodeCall op
decodeOp op | op >= 0x50 && op <= 0x59 = decodeMem op
decodeOp op | op >= 0x61 && op <= 0x80 = decodeMisc op
decodeOp op = return $ fromMaybe
    ("<UNKNOWN 0x"++showHex2 op++">") (lookupSimple op)

decodePush :: Get String
decodePush = getWord8 >>= decodePushVal

decodePushVal :: Word8 -> Get String
decodePushVal 0x00 = do
    b <- getWord8
    pure $ "PUSH " ++ (if b == 0 then "False" else "True")
decodePushVal 0x05 = fmap (("PUSH " ++) . show) getInt32be
decodePushVal t = skip (immSize t) >> pure "PUSH<?>"

decodeJump :: Word8 -> Get String
decodeJump 0x30 = fmap (("JUMP " ++) . show) getInt32be
decodeJump 0x31 = fmap (("JUMP_IF_FALSE " ++) . show) getInt32be
decodeJump 0x32 = fmap (("JUMP_IF_TRUE " ++) . show) getInt32be
decodeJump _ = pure "UNKNOWN_JUMP"

decodeCall :: Word8 -> Get String
decodeCall 0x40 = fmap (("CALL " ++) . show) getInt32be
decodeCall 0x41 = fmap (("TAILCALL " ++) . show) getInt32be
decodeCall 0x42 = pure "CALL_INDIRECT"
decodeCall 0x43 = pure "RET"
decodeCall _ = pure "UNKNOWN_CALL"

decodeMem :: Word8 -> Get String
decodeMem 0x50 = fmap (("LOAD_LOCAL " ++) . show) getInt32be
decodeMem 0x51 = fmap (("STORE_LOCAL " ++) . show) getInt32be
decodeMem 0x52 = fmap (("LOAD_GLOBAL " ++) . show) getInt32be
decodeMem 0x53 = fmap (("STORE_GLOBAL " ++) . show) getInt32be
decodeMem 0x54 = fmap (("LOAD_CAPTURE " ++) . show) getInt32be
decodeMem 0x55 = fmap (("STORE_CAPTURE " ++) . show) getInt32be
decodeMem _ = pure "UNKNOWN_MEM"

decodeMakeClosure :: Get String
decodeMakeClosure = do
    a <- getInt32be
    n <- getInt32be
    pure $ "MAKE_CLOSURE " ++ show a ++ ", " ++ show n

decodeMisc :: Word8 -> Get String
decodeMisc 0x61 = fmap (("GET_FUNC_ADDR " ++) . show) getInt32be
decodeMisc 0x80 = fmap (("CAST " ++) . show) getWord8
decodeMisc 0x70 = pure "PRINT"
decodeMisc 0x71 = pure "HALT"
decodeMisc _ = pure "UNKNOWN_MISC"

lookupSimple :: Word8 -> Maybe String
lookupSimple op = lookupStack op >>= return
  where
    lookupStack 0xFF = Just "NOP"
    lookupStack o | o >= 0x02 && o <= 0x04 = simpleStack o
                  | o >= 0x10 && o <= 0x14 = simpleArith o
                  | o >= 0x20 && o <= 0x25 = simpleLogic o
                  | otherwise = Nothing

simpleStack :: Word8 -> Maybe String
simpleStack 0x02 = Just "POP"
simpleStack 0x03 = Just "DUP"
simpleStack 0x04 = Just "SWAP"
simpleStack _ = Nothing

simpleArith :: Word8 -> Maybe String
simpleArith 0x10 = Just "ADD"
simpleArith 0x11 = Just "SUB"
simpleArith 0x12 = Just "MUL"
simpleArith 0x13 = Just "DIV"
simpleArith 0x14 = Just "MOD"
simpleArith _ = Nothing

simpleLogic :: Word8 -> Maybe String
simpleLogic 0x20 = Just "EQ"
simpleLogic 0x21 = Just "LT"
simpleLogic 0x22 = Just "NOT"
simpleLogic 0x23 = Just "AND"
simpleLogic 0x24 = Just "OR"
simpleLogic 0x25 = Just "LE"
simpleLogic _ = Nothing

-- | Helper: show opcode as 2-digit hex
showHex2 :: Word8 -> String
showHex2 w = let h = "0123456789ABCDEF"
                 hi = fromIntegral ((w `shiftR` 4) .&. 0xF)
                 lo = fromIntegral (w .&. 0xF)
             in [h !! hi, h !! lo]

-- | Immediate size for unknown typeId
immSize :: Word8 -> Int
immSize op = maybe 1 immediateSize (lookupTypeID op)

lookupTypeID :: Word8 -> Maybe Immediate
lookupTypeID 0x00 = Just (ImmBool False)
lookupTypeID 0x01 = Just (ImmInt (I8 0))
lookupTypeID 0x02 = Just (ImmInt (UI8 0))
lookupTypeID 0x03 = Just (ImmInt (I16 0))
lookupTypeID 0x04 = Just (ImmInt (UI16 0))
lookupTypeID op = lookupLarge op

lookupLarge :: Word8 -> Maybe Immediate
lookupLarge 0x05 = Just (ImmInt (I32 0))
lookupLarge 0x06 = Just (ImmInt (UI32 0))
lookupLarge 0x07 = Just (ImmInt (I64 0))
lookupLarge 0x08 = Just (ImmInt (UI64 0))
lookupLarge _    = Nothing
