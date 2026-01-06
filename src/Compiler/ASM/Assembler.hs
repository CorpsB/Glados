{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Assembler.hs
-}

module Compiler.ASM.Assembler
    ( assemble
    ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Monad (foldM)

import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..), Immediate(..), getInstCode, immediateToTypeID, immediateSize)
import Compiler.Bytecode.Encoder (encodeInt32BE, encodeWord8, encodeBool)
import Common.Type.Integer (IntValue(..))

import qualified Data.Set as Set
import Control.Monad (foldM, when)
import Data.Foldable (traverse_)

magicBytes :: B.Builder
magicBytes = B.word8 0x47 <> B.word8 0x4C <> B.word8 0x41 <> B.word8 0x44

versionBytes :: B.Builder
versionBytes = B.word8 0x01 <> B.word8 0x00

flagBytes :: B.Builder
flagBytes = B.word8 0x00 <> B.word8 0x00

type LabelMap = Map.Map Text Int

assemble :: [PsInstruction] -> Either Text BS.ByteString
assemble ps = do
    (lMap, size) <- foldM buildMap (Map.empty, 0) ps
    insts <- resolveLabels lMap ps

    let optimized = peephole insts

    validateJumps optimized size
    return $ finalizeBytecode optimized

validateJumps :: [Instruction] -> Int -> Either Text ()
validateJumps insts totalSize = 
    let
        offsets = scanl (\off i -> off + instructionSize i) 0 insts
        validSet = Set.fromList offsets

        check (off, inst) = case getJumpTarget off (instructionSize inst) inst of
            Nothing  -> Right ()
            Just tgt -> 
                ensure (tgt >= 0 && tgt < totalSize) ("Jump out of bounds: " <> pack (show tgt)) *>
                ensure (Set.member tgt validSet)     ("Jump target not aligned: " <> pack (show tgt))
    in
        traverse_ check (zip offsets insts)

getJumpTarget :: Int -> Int -> Instruction -> Maybe Int
getJumpTarget off sz (Jump rel)        = Just (off + sz + rel)
getJumpTarget off sz (JumpIfFalse rel) = Just (off + sz + rel)
getJumpTarget off sz (JumpIfTrue rel)  = Just (off + sz + rel)
getJumpTarget off sz (Call rel)        = Just (off + sz + rel)
getJumpTarget off sz (TailCall rel)    = Just (off + sz + rel)
getJumpTarget off sz (MakeClosure rel _) = Just (off + sz + rel)
getJumpTarget _   _  _                 = Nothing

ensure :: Bool -> Text -> Either Text ()
ensure True _ = Right ()
ensure False msg = Left msg

buildMap :: (LabelMap, Int) -> PsInstruction -> Either Text (LabelMap, Int)
buildMap (acc, off) (LabelDef name)
    | Map.member name acc = Left $ "Duplicate label: " <> name
    | otherwise           = Right (Map.insert name off acc, off)
buildMap (acc, off) inst  = Right (acc, off + pseudoSize inst)

finalizeBytecode :: [Instruction] -> BS.ByteString
finalizeBytecode insts =
    let body   = B.toLazyByteString $ foldMap serializeInstruction insts
        header = buildHeader (fromIntegral $ BL.length body)
    in BL.toStrict . B.toLazyByteString $ header <> body

buildHeader :: Int32 -> B.Builder
buildHeader size = 
    magicBytes <> versionBytes <> flagBytes <> encodeInt32BE size

pseudoSize :: PsInstruction -> Int
pseudoSize (LabelDef _) = 0
pseudoSize (Real inst)  = instructionSize inst
pseudoSize (JumpLabel _) = 5
pseudoSize (JumpIfFalseLabel _) = 5
pseudoSize (JumpIfTrueLabel _) = 5
pseudoSize (CallLabel _) = 5
pseudoSize (TailCallLabel _) = 5
pseudoSize (GetFuncAddrLabel _) = 5
pseudoSize (MakeClosureLabel _ _) = 9

instructionSize :: Instruction -> Int
instructionSize inst = 1 + payloadSize inst

payloadSize :: Instruction -> Int
payloadSize (Push imm) = 1 + immediateSize imm
payloadSize (Jump _)  = 4
payloadSize (JumpIfFalse _) = 4
payloadSize (JumpIfTrue _) = 4
payloadSize (Call _) = 4
payloadSize (TailCall _) = 4
payloadSize (LoadLocal _) = 4
payloadSize (StoreLocal _) = 4
payloadSize (LoadGlobal _) = 4
payloadSize (StoreGlobal _) = 4
payloadSize (LoadCapture _) = 4
payloadSize (StoreCapture _) = 4
payloadSize (MakeClosure _ _) = 8
payloadSize (GetFuncAddr _) = 4
payloadSize (Cast _) = 1
payloadSize (CheckStack _) = 4
payloadSize _ = 0

peephole :: [Instruction] -> [Instruction]
peephole = id

serializeInstruction :: Instruction -> B.Builder
serializeInstruction inst = 
    encodeWord8 (getInstCode inst) <> serializePayload inst

serializePayload :: Instruction -> B.Builder
serializePayload (Push imm) = 
    encodeWord8 (immediateToTypeID imm) <> serializeImmediate imm
serializePayload (Jump off) = encodeInt32BE (fromIntegral off)
serializePayload (JumpIfFalse off) = encodeInt32BE (fromIntegral off)
serializePayload (JumpIfTrue off) = encodeInt32BE (fromIntegral off)
serializePayload (Call off) = encodeInt32BE (fromIntegral off)
serializePayload (TailCall off) = encodeInt32BE (fromIntegral off)
serializePayload (LoadLocal idx) = encodeInt32BE (fromIntegral idx)
serializePayload (StoreLocal idx) = encodeInt32BE (fromIntegral idx)
serializePayload (LoadGlobal idx) = encodeInt32BE (fromIntegral idx)
serializePayload (StoreGlobal idx) = encodeInt32BE (fromIntegral idx)
serializePayload (LoadCapture idx) = encodeInt32BE (fromIntegral idx)
serializePayload (StoreCapture idx) = encodeInt32BE (fromIntegral idx)
serializePayload (MakeClosure off n) = 
    encodeInt32BE (fromIntegral off) <> encodeInt32BE (fromIntegral n)
serializePayload (GetFuncAddr off) = encodeInt32BE (fromIntegral off)
serializePayload (Cast typeId) = encodeWord8 typeId
serializePayload (CheckStack n) = encodeInt32BE (fromIntegral n)
serializePayload _ = mempty

serializeImmediate :: Immediate -> B.Builder
serializeImmediate (ImmBool b) = encodeBool b
serializeImmediate (ImmInt (I8 i)) = B.int8 i
serializeImmediate (ImmInt (UI8 i)) = B.word8 i
serializeImmediate (ImmInt (I16 i)) = B.int16BE i
serializeImmediate (ImmInt (UI16 i)) = B.word16BE i
serializeImmediate (ImmInt (I32 i)) = B.int32BE i
serializeImmediate (ImmInt (UI32 i)) = B.word32BE i
serializeImmediate (ImmInt (I64 i)) = B.int64BE i
serializeImmediate (ImmInt (UI64 i)) = B.word64BE i
