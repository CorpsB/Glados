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
import Data.Text (Text, pack)
import Data.Int (Int32)
import Control.Monad (foldM)

import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..), Immediate(..),
    getInstCode, immediateToTypeID, immediateSize)
import Compiler.Bytecode.Encoder (encodeInt32BE, encodeWord8, encodeBool)

import qualified Data.Set as Set
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

-- Stub pour compilation (Logique implémentée ailleurs)
resolveLabels :: LabelMap -> [PsInstruction] -> Either Text [Instruction]
resolveLabels _ _ = Right []

validateJumps :: [Instruction] -> Int -> Either Text ()
validateJumps insts size =
    let offs = scanl (\off i -> off + instructionSize i) 0 insts
        valid = Set.fromList offs
    in traverse_ (checkJump size valid) (zip offs insts)

checkJump :: Int -> Set.Set Int -> (Int, Instruction) -> Either Text ()
checkJump size set (off, inst) =
    case getJumpTarget off (instructionSize inst) inst of
        Nothing -> Right ()
        Just t -> verifyTarget size set t

verifyTarget :: Int -> Set.Set Int -> Int -> Either Text ()
verifyTarget size set t =
    ensure (t >= 0 && t < size)
        ("Jump out of bounds: " <> pack (show t)) *>
    ensure (Set.member t set)
        ("Jump target not aligned: " <> pack (show t))

getJumpTarget :: Int -> Int -> Instruction -> Maybe Int
getJumpTarget off sz (Jump rel) = Just (off + sz + rel)
getJumpTarget off sz (JumpIfFalse rel) = Just (off + sz + rel)
getJumpTarget off sz (JumpIfTrue rel) = Just (off + sz + rel)
getJumpTarget off sz (Call rel) = Just (off + sz + rel)
getJumpTarget off sz (TailCall rel) = Just (off + sz + rel)
getJumpTarget off sz (MakeClosure rel _) = Just (off + sz + rel)
getJumpTarget _ _ _ = Nothing

ensure :: Bool -> Text -> Either Text ()
ensure True _ = Right ()
ensure False msg = Left msg

buildMap :: (LabelMap, Int) -> PsInstruction -> Either Text (LabelMap, Int)
buildMap (acc, off) (LabelDef name)
    | Map.member name acc = Left $ "Duplicate label: " <> name
    | otherwise = Right (Map.insert name off acc, off)
buildMap (acc, off) inst = Right (acc, off + pseudoSize inst)

finalizeBytecode :: [Instruction] -> BS.ByteString
finalizeBytecode insts =
    let body = B.toLazyByteString $ foldMap serializeInstruction insts
        header = buildHeader (fromIntegral $ BL.length body)
    in BL.toStrict . B.toLazyByteString $ header <> body

buildHeader :: Int32 -> B.Builder
buildHeader size =
    magicBytes <> versionBytes <> flagBytes <> encodeInt32BE size

pseudoSize :: PsInstruction -> Int
pseudoSize (LabelDef _) = 0
pseudoSize (Real inst) = instructionSize inst
pseudoSize (MakeClosureLabel _ _) = 9
pseudoSize _ = 5

instructionSize :: Instruction -> Int
instructionSize inst = 1 + payloadSize inst

payloadSize :: Instruction -> Int
payloadSize (Push imm) = 1 + immediateSize imm
payloadSize (MakeClosure _ _) = 8
payloadSize (Cast _) = 1
payloadSize inst
    | isInt32Inst inst = 4
    | otherwise = 0

isInt32Inst :: Instruction -> Bool
isInt32Inst (Jump _) = True
isInt32Inst (JumpIfFalse _) = True
isInt32Inst (JumpIfTrue _) = True
isInt32Inst (Call _) = True
isInt32Inst (TailCall _) = True
isInt32Inst (LoadLocal _) = True
isInt32Inst (StoreLocal _) = True
isInt32Inst (LoadGlobal _) = True
isInt32Inst (StoreGlobal _) = True
isInt32Inst (LoadCapture _) = True
isInt32Inst (StoreCapture _) = True
isInt32Inst (GetFuncAddr _) = True
isInt32Inst (CheckStack _) = True
isInt32Inst _ = False

peephole :: [Instruction] -> [Instruction]
peephole = id

serializeInstruction :: Instruction -> B.Builder
serializeInstruction inst =
    encodeWord8 (getInstCode inst) <> serializePayload inst

serializePayload :: Instruction -> B.Builder
serializePayload (Push i) =
    encodeWord8 (immediateToTypeID i) <> serializeImmediate i
serializePayload (MakeClosure o n) =
    encodeInt32BE (fromIntegral o) <> encodeInt32BE (fromIntegral n)
serializePayload (Cast t) = encodeWord8 t
serializePayload inst
    | isInt32Inst inst =
        encodeInt32BE (fromIntegral (getInt32Payload inst))
    | otherwise = mempty

getInt32Payload :: Instruction -> Int
getInt32Payload (Jump x) = x
getInt32Payload (JumpIfFalse x) = x
getInt32Payload (JumpIfTrue x) = x
getInt32Payload (Call x) = x
getInt32Payload (TailCall x) = x
getInt32Payload (LoadLocal x) = x
getInt32Payload (StoreLocal x) = x
getInt32Payload (LoadGlobal x) = x
getInt32Payload (StoreGlobal x) = x
getInt32Payload (LoadCapture x) = x
getInt32Payload (StoreCapture x) = x
getInt32Payload (GetFuncAddr x) = x
getInt32Payload (CheckStack x) = x
getInt32Payload _ = 0

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
