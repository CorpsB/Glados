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
import Data.Text (Text, pack)
import Data.Int (Int32)
import qualified Data.Set as Set
import Data.Foldable (traverse_)

import Compiler.PsInstruction (PsInstruction)
import Compiler.Instruction (Instruction(..), instructionSize)
import Compiler.Bytecode.Serializer (serializeInstruction)
import Compiler.ResolveLabels.ResolveLabels (resolveLabels)
import Compiler.Bytecode.Encoder (encodeInt32BE)

magicBytes :: B.Builder
magicBytes = B.word8 0x47 <> B.word8 0x4C <> B.word8 0x41 <> B.word8 0x44 -- GLAD

versionBytes :: B.Builder
versionBytes = B.word8 0x02

flagBytes :: B.Builder
flagBytes = B.word8 0x00

-- | Main assembly function.
--
-- @details
--   1. Resolves labels (PsInstruction -> Instruction) using ResolveLabels module.
--   2. Validates jump offsets (bounds checking).
--   3. Serializes instructions to ByteString with Header.
--
assemble :: [PsInstruction] -> Either Text BS.ByteString
assemble ps = do
    insts <- resolveLabels ps

    let totalSize = sum (map instructionSize insts)
    validateJumps insts totalSize

    return $ finalizeBytecode insts

-- | Serialize the resolved instructions and prepend the header.
--
finalizeBytecode :: [Instruction] -> BS.ByteString
finalizeBytecode insts =
    let body = B.toLazyByteString $ foldMap serializeInstruction insts
        header = buildHeader (fromIntegral $ BL.length body) in
    BL.toStrict . B.toLazyByteString $ header <> B.lazyByteString body

buildHeader :: Int32 -> B.Builder
buildHeader size =
    magicBytes <> versionBytes <> flagBytes <> encodeInt32BE size

-- | Ensures all jumps point to valid instruction boundaries within the code.
--
validateJumps :: [Instruction] -> Int -> Either Text ()
validateJumps insts size =
    let offs = scanl (\off i -> off + instructionSize i) 0 insts
        validOffsets = Set.fromList offs in
    traverse_ (checkJump size validOffsets) (zip offs insts)

-- | Checks a single instruction if it's a Jump.
--
checkJump :: Int -> Set.Set Int -> (Int, Instruction) -> Either Text ()
checkJump size validSet (currentOffset, inst) =
    case getJumpTarget currentOffset (instructionSize inst) inst of
        Nothing -> Right ()
        Just target -> verifyTarget size validSet target

verifyTarget :: Int -> Set.Set Int -> Int -> Either Text ()
verifyTarget size validSet t = case t >= 0 && t < size of
    True -> case Set.member t validSet of
        True -> Right ()
        False -> Left $ pack "Jump target not aligned (mid-instruction): " <>
            pack (show t)
    False -> Left $ pack "Jump out of bounds: " <> pack (show t)

-- | Helper to calculate absolute target of relative jumps.
--
getJumpTarget :: Int -> Int -> Instruction -> Maybe Int
getJumpTarget off sz (Jump rel)          = Just (off + sz + rel)
getJumpTarget off sz (JumpIfFalse rel)   = Just (off + sz + rel)
getJumpTarget off sz (JumpIfTrue rel)    = Just (off + sz + rel)
getJumpTarget off sz (Call rel)          = Just (off + sz + rel)
getJumpTarget off sz (TailCall rel)      = Just (off + sz + rel)
getJumpTarget off sz (MakeClosure rel _) = Just (off + sz + rel)
getJumpTarget _ _ _ = Nothing
