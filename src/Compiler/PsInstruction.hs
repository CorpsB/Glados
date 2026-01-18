{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- PsInstruction
-}

{-|
Module : Compiler.PsInstruction
Description : Intermediate Assembly representation.
Stability : stable
-}
module Compiler.PsInstruction
    ( PsInstruction(..)
    ) where

import Compiler.Instruction (Instruction)
import Data.Text (Text)

-- | Pseudo-instructions used during compilation/assembly.
--
-- @details
--   PsInstruction is an intermediate representation used by the compiler to
--   express labels and label-referencing operations before a dedicated
--   assembly pass resolves textual labels to concrete instruction offsets.
--   Many constructors here resolve to a 'Real Instruction' when labels are
--   resolved in the assembly pass.
--
-- @return
--   N/A (this documents the type)
--
data PsInstruction
    -- | A concrete instruction ready for bytecode generation
    = Real Instruction

    -- | Defines a label position (e.g., "func_main:")
    | LabelDef Text

    -- | Jumps to Label (resolved in assembly pass)
    | JumpLabel Text              -- ^ Resolves to Real (Jump off)
    | JumpIfFalseLabel Text       -- ^ Resolves to Real (JumpIfFalse off)
    | JumpIfTrueLabel Text        -- ^ Resolves to Real (JumpIfTrue off)

    -- | Function Calls to Label
    | CallLabel Text              -- ^ Resolves to Real (Call off)
    | TailCallLabel Text          -- ^ Resolves to Real (TailCall off)

    -- | Closure & Address Ops to Label
    -- Used when we need the address of a function defined by a label
    | MakeClosureLabel Text Int   -- ^ Resolves to Real (MakeClosure off n)
    | GetFuncAddrLabel Text       -- ^ Resolves to Real (GetFuncAddr off)

    deriving (Show, Eq)
