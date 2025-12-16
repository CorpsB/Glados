{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerState
-}

{-|
Module : Compiler.CompilerState
Description : Core types and state for the Compiler logic.
Stability : stable
-}
module Compiler.CompilerState
    ( CompilerState(..)
    , SymTable
    , createCompilerState
    ) where

import Compiler.PsInstruction (PsInstruction(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)

-- | Symbol table mapping textual names to local indices.
--
-- @details
--   SymTable stores mapping commonly used by the compiler to resolve local
--   variables to their index within a function frame. Keys are 'Text' names
--   and values are 'Int' indices.
--
type SymTable = Map Text Int

-- | Compiler state (kept simple and strict to avoid accidental thunks)
--
-- @details
--   The CompilerState stores the accumulated pseudo-instructions, local symbol
--   mapping, next index to allocate for locals, and an internal counter for
--   generating unique labels.
--
-- @return
--   N/A (this documents the type)
--
data CompilerState = CompilerState
    { csCode      :: (Seq PsInstruction)  -- ^ Generated code (in natural order)
    , csSymbols   :: SymTable          -- ^ Local variable mapping
    , csNextIndex :: Int               -- ^ Next available local index
    , csLabelCnt  :: Int               -- ^ Counter for unique label generation
    } deriving (Show, Eq)

-- | Initial, empty compiler state.
--
-- @details
--   Provides a fresh CompilerState with no code, an empty symbol table, the
--   next local index set to 0 and the label counter set to 0.
--
-- @return
--   A 'CompilerState' initialized for a new compilation unit.
--
createCompilerState :: CompilerState
createCompilerState = CompilerState
    { csCode = Seq.empty
    , csSymbols = Map.empty
    , csNextIndex = 0
    , csLabelCnt = 0
    }
