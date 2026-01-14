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
    , ScopeType(..)
    , createCompilerState
    ) where

import Compiler.PsInstruction (PsInstruction(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)

-- | Defines the memory scope of a variable.
--
-- @details
--   This enumeration is used in the symbol table to determine which
--   VM instruction (LOAD_GLOBAL, LOAD_LOCAL, or LOAD_CAPTURE) should be
--   emitted when a symbol is referenced.
--
data ScopeType 
    = ScopeGlobal   -- ^ Global variable (LOAD_GLOBAL)
    | ScopeLocal    -- ^ Local argument or variable (LOAD_LOCAL)
    | ScopeCapture  -- ^ Variable captured by a closure (LOAD_CAPTURE)
    deriving (Show, Eq)

-- | Symbol table mapping textual names to their scope and index.
--
-- @details
--   SymTable stores the mapping required to resolve variable names to
--   concrete memory indices. 
--     - ScopeType: Determines which LOAD instruction to use.
--     - MemoryIndex: The integer offset in the stack/heap.
--     - TypeName: The string representation of the variable's type.
--
type SymTable = Map Text (ScopeType, Int, Text)

-- | Compiler state holding code buffers and symbol information.
--
-- @details
--   The CompilerState maintains two code buffers: 'csCode' for the current
--   block being compiled (e.g., main or current function body) and 'csFuncs'
--   for accumulating the code of defined functions and lambdas that will be
--   appended at the end of the binary.
--
-- @return
--   N/A (this documents the type)
--
data CompilerState = CompilerState
    { csCode      :: (Seq PsInstruction)      -- ^ Generated code (in natural order)
    , csFuncs     :: Seq PsInstruction        -- ^ Compiled functions' buffer
    , csSymbols   :: SymTable                 -- ^ Local variable mapping
    , csStructs   :: Map Text [(Text, Text)]  -- ^ Struct definitions (Name -> Field Names in order)
    , csNextIndex :: Int                      -- ^ Next available local index
    , csLabelCnt  :: Int                      -- ^ Counter for unique label generation
    , csScopeContext :: ScopeType             -- ^ Scope context (used to determine variables scopes)
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
    , csFuncs = Seq.empty
    , csSymbols = Map.empty
    , csStructs = Map.empty
    , csNextIndex = 0
    , csLabelCnt = 0
    , csScopeContext = ScopeGlobal
    }
