{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerMonad
-}

{-|
Module : Compiler.CompilerMonad
Description : Compilation monad and explicit instruction emission helpers.
Stability : stable
-}
module Compiler.ASM.CompilerMonad
    (CompilerMonad
    , emitInstruction
    , emitLabelDefinition
    , emitJumpToLabel
    , emitJumpIfFalseToLabel
    , emitJumpIfTrueToLabel
    , emitCallToLabel
    , generateUniqueLabel
    , defineSymbol
    ) where

import Control.Monad.State
import Data.Text (Text, pack)
import Data.Sequence ((|>))
import qualified Data.Map.Strict as Map

import Compiler.CompilerState (CompilerState(..))
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction)

-- | Compilation monad used throughout the compiler.
--
-- @details
--   CompilerMonad is a stateful monad carrying the 'CompilerState' and allowing
--   early failure using 'Either Text'. It is responsible for emitting pseudo-
--   instructions, managing labels, and updating compiler state consistently.
--
-- @return
--   A monadic computation producing a value of type @a@ or a compilation error.
--
type CompilerMonad a = StateT CompilerState (Either Text) a

-- | Append a pseudo-instruction to the current code sequence.
--
-- @args
--   - pseudoInst: the 'PsInstruction' to append
--
-- @details
--   This internal helper mutates the compiler state by appending the given
--   pseudo-instruction to the end of the code buffer. The underlying data
--   structure is a 'Data.Sequence.Seq', allowing efficient O(1) appends.
--   This function is intentionally kept private to enforce controlled emission.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
appendPseudoInstruction :: PsInstruction -> CompilerMonad ()
appendPseudoInstruction pseudoInst = modify $ \currentState ->
    currentState { csCode = csCode currentState |> pseudoInst }

-- | Emit a concrete machine instruction.
--
-- @args
--   - instruction: the concrete 'Instruction' to emit
--
-- @details
--   Wraps a real VM instruction into a 'PsInstruction' and appends it to the
--   current code stream. This function is used for all non-label bytecode
--   instructions such as arithmetic, stack, and memory operations.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
emitInstruction :: Instruction -> CompilerMonad ()
emitInstruction instruction = appendPseudoInstruction (Real instruction)

-- | Define a label position in the emitted code.
--
-- @args
--   - labelName: textual name of the label
--
-- @details
--   Inserts a label definition at the current position in the code stream.
--   Labels are resolved later during the assembly pass into concrete offsets.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
emitLabelDefinition :: Text -> CompilerMonad ()
emitLabelDefinition labelName = appendPseudoInstruction (LabelDef labelName)

-- | Emit an unconditional jump to a label.
--
-- @args
--   - labelName: target label name
--
-- @details
--   Emits a pseudo-instruction representing an unconditional jump whose
--   destination will be resolved during the assembly phase.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
emitJumpToLabel :: Text -> CompilerMonad ()
emitJumpToLabel labelName = appendPseudoInstruction (JumpLabel labelName)

-- | Emit a conditional jump (jump if false) to a label.
--
-- @args
--   - labelName: target label name
--
-- @details
--   Emits a conditional jump that transfers control if the boolean value on
--   top of the stack evaluates to false. The actual jump offset is resolved
--   during assembly.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
emitJumpIfFalseToLabel :: Text -> CompilerMonad ()
emitJumpIfFalseToLabel labelName = appendPseudoInstruction (
    JumpIfFalseLabel labelName)

-- | Emit a conditional jump (jump if true) to a label.
--
-- @args
--   - labelName: target label name
--
-- @details
--   Emits a conditional jump that transfers control if the boolean value on
--   top of the stack evaluates to true. The actual jump offset is resolved
--   during assembly.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
emitJumpIfTrueToLabel :: Text -> CompilerMonad ()
emitJumpIfTrueToLabel labelName = appendPseudoInstruction (
    JumpIfTrueLabel labelName)

-- | Emit a function call to a label.
--
-- @args
--   - labelName: target function label
--
-- @details
--   Emits a pseudo-instruction representing a function call. The label will
--   be resolved to a concrete instruction offset during assembly.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
emitCallToLabel :: Text -> CompilerMonad ()
emitCallToLabel labelName = appendPseudoInstruction (CallLabel labelName)

-- | Generate a unique label name using a given prefix.
--
-- @args
--   - prefixName: textual prefix for the generated label
--
-- @details
--   Uses the internal 'csLabelCnt' counter from 'CompilerState' to produce a
--   unique label name of the form @<prefix>_<n>@. The counter is incremented
--   atomically as part of the state update.
--
-- @return
--   A unique label name as 'Text'.
--
generateUniqueLabel :: Text -> CompilerMonad Text
generateUniqueLabel prefixName = do
    currentState <- get
    put $ currentState { csLabelCnt = (csLabelCnt currentState) + 1 }
    return $ prefixName <> pack "_" <> pack (show (csLabelCnt currentState))

-- | Defines a new global symbol and allocates an index for it.
--
-- @args
--   - name: The name of the symbol to define.
--
-- @details
--   Checks if the symbol is already defined in 'csSymbols'. If it is, returns
--   an error. Otherwise, allocates the current 'csNextIndex', inserts the
--   symbol into the map, and increments the index counter.
--
-- @return
--   The allocated index (Int) or an error (Left Text).
--
defineSymbol :: Text -> CompilerMonad Int
defineSymbol name = do
    s <- get
    if Map.member name (csSymbols s)
        then lift $ Left (pack "Symbol already defined: " <> name)
        else do
            let idx = csNextIndex s
            put s { 
                csSymbols = Map.insert name idx (csSymbols s), 
                csNextIndex = idx + 1 
            }
            return idx
