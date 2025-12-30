{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstToAsm
-}

{-|
Module : Compiler.ASM.AstToAsm
Description : Atomic converters from AST nodes to Assembly instructions.
Stability : experimental
-}
module Compiler.ASM.AstToAsm
    ( astIntToAsm
    , astBoolToAsm
    , astSymbolToAsm
    , astListToAsm
    , astCallToAsm
    , builtinMap
    ) where

import Control.Monad.State (get, lift)
import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import Data.Int (Int64)

import Compiler.ASM.CompilerMonad
    ( CompilerMonad
    , emitInstruction
    , emitCallToLabel
    )
import Compiler.CompilerState (CompilerState(..), ScopeType(..))
import Compiler.Instruction (Instruction(..), Immediate(..))
import Common.Type.Integer (IntValue(..))
import AST.Ast (Ast(..))

-- | Mapping of builtin operator names to their VM instructions.
builtinMap :: Map.Map Text Instruction
builtinMap = Map.fromList
    [ (pack "+", Add)
    , (pack "-", Sub)
    , (pack "*", Mul)
    , (pack "div", Div)
    , (pack "mod", Mod)
    , (pack "==", Eq)
    , (pack "<", Lt)
    , (pack "<=", Le)
    ]

-- | Converts a native Haskell Int to a PUSH instruction.
--
-- @args
--   - n: The integer to convert.
--
-- @details
--   Converts the Int to Int64 (safe default) and emits a Push instruction.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
astIntToAsm :: Int -> CompilerMonad ()
astIntToAsm n = 
    let int64Val = I64 (fromIntegral n :: Int64)
    in emitInstruction (Push (ImmInt int64Val))

-- | Converts a native Haskell Bool to a PUSH instruction.
--
-- @args
--   - b: The boolean to convert.
--
-- @details
--   Emits a Push instruction with an immediate boolean value.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
astBoolToAsm :: Bool -> CompilerMonad ()
astBoolToAsm b = emitInstruction (Push (ImmBool b))

-- | Converts a symbol (variable) to a LOAD instruction.
--
-- @args
--   - name: The name of the symbol.
--
-- @details
--   Looks up the symbol in the 'csSymbols' table. Based on the associated
--   'ScopeType', it emits either 'LoadGlobal', 'LoadLocal', or 'LoadCapture'
--   with the correct index. Returns an error if the symbol is undefined.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
astSymbolToAsm :: Text -> CompilerMonad ()
astSymbolToAsm name = do
    currentState <- get
    case Map.lookup name (csSymbols currentState) of
        Just (ScopeGlobal, idx)  -> emitInstruction (LoadGlobal idx)
        Just (ScopeLocal, idx)   -> emitInstruction (LoadLocal idx)
        Just (ScopeCapture, idx) -> emitInstruction (LoadCapture idx)
        Nothing  -> lift $ Left (pack "Undefined symbol: " <> name)

-- | Converts a literal AST list to assembly instructions.
--
-- @args
--   - compileFn: The recursive compilation function.
--   - elements: The list of AST nodes to compile.
--
-- @details
--   Compiles each element pushing them onto the stack, then calls the
--   builtin "list" function to construct the list object.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
astListToAsm :: (Ast -> CompilerMonad ()) -> [Ast] -> CompilerMonad ()
astListToAsm compileFn elements = mapM_ compileFn elements >>
    emitCallToLabel (pack "list")

-- | Compiles a function call (Builtin or User-defined).
--
-- @args
--   - compileFn: The recursive compilation function.
--   - callee: The AST representing the function being called.
--   - args: The list of argument ASTs.
--
-- @details
--   If the callee is a known builtin, emits the specific instruction.
--   Otherwise, emits a 'CallLabel' to the named function.
--   (Higher-order calls are explicitly not supported in this version).
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
astCallToAsm :: (Ast -> CompilerMonad ()) -> Ast -> [Ast] -> CompilerMonad ()
astCallToAsm compileFn callee args = case callee of
    ASymbol name -> 
        case Map.lookup name builtinMap of
            Just instr -> mapM_ compileFn args >> emitInstruction instr
            Nothing -> mapM_ compileFn args >> emitCallToLabel name
    _ -> lift $ Left (pack "Error: Higher calls are not supported yet.")
