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
import Compiler.CompilerState (CompilerState(..))
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
astIntToAsm :: Int -> CompilerMonad ()
astIntToAsm n = 
    let int64Val = I64 (fromIntegral n :: Int64)
    in emitInstruction (Push (ImmInt int64Val))

-- | Converts a native Haskell Bool to a PUSH instruction.
--
-- @args
--   - b: The boolean to convert.
--
astBoolToAsm :: Bool -> CompilerMonad ()
astBoolToAsm b = emitInstruction (Push (ImmBool b))

-- | Converts a symbol (variable) to a LOAD instruction.
--
-- @args
--   - name: The name of the symbol.
--
-- @details
--   Looks up the symbol in the 'csSymbols' table.
--   If found, emits 'LoadGlobal' with the corresponding index.
--   If not found, returns a compilation error.
--   Note: Builtins are handled in the Call logic, not here.
--
astSymbolToAsm :: Text -> CompilerMonad ()
astSymbolToAsm name = do
    currentState <- get
    case Map.lookup name (csSymbols currentState) of
        Just idx -> emitInstruction (LoadGlobal idx)
        Nothing  -> lift $ Left (pack "Undefined symbol: " <> name)

-- | Converts a literal AST list to assembly instructions.
--
-- @args
--   - compileFn: The recursive compilation function (Dependency Injection).
--   - elements: The list of AST nodes to compile.
--
-- @details
--   1. Compiles each element (pushing them onto the stack).
--   2. Calls the runtime builtin "list" to construct the list object.
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
--   - If 'callee' is a symbol found in 'builtinMap', emits the corresponding
--     instruction after compiling arguments.
--   - If 'callee' is a user symbol, emits 'CallLabel'.
--   - Higher-order calls (complex callee) are currently unsupported.
--
astCallToAsm :: (Ast -> CompilerMonad ()) -> Ast -> [Ast] -> CompilerMonad ()
astCallToAsm compileFn callee args = case callee of
    ASymbol name -> 
        case Map.lookup name builtinMap of
            Just instr -> mapM_ compileFn args >> emitInstruction instr
            Nothing -> mapM_ compileFn args >> emitCallToLabel name
    _ -> lift $ Left (pack "Error: Higher calls are not supported yet.")
