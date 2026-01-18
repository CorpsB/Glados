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

import Compiler.ASM.Builtins (builtinMap)
import Compiler.ASM.CompilerMonad
    ( CompilerMonad
    , emitInstruction
    , emitCallToLabel
    , lookupSymbol
    )
import Compiler.CompilerState (CompilerState(..), ScopeType(..))
import Compiler.Instruction (Instruction(..), Immediate(..))
import Common.Type.Integer (IntValue(..))
import AST.Ast (Ast(..))

-- | Pushes an Integer AST value directly to the stack.
--
-- @args
--   - val: The IntValue from the AST.
--
-- @details
--   Since the AST construction (Parser) already uses 'fitInteger' to determine
--   the optimal storage size (I8, I16, etc.), we simply wrap the value in
--   an 'ImmInt' and emit the PUSH instruction.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
astIntToAsm :: IntValue -> CompilerMonad ()
astIntToAsm v = emitInstruction (Push (ImmInt v))

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
        Just (ScopeGlobal, idx, _)  -> emitInstruction (LoadGlobal idx)
        Just (ScopeLocal, idx, _)   -> emitInstruction (LoadLocal idx)
        Just (ScopeCapture, idx, _) -> emitInstruction (LoadCapture idx)
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
    emitInstruction (BuildList (length elements))

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
astCallToAsm compileFn (ASymbol name) [a, b] | name == pack ">" =
    compileFn b >> compileFn a >>
    emitInstruction Lt
astCallToAsm compileFn (ASymbol name) [a, b] | name == pack ">=" =
    compileFn b >> compileFn a >>
    emitInstruction Le
astCallToAsm compileFn (ASymbol name) [a, b] | name == pack "neq?" =
    compileFn a >> compileFn b >>
    emitInstruction Eq >> emitInstruction Not
astCallToAsm compileFn (ASymbol name) [a, b] | name == pack "tneq?" =
    compileFn a >> compileFn b >>
    emitInstruction TEq >> emitInstruction Not
astCallToAsm compileFn (ASymbol name) args = case Map.lookup name builtinMap of
    Just instr -> mapM_ compileFn args >> emitInstruction instr
    Nothing -> do
        sym <- lookupSymbol name
        case sym of
            Just _ -> mapM_ compileFn args >> compileFn (ASymbol name) >>
                emitInstruction CallIndirect
            Nothing -> mapM_ compileFn args >>
                emitCallToLabel (pack "fun_" <> name)
astCallToAsm _ (AInteger _) _ =
    lift $ Left (pack "Error: Cannot call an Integer")
astCallToAsm _ (ABool _) _ =
    lift $ Left (pack "Error: Cannot call a Boolean")
astCallToAsm _ AVoid _ =
    lift $ Left (pack "Error: Cannot call Void")
astCallToAsm compileFn expr args =
    mapM_ compileFn args >> compileFn expr >>
    emitInstruction CallIndirect
