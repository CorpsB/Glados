{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CodeGeneration
-}

{-|
Module : Compiler.ASM.Compiler
Description : High-level code generation (Flow control, Definitions, Calls).
Stability : experimental
-}
module Compiler.ASM.Compiler 
    ( compileIf
    , compileDefine
    ) where

import Data.Text (Text, pack)

import Compiler.ASM.CompilerMonad
import Compiler.Instruction (Instruction(..))
import AST.Ast (Ast(..))

-- | Compiles a conditional expression (If-Then-Else).
--
-- @args
--   - compileFn: The recursive compilation function.
--   - cond: The condition AST.
--   - thenBranch: The AST for the True branch.
--   - elseBranch: The AST for the False branch.
--
-- @details
--   Generates labels for the Else block and the End.
--   Emits JumpIfFalse to skip the Then block if the condition is false.
--   Emits an unconditional Jump after the Then block to skip the Else block.
--
compileIf :: (Ast -> CompilerMonad ()) -> Ast -> Ast -> Ast -> CompilerMonad ()
compileIf compileFn cond thenBranch elseBranch = do
    lElse <- generateUniqueLabel (pack "else")
    lEnd  <- generateUniqueLabel (pack "endif")
    compileFn cond
    emitJumpIfFalseToLabel lElse
    compileFn thenBranch
    emitJumpToLabel lEnd
    emitLabelDefinition lElse
    compileFn elseBranch
    emitLabelDefinition lEnd

-- | Compiles a variable definition.
--
-- @args
--   - compileFn: The recursive compilation function.
--   - name: The name of the variable to define.
--   - body: The expression to evaluate.
--
-- @details
--   1. Compiles the body (pushes value to stack).
--   2. Allocates a new global index using 'defineSymbol'.
--   3. Emits 'StoreGlobal' to save the value from the stack.
--
compileDefine :: (Ast -> CompilerMonad ()) -> Text -> Ast -> CompilerMonad ()
compileDefine compileFn name body = do
    compileFn body
    idx <- defineSymbol name
    emitInstruction (StoreGlobal idx)
