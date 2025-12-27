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
    , compileSetVar
    , compileDefineFun
    , compileDefineLambda
    , getLambdaFreeVariables
    ) where

import Data.Text (Text, pack)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (zipWithM_)

import Compiler.ASM.CompilerMonad
import Compiler.ASM.AstToAsm (astSymbolToAsm)
import Compiler.Instruction (Instruction(..))
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.CompilerState (ScopeType(..))
import AST.Ast (Ast(..))

-- | Analyzes an AST node to find free variables (variables used but not defined locally).
--
-- @args
--   - node: The AST node to analyze.
--
-- @details
--   Recursively traverses the AST to compute the set of free variables.
--   For a Lambda, free vars are (body_free_vars - parameters).
--   For a Define, the defined name is excluded from the body's free vars.
--   This function is critical for determining closure captures.
--
-- @return
--   A Set of Text representing the names of free variables.
--
getLambdaFreeVariables :: Ast -> Set Text
getLambdaFreeVariables (ASymbol s) = Set.singleton s
getLambdaFreeVariables (AInteger _) = Set.empty
getLambdaFreeVariables (ABool _) = Set.empty
getLambdaFreeVariables (ADefineLambda params body) =
    Set.difference (getLambdaFreeVariables body) (Set.fromList params)
getLambdaFreeVariables (ASetVar name _ body) =
    Set.delete name (getLambdaFreeVariables body)
getLambdaFreeVariables (ACall func args) =
    Set.union (getLambdaFreeVariables func) (
        Set.unions (map getLambdaFreeVariables args))
getLambdaFreeVariables (AIf cond t e) =
    Set.unions [getLambdaFreeVariables cond, getLambdaFreeVariables t,
        getLambdaFreeVariables e]
getLambdaFreeVariables (AList e) = Set.unions (map getLambdaFreeVariables e)
getLambdaFreeVariables _ = Set.empty

-- | Compiles a conditional expression (If-Then-Else).
--
-- @args
--   - compileFn: The recursive compilation function.
--   - cond: The condition AST.
--   - thenBranch: The AST for the True branch.
--   - elseBranch: The AST for the False branch.
--
-- @details
--   Generates labels for the Else block and the End. Emits JumpIfFalse to
--   skip the Then block if appropriate, and an unconditional Jump to skip
--   the Else block after executing Then.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
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
--   Compiles the body to push the value onto the stack, allocates a new
--   global symbol index, and emits a StoreGlobal instruction.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileSetVar :: (Ast -> CompilerMonad ()) -> Text -> Ast -> CompilerMonad ()
compileSetVar compileFn name body = do
    compileFn body
    idx <- defineSymbol name
    emitInstruction (StoreGlobal idx)

-- | Compiles a named function definition.
--
-- @args
--   - compileFn: The recursive compilation function.
--   - name: The function name.
--   - args: The list of parameter names.
--   - body: The function body AST.
--
-- @details
--   Generates a label for the function and compiles the body within an
--   isolated function scope. Arguments are registered as 'ScopeLocal'
--   symbols (indices 0 to N-1). The compiled code is stored in 'csFuncs'.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileDefineFun :: (Ast -> CompilerMonad ()) -> Text -> [Text] -> Ast -> CompilerMonad ()
compileDefineFun compileFn name args body = do
    ulabel <- generateUniqueLabel (pack "fun_" <> name)
    compileInIsolatedFunctionScope $ do
        emitLabelDefinition ulabel
        zipWithM_ (\arg idx -> registerSymbol arg ScopeLocal idx) args [0..]
        compileFn body
        emitInstruction Ret
    return ()

-- | Compiles a Lambda (anonymous function/closure).
--
-- @args
--   - compileFn: The recursive compilation function.
--   - params: The list of parameter names.
--   - body: The function body AST.
--
-- @details
--   Identifies free variables (captures), pushes their current values onto
--   the stack, and emits a 'MakeClosure' instruction pointing to a new label.
--   The body is compiled in an isolated scope where captures are registered
--   as 'ScopeCapture' and parameters as 'ScopeLocal'.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileDefineLambda :: (Ast -> CompilerMonad ()) -> [Text] -> Ast ->
    CompilerMonad ()
compileDefineLambda compileFn params body = do
    let fvars = Set.toList (getLambdaFreeVariables (ADefineLambda params body))
    mapM_ astSymbolToAsm fvars
    ulabel <- generateUniqueLabel (pack "lambda")
    compileInIsolatedFunctionScope $ do
        emitLabelDefinition ulabel
        zipWithM_ (\p i -> registerSymbol p ScopeLocal i) params [0..]
        zipWithM_ (\c i -> registerSymbol c ScopeCapture i) fvars [0..]
        compileFn body
        emitInstruction Ret
    appendPseudoInstruction (MakeClosureLabel ulabel (length fvars))
