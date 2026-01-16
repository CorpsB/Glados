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
    ( compileAst
    , compileIf
    , compileFor
    , compileWhile
    , compileSetVar
    , compileSetStruct
    , compileDefineFun
    , compileDefineLambda
    , compileDefineStruct
    , compileTail
    , compileLoop
    , compileAccessStruct
    , getLambdaFreeVariables
    , inferType
    ) where

import Data.Text (Text, pack)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad (zipWithM_, forM_)
import Control.Monad.State (lift)

import Compiler.ASM.CompilerMonad
import Compiler.ASM.AstToAsm
    ( builtinMap
    , astSymbolToAsm
    , astIntToAsm
    , astBoolToAsm
    , astListToAsm
    , astCallToAsm
    )
import Compiler.Instruction (Instruction(..))
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.CompilerState (ScopeType(..))
import Common.Utils.List (zipWith3M_)
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
--   Built-in operators (like +, -, *) are excluded from free variables.
--
-- @return
--   A Set of Text representing the names of free variables.
--
getLambdaFreeVariables :: Ast -> Set Text
getLambdaFreeVariables (ASymbol s)
    | Map.member s builtinMap = Set.empty
    | otherwise = Set.singleton s
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

-- | Helper function for Tail Call Optimization
--
-- @args
--   - compileFn: The standard compilation function for non-tail expressions.
--   - ast: The AST node to compile in tail position.
--
-- @details
--   Detects if the expression is a function call. If so, emits 'TailCallLabel'
--   (TCO). If it is a control structure (If, List), it propagates the tail
--   context recursively. Otherwise, compiles normally and emits 'Ret'.
--
compileTail :: (Ast -> CompilerMonad ()) -> Ast -> CompilerMonad ()
compileTail compileFn (ACall func args) = case func of
    ASymbol name -> case Map.lookup name builtinMap of
        Just instr -> mapM_ compileFn args >>
            emitInstruction instr >> emitInstruction Ret
        Nothing -> mapM_ compileFn args >>
            appendPseudoInstruction (TailCallLabel name)
    _ -> compileFn func >> mapM_ compileFn args >>
        emitInstruction CallIndirect >> emitInstruction Ret
compileTail compileFn (AIf cond t e) = do
    lElse <- generateUniqueLabel (pack "else")
    lEnd  <- generateUniqueLabel (pack "endif")
    compileFn cond
    emitJumpIfFalseToLabel lElse
    compileTail compileFn t
    emitJumpToLabel lEnd
    emitLabelDefinition lElse
    compileTail compileFn e
    emitLabelDefinition lEnd
compileTail compileFn (AList exprs)
    | null exprs = emitInstruction Ret
    | otherwise = mapM_ compileFn (init exprs) >>
        compileTail compileFn (last exprs)
compileTail compileFn other = compileFn other >> emitInstruction Ret

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

-- | Helper function to compile the common logic of loops.
--
-- @args
--   - compileFn: The recursive compilation function.
--   - cond: The condition AST.
--   - body: The body AST.
--   - lEnd: The label to jump to if the condition is false.
--
-- @details
--   Compiles the condition, emits the jump-if-false check, and compiles the body.
--   This sequence is shared between While and For loops.
--
compileLoop :: (Ast -> CompilerMonad ()) -> Ast -> Ast -> Text -> CompilerMonad ()
compileLoop compileFn cond body lEnd =
    compileFn cond >>
    emitJumpIfFalseToLabel lEnd >>
    compileFn body

-- | Compiles a While loop.
--
-- @args
--   - compileFn: The recursive compilation function.
--   - cond: The loop condition AST.
--   - body: The loop body AST.
--
-- @details
--   Generates start and end labels. Uses 'compileLoop' for the core logic
--   and handles the looping jump back to start.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileWhile :: (Ast -> CompilerMonad ()) -> Ast -> Ast -> CompilerMonad ()
compileWhile compileFn cond body = do
    lStart <- generateUniqueLabel (pack "while_start")
    lEnd   <- generateUniqueLabel (pack "while_end")
    emitLabelDefinition lStart
    compileLoop compileFn cond body lEnd
    emitJumpToLabel lStart
    emitLabelDefinition lEnd

-- | Compiles a For loop.
--
-- @args
--   - compileFn: The recursive compilation function.
--   - initAst: The initialization AST (executed once).
--   - cond: The loop condition AST.
--   - body: The loop body AST.
--   - updateAst: The update/increment AST (executed after each iteration).
--
-- @details
--   Compiles the initialization step first. Then uses 'compileLoop' for the
--   condition and body. Finally, compiles the update step before jumping back.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileFor :: (Ast -> CompilerMonad ()) -> Ast -> Ast -> Ast -> Ast ->
    CompilerMonad ()
compileFor compileFn initAst cond body updateAst = do
    lStart <- generateUniqueLabel (pack "for_start")
    lEnd   <- generateUniqueLabel (pack "for_end")
    compileFn initAst
    emitLabelDefinition lStart
    compileLoop compileFn cond body lEnd
    compileFn updateAst
    emitJumpToLabel lStart
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
compileSetVar :: (Ast -> CompilerMonad ()) -> Text -> Text -> Ast -> CompilerMonad ()
compileSetVar compileFn name typeName body = do
    compileFn body
    (scope, idx) <- defineSymbol name typeName
    case scope of
        ScopeGlobal  -> emitInstruction (StoreGlobal idx)
        ScopeLocal   -> emitInstruction (StoreLocal idx)
        ScopeCapture -> emitInstruction (StoreCapture idx)

-- | Compiles a structure instantiation.
--
-- @args
--   - compileFn: The recursive compilation function.
--   - name: The name of the structure to instantiate.
--   - assignedFields: The list of (Field Name, Value AST) pairs.
--
-- @details
--   Retrieves the structure definition from 'CompilerState' using 'getStructDefinition'.
--   Reorders the provided values to match the definition order, compiles them
--   (pushing to stack), and emits a 'BuildStruct' instruction.
--   Returns an error if the struct is undefined or fields mismatch.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileSetStruct :: (Ast -> CompilerMonad ()) -> Text -> [(Text, Ast)] ->
    CompilerMonad ()
compileSetStruct compileFn name assignedFields = do
    defFields <- getStructDefinition name
    forM_ defFields $ \(fName, _fType) ->
        case Map.lookup fName (Map.fromList assignedFields) of
            Just valAst -> compileFn valAst
            Nothing -> lift $ Left (pack (
                "Missing field in struct instantiation: " ++ show fName))
    emitInstruction (BuildStruct (length defFields))

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
compileDefineFun :: (Ast -> CompilerMonad ()) -> Text -> [(Text, Text)] ->
    Ast -> CompilerMonad ()
compileDefineFun compileFn name args body = do
    ulabel <- generateUniqueLabel (pack "fun_" <> name)
    compileInIsolatedFunctionScope $
        emitLabelDefinition ulabel >>
        zipWithM_ (\(argName, argType) idx -> 
            registerSymbol argName argType ScopeLocal idx) args [0..] >>
        compileFn body >>
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
    capTypes <- mapM getSymbolType fvars
    mapM_ astSymbolToAsm fvars; ulabel <- generateUniqueLabel (pack "lambda")
    compileInIsolatedFunctionScope $ emitLabelDefinition ulabel >>
        zipWith3M_ (\name tName i ->
            registerSymbol name tName ScopeCapture i) fvars capTypes [0..] >>
        zipWithM_ (\pName i -> registerSymbol pName (pack "auto") ScopeLocal (
            (length fvars) + i)) params [0..] >> compileTail compileFn body
    appendPseudoInstruction (MakeClosureLabel ulabel (length fvars))

-- | Registers a structure definition.
--
-- @args
--   - name: The name of the structure.
--   - fields: The list of (Field Name, Field Type) pairs.
--
-- @details
--   Updates the 'CompilerState' to store the list of field names in order.
--   No bytecode is emitted for a structure definition (it is a compile-time
--   metadata operation).
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileDefineStruct :: Text -> [(Text, Text)] -> CompilerMonad ()
compileDefineStruct name fields = defineStruct name fields

-- | Infers the type of an AST expression.
--
-- @details
--   - ASymbol: Look up the variable in the symbol table.
--   - AAccessStruct: Recursively infer the parent object's type,
--   then look up the field's type.
--
inferType :: Ast -> CompilerMonad Text
inferType (ASymbol name) = getSymbolType name
inferType (AAccessStruct obj field) = do
    parentType <- inferType obj
    (_, fieldType) <- getStructField parentType field
    return fieldType
inferType _ = lift $ Left (pack
    "Cannot infer type of expression (too complex AST).")

-- | Compiles a struct field access.
--
-- @details
--   1. Infers the type of the object being accessed (e.g., "Player").
--   2. Looks up the field index in the struct definition.
--   3. Emits the code for the object followed by GET_STRUCT_FIELD.
--
compileAccessStruct :: (Ast -> CompilerMonad ()) -> Ast -> Text -> CompilerMonad ()
compileAccessStruct compileFn obj fieldName = do
    structType <- inferType obj
    (idx, _) <- getStructField structType fieldName
    compileFn obj
    emitInstruction (GetStructField idx)

-- | Main Dispatcher Function: Compiles any AST node.
--
-- @args
--   - ast: The AST node to compile.
--
-- @details
--   This is the central router of the compiler. It pattern-matches on the AST
--   constructors and delegates to the appropriate specific compilation function.
--   It passes itself ('compileAst') recursively to handle nested expressions.
--
-- @return
--   Unit value wrapped in 'CompilerMonad'.
--
compileAst :: Ast -> CompilerMonad ()
compileAst (APos _ _ ast) = compileAst ast
compileAst (ABlock xs) = mapM_ compileAst xs
compileAst (AInteger i) = astIntToAsm i
compileAst (ABool b) = astBoolToAsm b
compileAst (ASymbol s) = astSymbolToAsm s
compileAst AVoid = return ()
compileAst (AList xs) = astListToAsm compileAst xs
compileAst (ADefineFunc name args _ body) =
    compileDefineFun compileAst name args body
compileAst (ADefineLambda params body) =
    compileDefineLambda compileAst params body
compileAst (ADefineStruct name fields) = compileDefineStruct name fields
compileAst (ASetVar name typeName body) =
    compileSetVar compileAst name typeName body
compileAst (ASetStruct name fields) = compileSetStruct compileAst name fields
compileAst (AAccessStruct obj field) = compileAccessStruct compileAst obj field
compileAst (AIf cond t f) = compileIf compileAst cond t f
compileAst (AWhile cond body) = compileWhile compileAst cond body
compileAst (AFor i cond body u) = compileFor compileAst i cond body u
compileAst (ACall func args) = astCallToAsm compileAst func args
compileAst (AReturn expr) = compileAst expr >> emitInstruction Ret
compileAst (AImport _) = return ()
