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

import Data.Char (chr)
import Data.Text (Text, pack, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad (zipWithM_, forM_)
import Control.Monad.State (lift, get, modify)

import Compiler.ASM.CompilerMonad
import Compiler.ASM.AstToAsm
    ( builtinMap
    , astSymbolToAsm
    , astIntToAsm
    , astBoolToAsm
    , astListToAsm
    , astCallToAsm
    )
import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.CompilerState (CompilerState(..), ScopeType(..))
import Compiler.ASM.Builtins (getBuiltinReturnType)
import Common.Type.Integer (IntValue(..))
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
compileTail compileFn (ACall (ASymbol name) args) =
    case Map.lookup name builtinMap of
        Just instr -> do
            mapM_ compileFn args
            emitInstruction instr
            s <- get
            emitInstruction (Ret (csCurrentArgCount s))
        Nothing -> mapM_ compileFn args >>
            appendPseudoInstruction (TailCallLabel name)
compileTail compileFn (ACall func args) = do
    compileFn func
    mapM_ compileFn args
    emitInstruction CallIndirect
    s <- get
    emitInstruction (Ret (csCurrentArgCount s))
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
    | null exprs = do
        s <- get
        emitInstruction (Ret (csCurrentArgCount s))
    | otherwise = mapM_ compileFn (init exprs) >>
        compileTail compileFn (last exprs)
compileTail compileFn other = do
    compileFn other
    s <- get
    emitInstruction (Ret (csCurrentArgCount s))

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
    pushLoopExit lEnd >>
    compileFn body >>
    popLoopExit

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
--   - updateAst: The update/increment AST (executed after each iteration).
--   - body: The loop body AST.
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
compileFor compileFn initAst cond updateAst body = do
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
compileSetVar :: (Ast -> CompilerMonad ()) -> Text -> Text -> Ast ->
    Bool -> CompilerMonad ()
compileSetVar compileFn name typeName body isStatement = do
    compileFn body
    (scope, idx) <- defineSymbol name typeName
    case not isStatement of
        True -> emitInstruction Dup
        False -> return ()
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
compileDefineFun compileFn name args body = let nArgs = length args in
    compileInIsolatedFunctionScope (
        modify (\s -> s { csCurrentArgCount = nArgs }) >>
        emitLabelDefinition (pack "fun_" <> name) >>
        zipWithM_ (\(aName, aType) i ->
            registerSymbol aName aType ScopeLocal (i - nArgs)) args [0..] >>
        compileFn body >>
        emitInstruction (Push (ImmInt (I64 0))) >>
        emitInstruction (Ret nArgs)
    ) >> return ()

-- | Registers lambda parameters with negative offsets.
--
-- @args
--   - params: The list of parameter names.
--
-- @details
--   Calculates indices relative to the Frame Pointer.
--   Arg 0 is at (0 - N), Last Arg is at -1.
--
registerLambdaParams :: [Text] -> CompilerMonad ()
registerLambdaParams params =
    let count = length params in
    zipWithM_ (\p i ->
        registerSymbol p (pack "auto") ScopeLocal (i - count)) params [0..]

-- | Internal helper to compile the isolated scope of a lambda.
--
-- @args
--   - compileFn: Recursion callback.
--   - params: Lambda parameters.
--   - body: Lambda body.
--   - label: The unique label for this function.
--   - fvars: List of captured variable names.
--   - types: List of captured variable types.
--
compileLambdaScope :: (Ast -> CompilerMonad ()) -> [Text] -> Ast -> Text ->
    [Text] -> [Text] -> CompilerMonad ()
compileLambdaScope compileFn params body label fvars types =
    emitLabelDefinition label >>
    zipWith3M_ (
        \n t i -> registerSymbol n t ScopeCapture i) fvars types [0..] >>
    registerLambdaParams params >>
    compileTail compileFn body

-- | Compiles a lambda definition.
--
-- @args
--   - compileFn: The main compilation function (recursion).
--   - params: List of parameter names.
--   - body: The body of the lambda.
--
-- @details
--   1. Identifies free variables (captures).
--   2. Emits code to push captures onto the stack.
--   3. Generates a unique label for the lambda body.
--   4. Compiles the body in an isolated scope.
--   5. Emits 'MakeClosure' to create the closure object at runtime.
--
compileDefineLambda :: (Ast -> CompilerMonad ()) -> [Text] -> Ast ->
    CompilerMonad ()
compileDefineLambda compileFn params body = do
    let ast = ADefineLambda params body
    let fvars = Set.toList (getLambdaFreeVariables ast)
    capTypes <- mapM getSymbolType fvars
    mapM_ astSymbolToAsm fvars
    ulabel <- generateUniqueLabel (pack "lambda")
    compileInIsolatedFunctionScope $
        compileLambdaScope compileFn params body ulabel fvars capTypes
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
inferType (APos _ _ ast) = inferType ast
inferType (ASymbol name) = getSymbolType name
inferType (AInteger _) = return (pack "int")
inferType (ABool _) = return (pack "bool")
inferType (ACall (ASymbol name) args) = do
    argTypes <- mapM inferType args
    case getBuiltinReturnType (unpack name) argTypes of
        Right builtinType -> return builtinType
        Left _ -> getSymbolType name
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

extractChar :: Ast -> CompilerMonad Char
extractChar (AInteger (IChar c)) = return (chr (fromIntegral c))
extractChar _ =
    lift $ Left (pack "attr_update: Field name must be a string of characters")

-- | Convert an AST list of IChar to Haskell Text
--
extractFieldName :: Ast -> CompilerMonad Text
extractFieldName (ASymbol s) = return s
extractFieldName (AList xs) = do
    chars <- mapM extractChar xs
    return (pack chars)
extractFieldName _ =
    lift $ Left (pack "attr_update: Second argument must be a Sym or String")

-- | Compiles the 'attr_update' builtin special form.
--
-- @args
--   - compileFn: Recursive compiler.
--   - obj: The AST for the structure object.
--   - fieldArg: The AST for the field name (must be a Symbol).
--   - v: The AST for the new value.
--
-- @details
--   1. Infers the type of 'obj' to find its struct definition.
--   2. Resolves 'fieldArg' to an integer index.
--   3. Pushes Obj, Index, and Value onto the stack.
--   4. Emits AttrUpdate.
--
compileAttrUpdate :: (Ast -> CompilerMonad ()) -> Ast -> Ast -> Ast ->
    CompilerMonad ()
compileAttrUpdate compileFn obj fieldArg v = do
    fieldName <- extractFieldName fieldArg
    structType <- inferType obj
    (idx, _) <- getStructField structType fieldName
    compileFn obj
    emitInstruction (Push (ImmInt (I64 (fromIntegral idx))))
    compileFn v
    emitInstruction AttrUpdate

-- | Pushes a loop exit label onto the stack.
--
pushLoopExit :: Text -> CompilerMonad ()
pushLoopExit label = modify $ \s -> s { csLoopExits = label : csLoopExits s }

-- | Pops the last loop exit label.
--
popLoopExit :: CompilerMonad ()
popLoopExit = modify $ \s -> case csLoopExits s of
    (_:xs) -> s { csLoopExits = xs }
    []     -> s

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
compileAst (AExprStmt (ASetVar name typeName body)) =
    compileSetVar compileAst name typeName body True
compileAst (ASetVar name typeName body) =
    compileSetVar compileAst name typeName body False
compileAst (ASetStruct name fields) = compileSetStruct compileAst name fields
compileAst (AAccessStruct obj field) = compileAccessStruct compileAst obj field
compileAst (AIf cond t f) = compileIf compileAst cond t f
compileAst (AWhile cond body) = compileWhile compileAst cond body
compileAst (AFor i cond u body) = compileFor compileAst i cond u body
compileAst (ACall (ASymbol name) [obj, fieldArg, v]) 
    | name == pack "attr_update" = compileAttrUpdate compileAst obj fieldArg v
compileAst (ACall func args) = astCallToAsm compileAst func args
compileAst ABreak = do
    state <- get
    case csLoopExits state of
        [] -> lift $ Left (pack "Error: 'break' statement outside of loop")
        (label:_) -> emitJumpToLabel label
compileAst (AReturn expr) = do
    compileAst expr
    s <- get
    emitInstruction (Ret (csCurrentArgCount s))
compileAst (AImport _) = return ()
compileAst (AExprStmt expr) = compileAst expr
