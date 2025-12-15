{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AST
-}

{-|
Module      : Eval.Ast
Description : AST evaluator for the Glados interpreter.
Copyright   : (c) EPITECH, 2025
License     : Proprietary (EPITECH)
Maintainer  : (unknown)
Stability   : experimental
Portability : POSIX

This module implements evaluation of the language AST used by the Glados project.

It supports:
* literal evaluation (integers, booleans, lists, void),
* symbol resolution through an environment ('Env'),
* lambda creation as closures (capturing the current environment),
* conditional evaluation via 'Eval.Conditions',
* function calls:
    * builtins via 'Eval.Builtins',
    * user-defined functions via a 'FuncTable',
    * closures stored in the environment.

Two entry points are provided:
* 'evalAST' evaluates an expression with an explicit environment.
* 'evalASTEnv' is a helper used to evaluate arguments under a given environment; it delegates
  to 'evalAST' for non-trivial forms to keep behaviour consistent.

Note: 'Import' is intentionally rejected in interpreter mode.
-}
module Eval.Ast (evalAST, evalASTEnv) where

import Ast (Ast(..), Env)
import Eval.Builtins (execBuiltin)
import Eval.Conditions (execCondition)
import Eval.Functions (getFunction, FuncTable)
import Utils.List (sameLength)
import qualified Data.Text as DT

{-|
Apply a closure to a list of already-evaluated arguments.

A closure carries:
* the parameter list,
* the function body,
* the captured environment at lambda definition time.

On application, the captured environment is extended with a new binding layer
created from @(params, args)@, then the body is evaluated in that extended
environment.

Fails with an error if the number of parameters does not match the number of
arguments.
-}
applyClosure :: FuncTable -> Env -> [DT.Text] -> Ast -> [Ast] -> Either DT.Text Ast
applyClosure ftable cEnv params body args
    | sameLength params args =
        evalASTEnv ftable (zip params args ++ cEnv) body
    | otherwise = Left $ DT.pack
        "*** ERROR: Argument length mismatch in lambda call"

{-|
Execute a user-defined function stored in the function table.

The function is looked up by name using 'getFunction'. If found, and the arity
matches, the function body is evaluated in an environment extended with the
argument bindings.

If the function is not found, the provided fallback error message is returned.
If the arity does not match, an explicit arity-mismatch error is returned.
-}
execUserFunc :: FuncTable -> Env -> DT.Text -> [Ast] -> DT.Text -> Either DT.Text Ast
execUserFunc ft env op args err = case getFunction ft op of
    Just (p, b) | sameLength p args ->
        evalASTEnv ft (zip p args ++ env) b
    Just _ -> Left $ DT.pack
        "*** ERROR: Argument length mismatch for function " <> op
    Nothing -> Left err

{-|
Execute a named call where the callee is a symbol (e.g. @(foo 1 2)@).

Resolution order:
1. Try a builtin via 'execBuiltin'.
2. If the builtin reports an unknown-builtin error, try resolving the symbol in
   the environment as a closure.
3. Otherwise, try resolving it as a user-defined function in the 'FuncTable'.

Any other builtin failure is propagated as-is.
-}
execNamedCall :: FuncTable -> Env -> DT.Text -> [Ast] -> Either DT.Text Ast
execNamedCall ft env op args = case execBuiltin op args of
    Right r -> Right r
    Left builtinErr ->
        if DT.isPrefixOf (DT.pack "*** ERROR: Unknown builtin:") builtinErr
        then
            case lookupEnv env op of
                Just (Closure p b cEnv) -> applyClosure ft cEnv p b args
                _ -> execUserFunc ft env op args (DT.pack
                    "*** ERROR: Unknown func: " <> op)
        else Left builtinErr

{-|
Execute a call where the callee itself is an expression.

The callee expression is evaluated first; it must produce a 'Closure'.
If it does not, evaluation fails with an error indicating a non-callable value.
-}
execExprCall :: FuncTable -> Env -> Ast -> [Ast] -> Either DT.Text Ast
execExprCall ft env func args = evalAST ft env func >>= \res ->
    case res of
        Closure p b cEnv -> applyClosure ft cEnv p b args
        _ -> Left $ DT.pack "*** ERROR: Attempt to call a non-function"

{-|
Evaluate an 'Ast' node in the context of:
* a function table ('FuncTable') for named user functions,
* an environment ('Env') for symbol bindings and closures.

Behaviour by node:
* Literals evaluate to themselves.
* 'ASymbol' is resolved via 'lookupEnv'.
* 'Lambda' yields a 'Closure' that captures the current environment.
* 'Define' evaluates the RHS and returns a definition node containing the
  evaluated RHS (the caller is expected to integrate it into the environment).
* 'DefineFun' is returned as-is (function table integration is handled elsewhere).
* 'Condition' evaluates the condition, selects a branch via 'execCondition',
  then evaluates the chosen branch.
* 'Call' evaluates arguments (left-to-right via 'traverse'), then dispatches to:
    * 'execNamedCall' when the callee is a symbol,
    * 'execExprCall' otherwise.
* 'Import' is rejected in interpreter mode.
-}
evalAST :: FuncTable -> Env -> Ast -> Either DT.Text Ast
evalAST _ _ (AInteger n) = Right $ AInteger n
evalAST _ _ (ABool b) = Right $ ABool b
evalAST _ _ (AList l) = Right $ AList l
evalAST _ _ AVoid = Right AVoid
evalAST _ env (ASymbol s) = case lookupEnv env s of
    Just v -> Right v
    Nothing -> Left $ DT.pack "*** ERROR: Undefined symbol: " <> s
evalAST _ env (Lambda params body) = Right $ Closure params body env
evalAST _ _ (Closure p b e) = Right $ Closure p b e
evalAST ftable env (Define name body) = do
    b2 <- evalAST ftable env body
    Right $ Define name b2
evalAST _ _ (DefineFun name params body) = Right $ DefineFun name params body
evalAST ftable env (Condition cond th el) = do
    c <- evalAST ftable env cond
    chosen <- execCondition c th el
    evalAST ftable env chosen
evalAST ftable env (Call func args) = do
    evalArgs <- traverse (evalASTEnv ftable env) args
    case func of
        ASymbol op -> execNamedCall ftable env op evalArgs
        _ -> execExprCall ftable env func evalArgs
evalAST _ _ (Import _) = Left $ DT.pack
    "*** ERROR: 'Import' is not supported in interpreter mode"

{-|
Lookup a symbol in an environment.

The environment is represented as an association list. The first matching key
wins (shadowing).
-}
lookupEnv :: Env -> DT.Text -> Maybe Ast
lookupEnv [] _ = Nothing
lookupEnv ((k,v):xs) key
    | k == key  = Just v
    | otherwise = lookupEnv xs key

{-|
Helper evaluator used to evaluate nodes under a given environment.

For simple literal forms and symbol/lambda creation, it performs direct handling.
For compound forms ('Define', 'DefineFun', 'Call', 'Condition', 'Import'), it
delegates to 'evalAST' to ensure a single source of truth for semantics.
-}
evalASTEnv :: FuncTable -> Env -> Ast -> Either DT.Text Ast
evalASTEnv _ _ (AInteger n) = Right $ AInteger n
evalASTEnv _ _ (ABool b) = Right $ ABool b
evalASTEnv _ _ (AList l) = Right $ AList l
evalASTEnv _ _ AVoid = Right AVoid
evalASTEnv _ env (ASymbol s) = case lookupEnv env s of
    Just v -> Right v
    Nothing -> Left $ DT.pack "*** ERROR: Undefined symbol: " <> s
evalASTEnv _ env (Lambda params body) = Right $ Closure params body env
evalASTEnv _ _ (Closure p b e) = Right $ Closure p b e
evalASTEnv ftable env (Define name body) = evalAST ftable env
    (Define name body)
evalASTEnv ftable env (DefineFun name params body) =
    evalAST ftable env (DefineFun name params body)
evalASTEnv ftable env (Call f args) = evalAST ftable env (Call f args)
evalASTEnv ftable env (Condition c t e) = evalAST ftable env (Condition c t e)
evalASTEnv ftable env (Import i) = evalAST ftable env (Import i)
