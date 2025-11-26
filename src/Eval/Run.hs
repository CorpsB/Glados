{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Run
-}

module Eval.Run (processSExpr) where

import Ast (Ast(..), Env)
import Lisp (SExpr)
import Parser.Ast (sexprToAST)
import Eval.Functions (FuncTable, registerFunction, callFunction)
import Eval.Ast (evalAST, evalASTEnv)

evalBridge :: FuncTable -> Env -> Ast -> Maybe Ast
evalBridge ft en a = case evalASTEnv ft en a of
    Right r -> Just r
    Left _  -> Nothing

astFromSexpr :: SExpr -> Either String Ast
astFromSexpr sexpr = case sexprToAST sexpr of
    Just a  -> Right a
    Nothing -> Left $ "Syntax error: could not convert SExpr to AST: " ++
        show sexpr

processDefine :: FuncTable -> Env -> Ast ->
    Either String (FuncTable, Env, Maybe Ast)
processDefine ftable env (Define name body) = do
    val <- evalAST ftable env body
    Right (ftable, (name, val) : env, Nothing)
processDefine ftable env (DefineFun name params body) =
    case registerFunction ftable name params body of
        Left err        -> Left err
        Right up_ftable -> Right (up_ftable, env, Nothing)
processDefine _ _ _ = Left "processDefine called with non-define AST"

processCallOrEval :: FuncTable -> Env -> Ast ->
    Either String (FuncTable, Env, Maybe Ast)
processCallOrEval ftable env (Call (ASymbol name) args) =
    case callFunction evalBridge ftable env name args of
        Left err  -> Left err
        Right res -> Right (ftable, env, Just res)
processCallOrEval ftable env other =
    case evalAST ftable env other of
        Left err -> Left err
        Right r  -> Right (ftable, env, Just r)

processSExpr :: FuncTable -> Env -> SExpr ->
    Either String (FuncTable, Env, Maybe Ast)
processSExpr ftable env sexpr = do
    ast <- astFromSexpr sexpr
    case ast of
        Define{}    -> processDefine ftable env ast
        DefineFun{} -> processDefine ftable env ast
        _           -> processCallOrEval ftable env ast
