{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Run
-}

module Z_old.Src.Eval.Run (processSExpr, astFromSexpr, processDefine, processCallOrEval) where

import AST.Ast (Ast(..), Env)
import Z_old.Src.Lisp (SExpr)
import Z_old.Src.Parser.Ast (sexprToAST)
import Z_old.Src.Eval.Functions (FuncTable, registerFunction)
import Z_old.Src.Eval.Ast (evalAST)
import qualified Data.Text as DT

astFromSexpr :: SExpr -> Either DT.Text Ast
astFromSexpr sexpr = case sexprToAST sexpr of
    Right a  -> Right a
    Left err -> Left $ DT.pack $
        "Syntax error: could not convert SExpr to AST: " ++ DT.unpack err

processDefine :: FuncTable -> Env -> Ast ->
    Either DT.Text (FuncTable, Env, Maybe Ast)
processDefine ftable env (Define name _type body) = do
    val <- evalAST ftable env body
    case val of
        Closure params b cenv ->
            let recClosure = Closure params b ((name, recClosure) : cenv) in
            Right (ftable, (name, recClosure) : env, Nothing)
        _ -> Right (ftable, (name, val) : env, Nothing)
processDefine ftable env (DefineFun name argsWithType _ body) =
    let paramNames = map fst argsWithType 
    in case registerFunction ftable name paramNames body of
        Left err        -> Left err
        Right up_ftable -> Right (up_ftable, env, Nothing)
processDefine _ _ _ = Left $ DT.pack "processDefine called with non-define AST"

processCallOrEval :: FuncTable -> Env -> Ast ->
    Either DT.Text (FuncTable, Env, Maybe Ast)
processCallOrEval ftable env ast =
    case evalAST ftable env ast of
        Left err -> Left err
        Right r  -> Right (ftable, env, Just r)

processSExpr :: FuncTable -> Env -> SExpr ->
    Either DT.Text (FuncTable, Env, Maybe Ast)
processSExpr ftable env sexpr = do
    ast <- astFromSexpr sexpr
    case ast of
        Define{}    -> processDefine ftable env ast
        DefineFun{} -> processDefine ftable env ast
        _           -> processCallOrEval ftable env ast
