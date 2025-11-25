{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- LispToAst
-}

module Parser.Ast (sexprToAST) where

import Ast (Ast(..))
import Lisp (SExpr(..))

extractParam :: SExpr -> Maybe String
extractParam (SSymbol s) = Just s
extractParam _           = Nothing

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInteger n) = Just $ AInteger n
sexprToAST (SSymbol "#t") = Just $ ABool True
sexprToAST (SSymbol "#f") = Just $ ABool False
sexprToAST (SSymbol s) = Just $ ASymbol s
sexprToAST (List (SSymbol "lambda" : List params : body : [])) = do
    ps <- mapM extractParam params
    b  <- sexprToAST body
    return $ Lambda ps b
sexprToAST (List (SSymbol "define" : SSymbol name : body : [])) = do
    b <- sexprToAST body
    return $ Define name b
sexprToAST (List (SSymbol "define" : List (SSymbol name : params) : body : [])) = do
    ps <- mapM extractParam params
    b  <- sexprToAST body
    return $ DefineFun name ps b
sexprToAST (List (SSymbol "if" : cond : th : el : [])) = do
    c <- sexprToAST cond
    t <- sexprToAST th
    e <- sexprToAST el
    return $ Condition c t e
sexprToAST (List (h:q)) = do
    h2 <- sexprToAST h
    q2 <- mapM sexprToAST q
    return $ Call h2 q2
sexprToAST _ = Nothing
