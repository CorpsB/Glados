{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- LispToAst
-}

{-|
Module      : Parser.Ast
Description : Conversion from S-Expressions (Lisp) to AST.
Stability   : stable

This module acts as a bridge between the S-Expression parser (ParserISL)
and the internal AST used by the evaluator. It transforms raw S-Expressions
into structured AST nodes.
-}
module Parser.Ast (sexprToAST) where

import Ast (Ast(..))
import Lisp (SExpr(..))
import Type.Integer (fitInteger)
import qualified Data.Text as DT

-- | Extract a parameter name from an S-Expression.
--
-- Returns the name if the S-Expression is a symbol, otherwise returns an error.
-- Used for parsing function parameters.
extractParam :: SExpr -> Either DT.Text DT.Text
extractParam (SSymbol s) = Right s
extractParam _           = Left $ DT.pack
    "*** ERROR: Parameters must be symbols"

-- | Convert an S-Expression into an AST.
--
-- Handles recursively:
-- * Integers and Symbols
-- * Booleans (#t, #f)
-- * Lambdas (lambda (args) body)
-- * Definitions (define name val or define (func args) body)
-- * Conditions (if cond then else)
-- * Function calls ((func arg1 arg2))
sexprToAST :: SExpr -> Either DT.Text Ast
sexprToAST (SInteger n) = Right $ AInteger (fitInteger n)
sexprToAST (SSymbol s)
    | s == DT.pack "#t" = Right $ ABool True
    | s == DT.pack "#f" = Right $ ABool False
    | otherwise = Right $ ASymbol s

-- Lambda conversion
sexprToAST (List (SSymbol lmb : List params : body : []))
    | lmb == DT.pack "lambda" = do
        ps <- mapM extractParam params
        b  <- sexprToAST body
        Right $ Lambda ps b
sexprToAST (List (SSymbol lmb : _))
    | lmb == DT.pack "lambda" = Left $ DT.pack
        "*** ERROR: Invalid 'lambda' expression"

-- Variable definition conversion
sexprToAST (List (SSymbol def : SSymbol name : body : []))
    | def == DT.pack "define" = do
        b <- sexprToAST body
        Right $ Define name (DT.pack "undefined") b

-- Function definition conversion
sexprToAST (List (SSymbol def : List (SSymbol name : params) : body : []))
    | def == DT.pack "define" = do
        ps <- mapM extractParam params
        b  <- sexprToAST body
        let argsTyped = map (\p -> (p, DT.pack "Any")) ps
        Right $ DefineFun name argsTyped (DT.pack "Any") b
sexprToAST (List (SSymbol def : _))
    | def == DT.pack "define" = Left $ DT.pack
        "*** ERROR: Invalid 'define' expression"

-- Condition conversion
sexprToAST (List (SSymbol i : cond : th : el : []))
    | i == DT.pack "if" = do
        c <- sexprToAST cond
        t <- sexprToAST th
        e <- sexprToAST el
        Right $ Condition c t e

-- Function call conversion
sexprToAST (List (h:q)) = do
    h2 <- sexprToAST h
    q2 <- mapM sexprToAST q
    Right $ Call h2 q2
sexprToAST other = Left $ DT.pack $
    "*** ERROR: Invalid expressions: " ++ show other
