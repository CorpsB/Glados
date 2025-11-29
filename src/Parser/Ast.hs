{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- LispToAst
-}

module Parser.Ast (sexprToAST) where

import Ast (Ast(..), IntValue(..))
import Lisp (SExpr(..))
import Data.Int (Int8, Int16, Int32)

extractParam :: SExpr -> Either String String
extractParam (SSymbol s) = Right s
extractParam _           = Left "*** ERROR: Parameters must be symbols"

fitInteger :: Int -> IntValue
fitInteger n
    | n >= fromIntegral (minBound :: Int8)
        && n <= fromIntegral (maxBound :: Int8)  = I8  (fromIntegral n)
    | n >= fromIntegral (minBound :: Int16)
        && n <= fromIntegral (maxBound :: Int16) = I16 (fromIntegral n)
    | n >= fromIntegral (minBound :: Int32)
        && n <= fromIntegral (maxBound :: Int32) = I32 (fromIntegral n)
    | otherwise = I64 (fromIntegral n)

sexprToAST :: SExpr -> Either String Ast
sexprToAST (SInteger n) = Right $ AInteger (fitInteger n)
sexprToAST (SSymbol "#t") = Right $ ABool True
sexprToAST (SSymbol "#f") = Right $ ABool False
sexprToAST (SSymbol s) = Right $ ASymbol s
sexprToAST (List (SSymbol "lambda" : List params : body : [])) = do
    ps <- mapM extractParam params
    b  <- sexprToAST body
    Right $ Lambda ps b
sexprToAST (List (SSymbol "lambda" : _)) =
    Left "*** ERROR: Invalid 'lambda' expression"
sexprToAST (List (SSymbol "define" : SSymbol name : body : [])) = do
    b <- sexprToAST body
    Right $ Define name b
sexprToAST (List (SSymbol "define" : List (SSymbol name : params) : body : [])) = do
    ps <- mapM extractParam params
    b  <- sexprToAST body
    Right $ DefineFun name ps b
sexprToAST (List (SSymbol "define" : _)) =
    Left "*** ERROR: Invalid 'define' expression"
sexprToAST (List (SSymbol "if" : cond : th : el : [])) = do
    c <- sexprToAST cond
    t <- sexprToAST th
    e <- sexprToAST el
    Right $ Condition c t e
sexprToAST (List (h:q)) = do
    h2 <- sexprToAST h
    q2 <- mapM sexprToAST q
    Right $ Call h2 q2
sexprToAST _ = Left "*** ERROR: Invalid expressions"
