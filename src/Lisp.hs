{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Lisp
-}

module Lisp (SExpr(..), getSymbol, getInteger, getList) where

data SExpr = SInteger Int
    | SSymbol String
    | List [SExpr]
    deriving Show

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInteger i) = Just i
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List l) = Just l
getList _ = Nothing
