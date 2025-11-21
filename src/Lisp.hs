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

instance Eq SExpr where
    (SInteger a) == (SInteger b) = a == b
    (SSymbol a)  == (SSymbol b)  = a == b
    (List a)     == (List b)     = a == b
    _ == _ = False

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInteger i) = Just i
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List l) = Just l
getList _ = Nothing
