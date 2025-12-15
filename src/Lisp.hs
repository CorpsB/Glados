{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Lisp
-}

module Lisp (SExpr(..), getSymbol, getInteger, getList) where

import qualified Data.Text as DT

data SExpr = SInteger Int
    | SSymbol DT.Text
    | List [SExpr]
    deriving Show

getSymbol :: SExpr -> Maybe DT.Text
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInteger i) = Just i
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List l) = Just l
getList _ = Nothing
