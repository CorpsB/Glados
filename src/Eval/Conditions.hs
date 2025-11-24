{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Conditions.hs
-}

module Eval.Conditions (execCondition) where

import Ast (Ast(..))

execCondition :: Maybe Ast -> Maybe Ast -> Maybe Ast -> Maybe Ast
execCondition (Just (ABool True)) th _ = th
execCondition (Just (ABool False)) _ el = el
execCondition (Just (AInteger 1)) th _ = th
execCondition (Just (AInteger 0)) _ el = el
execCondition _ _ _ = Nothing
