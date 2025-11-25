{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Conditions.hs
-}

module Eval.Conditions (execCondition) where

import Ast (Ast(..))

execCondition :: Maybe Ast -> Maybe Ast -> Maybe Ast -> Either String Ast
execCondition (Just (ABool True)) (Just th) _ = Right th
execCondition (Just (ABool False)) _ (Just el) = Right el
execCondition (Just (AInteger 1)) (Just th) _ = Right th
execCondition (Just (AInteger 0)) _ (Just el) = Right el
execCondition Nothing _ _ = Left "*** ERROR: Condition is missing"
execCondition (Just cond) _ _ = Left $ "*** ERROR: Invalid " ++
    "condition: " ++ show cond
