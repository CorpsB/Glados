{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Conditions.hs
-}

module Eval.Conditions (execCondition) where

import Ast (Ast(..))
import Type.Integer (intValueEq)

execCondition :: Ast -> Ast -> Ast -> Either String Ast
execCondition (ABool True) th _ = Right th
execCondition (ABool False) _ el = Right el
execCondition (AInteger i) _ el | intValueEq i 0 = Right el
execCondition (AInteger _) th _ = Right th
execCondition cond _ _ = Left $ "*** ERROR: Invalid condition: " ++ show cond
