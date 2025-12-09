{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Conditions.hs
-}

module Eval.Conditions (execCondition) where

import Ast (Ast(..))
import Type.Integer (intValueEq)
import qualified Data.Text as DT

execCondition :: Ast -> Ast -> Ast -> Either DT.Text Ast
execCondition (ABool True) th _ = Right th
execCondition (ABool False) _ el = Right el
execCondition (AInteger i) _ el | intValueEq i 0 = Right el
execCondition (AInteger _) th _ = Right th
execCondition cond _ _ = Left $ DT.pack $
    "*** ERROR: Invalid condition: " ++ show cond
