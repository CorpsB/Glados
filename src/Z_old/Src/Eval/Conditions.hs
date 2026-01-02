{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Conditions.hs
-}

module Z_old.Src.Eval.Conditions (execCondition) where

import Z_old.Src.Ast (OldAst(..))
import Z_old.Src.Type.Integer (intValueEq)
import qualified Data.Text as DT

execCondition :: OldAst -> OldAst -> OldAst -> Either DT.Text OldAst
execCondition (ABool True) th _ = Right th
execCondition (ABool False) _ el = Right el
execCondition (AInteger i) _ el | intValueEq i 0 = Right el
execCondition (AInteger _) th _ = Right th
execCondition cond _ _ = Left $ DT.pack $
    "*** ERROR: Invalid condition: " ++ show cond
