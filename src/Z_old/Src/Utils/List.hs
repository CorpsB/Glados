{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- List
-}

module Z_old.Src.Utils.List (sameLength, astToList, listToAst) where

import Z_old.Src.Ast (OldAst(..))
import qualified Data.Text as DT

sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

astToList :: OldAst -> Either DT.Text [OldAst]
astToList (AList xs) = Right xs
astToList other = Left $ DT.pack $
    "*** ERROR: Expected list, got: " ++ show other

listToAst :: [OldAst] -> OldAst
listToAst = AList
