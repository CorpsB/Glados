{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- List
-}

module Utils.List (sameLength, astToList, listToAst) where

import Ast (Ast(..))
import qualified Data.Text as DT

sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

astToList :: Ast -> Either DT.Text [Ast]
astToList (AList xs) = Right xs
astToList other = Left $ DT.pack $ "*** ERROR: Expected list, got: " ++ show other

listToAst :: [Ast] -> Ast
listToAst = AList
