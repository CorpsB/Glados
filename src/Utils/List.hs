{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- List
-}

module Utils.List (sameLength, astToList, listToAst) where

import Ast (Ast(..))

sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

astToList :: Ast -> Either String [Ast]
astToList (AList xs) = Right xs
astToList other = Left $ "*** ERROR: Expected list, got: " ++ show other

listToAst :: [Ast] -> Ast
listToAst = AList
