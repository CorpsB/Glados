{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- List
-}

module Common.Utils.List (listEq) where

listEq :: [a] -> [b] -> Bool
listEq [] [] = True
listEq (_:xs) (_:ys) = listEq xs ys
listEq _ _ = False
