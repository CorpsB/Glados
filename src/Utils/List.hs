{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- List
-}

module Utils.List (sameLength) where

sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False
