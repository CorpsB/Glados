{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- List
-}

module Common.Utils.List
    ( listEq
    , zipWith3M_
    ) where

import Control.Monad()

listEq :: [a] -> [b] -> Bool
listEq [] [] = True
listEq (_:xs) (_:ys) = listEq xs ys
listEq _ _ = False

zipWith3M_ :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWith3M_ f as bs cs = sequence_ (zipWith3 f as bs cs)
