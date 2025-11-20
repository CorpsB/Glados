{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main.hs
-}

module Main where

import qualified Coucou
import qualified Parser.Coucou as Parser
import qualified Eval.Coucou   as Eval

main :: IO ()
main = do
    putStrLn "=== Test de compilation GLaDOS ==="
    Coucou.sayRoot
    Parser.sayParser
    Eval.sayEval
