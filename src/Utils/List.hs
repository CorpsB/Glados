{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- List
-}

{-|
Module      : Utils.List
Description : Utility functions for list manipulation and AST list conversion.
Stability   : experimental

This module provides small helper functions related to list handling,
especially conversions between generic Haskell lists and the language's
AST representation of lists.

These utilities are typically used during evaluation and type checking
to validate and manipulate list-based expressions.
-}
module Utils.List (sameLength, astToList, listToAst) where

import Ast (Ast(..))
import qualified Data.Text as DT

-- | Check whether two lists have the same length.
--
-- This function traverses both lists simultaneously and returns:
--
-- * 'True' if both lists end at the same time
-- * 'False' otherwise
--
-- The actual contents of the lists are ignored.
sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

-- | Extract the underlying list of AST nodes from a list expression.
--
-- If the given 'Ast' value is an 'AList', its contents are returned.
-- Otherwise, an error is produced indicating that a list was expected.
--
-- This is typically used to validate arguments to list-based primitives.
astToList :: Ast -> Either DT.Text [Ast]
astToList (AList xs) = Right xs
astToList other = Left $ DT.pack $
    "*** ERROR: Expected list, got: " ++ show other

-- | Convert a list of AST nodes into a list expression.
--
-- This is a simple wrapper around the 'AList' constructor and serves
-- as the inverse operation of 'astToList'.
listToAst :: [Ast] -> Ast
listToAst = AList
