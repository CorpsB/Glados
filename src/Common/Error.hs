{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Error Formatter
-}

{-|
Module      : Common.Error
Description : Error formatting utilities for the Glados compiler.
Stability   : stable

This module provides tools to transform technical Megaparsec errors into
human-readable, colored error messages. It helps standardize error reporting
across the entire application (Parsing, Semantics, Imports).
-}
module Common.Error (formatParseError, formatError) where

import Data.Text (Text)
import Data.Void (Void)

-- Imports specific to Position handling
import Text.Megaparsec
    ( PosState(..)
    , reachOffset
    , unPos
    , sourceLine
    , sourceColumn
    )

-- Imports for Error types (ParseError, Bundle, etc.)
import Text.Megaparsec.Error
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import Data.List (intercalate, sort)

-- | Formats a generic error message with a colored tag.
--
-- @args
--   - tag: The error category (e.g., "Semantic Error", "Import Error").
--   - msg: The error description.
--
-- @return
--   A formatted string with the tag in red ANSI color.
formatError :: String -> String -> String
formatError tag msg = "\x1b[31m[" ++ tag ++ "]\x1b[0m " ++ msg

-- | Extracts the "expected" tokens from a ParseError.
--
-- @args
--   - err: The Megaparsec error object.
--
-- @return
--   A clean string listing expected tokens or "custom error".
getExpected :: ParseError Text Void -> String
getExpected (TrivialError _ _ expectedSet) = 
    cleanExpected (sort $ Set.toList expectedSet)
getExpected (FancyError _ _) = "custom error"

-- | Extracts the "unexpected" token from a ParseError.
--
-- @args
--   - err: The Megaparsec error object.
--
-- @return
--   A string describing what was found instead (e.g., "end of input", "\"foo\"").
getUnexpected :: ParseError Text Void -> String
getUnexpected (TrivialError _ (Just unex) _) = parseErrorItem unex
getUnexpected (TrivialError _ Nothing _) = "end of input"
getUnexpected (FancyError _ _) = "custom error"

-- | Main entry point to format a Megaparsec error bundle.
--
-- @args
--   - bundle: The full error bundle returned by 'parse'.
--
-- @details
--   Extracts the first error from the bundle, calculates its line and column,
--   and constructs a detailed, user-friendly message using 'formatError'.
--
-- @return
--   A fully formatted error string ready for output.
formatParseError :: ParseErrorBundle Text Void -> String
formatParseError bundle = 
    let (err :| _) = bundleErrors bundle
        (line, col) = getLineCol (bundlePosState bundle) (errorOffset err)
        details = "line " ++ show line ++ ", column " ++ show col ++ 
                  ", expected " ++ getExpected err ++ 
                  " but got " ++ getUnexpected err
    in formatError "Parse Error" details

-- | Calculates line and column numbers from a generic Position State.
--
-- @args
--   - state: The initial position state.
--   - offset: The absolute character offset of the error.
--
-- @return
--   A tuple (Line, Column).
getLineCol :: PosState Text -> Int -> (Int, Int)
getLineCol state offset = 
    let (_, newState) = reachOffset offset state
        pos = pstateSourcePos newState
    in (unPos (sourceLine pos), unPos (sourceColumn pos))

-- | Formats a list of expected ErrorItems into a readable string.
--
-- @details
--   Prioritizes Labels (human-readable names like "semicolon") over raw Tokens.
--   If many tokens are expected, lists the first 3 or joins labels with "or".
--
cleanExpected :: [ErrorItem Char] -> String
cleanExpected [] = "unknown"
cleanExpected items = 
    let labels = [NE.toList l | Label l <- items] 
        tokens = [show (NE.toList t) | Tokens t <- items]
    in if null labels 
       then intercalate ", " (take 3 tokens)
       else intercalate " or " labels

-- | Converts a single ErrorItem to its string representation.
parseErrorItem :: ErrorItem Char -> String
parseErrorItem (Tokens t) = show (NE.toList t)
parseErrorItem (Label l) = NE.toList l
parseErrorItem (EndOfInput) = "end of input"
