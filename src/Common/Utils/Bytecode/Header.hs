{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Header
-}

module Common.Utils.Bytecode.Header
    ( extractContentIfValidHeader
    , evaluateHeader
    , hasValidSize
    , hasValidMagic
    , hasValidVersion
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.Exit (exitFailure)

-- | Checks if the file is large enough to contain a header (10 bytes).
--
hasValidSize :: BS.ByteString -> Bool
hasValidSize bs = BS.length bs >= 10

-- | Checks if the Magic Number is "GLAD" (0x47 0x4C 0x41 0x44).
--
hasValidMagic :: BS.ByteString -> Bool
hasValidMagic bs = BS.take 4 bs == C8.pack "GLAD"

-- | Checks if the Version is supported (0x02).
--
hasValidVersion :: BS.ByteString -> Bool
hasValidVersion bs = BS.index bs 4 == 0x02

-- | Evaluates the header validity by combining all predicates.
--
-- @details
--   Uses short-circuit boolean logic (&&).
--   Order matters: 'hasValidSize' must be checked first to prevent
--   segmentation faults in 'hasValidVersion'.
--
evaluateHeader :: BS.ByteString -> Bool
evaluateHeader bs = hasValidSize bs && hasValidMagic bs && hasValidVersion bs

-- | Validates the .gla file header and returns the raw bytecode.
--
-- @details
--   If the header is valid, drops the 10-byte header and returns the code.
--   If invalid, prints a generic error and exits.
--
extractContentIfValidHeader :: BS.ByteString -> IO BS.ByteString
extractContentIfValidHeader content =
    case evaluateHeader content of
        True  -> return (BS.drop 10 content)
        False -> do
            putStrLn "\ESC[31mInvalid File: Bad Header.\ESC[0m"
            exitFailure
