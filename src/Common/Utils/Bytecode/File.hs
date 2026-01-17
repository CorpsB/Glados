{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- File
-}

module Common.Utils.Bytecode.File
    ( getFileContent
    , tryReadFile
    ) where

import qualified Data.ByteString as BS
import Control.Exception (try, SomeException)
import System.Exit (exitWith, ExitCode(..))

-- | Helper to attempt reading a file with explicit typing.
--
-- @details
--   Moves the type casting out of the main logic.
--
tryReadFile :: String -> IO (Either SomeException BS.ByteString)
tryReadFile path = try (BS.readFile path)

-- | Retrieves the file content or exits the program on error.
--
-- @args
--   - path: Path to the file.
--
-- @details
--   Handles the IO exception immediately: prints the error and calls exitFailure.
--   This prevents 'executeFile' from handling nested cases.
--
-- @return
--   The file content as ByteString.
--
getFileContent :: String -> IO BS.ByteString
getFileContent path = do
    result <- tryReadFile path
    case result of
        Left err -> putStrLn ("\ESC[31mIO Error:\ESC[0m " ++ show err) >>
            exitWith (ExitFailure 84)
        Right content -> return content
