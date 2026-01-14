{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Entry Point
-}

{-|
Module      : Main
Description : Entry point for the Glados Virtual Machine.
Stability   : stable

This module handles:
1. Command line argument parsing (getting the bytecode file).
2. Loading the binary file into memory.
3. Initializing the VM State.
4. Executing the bytecode with error handling.
-}
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (try, SomeException)
import Control.Monad.State.Strict (runStateT)

import VM.VMState (VMState, createVMState)
import VM.Bytecode.Runner (runBytecode)
import Common.Utils.Bytecode.Header
import Common.Utils.Bytecode.File

-- | Helper function to execute the VM loop safely.
--
-- @args
--   - state: The initial VMState containing the loaded bytecode.
--
-- @details
--   Encapsulates the 'try' logic and fixes the polymorphic types
--   via the top-level signature, avoiding inline type casting.
--
-- @return
--   Either a runtime exception (Left) or the final result/state (Right).
--
tryRunVM :: VMState -> IO (Either SomeException ((), VMState))
tryRunVM state = try (runStateT runBytecode state)

-- | Loads and executes the specified bytecode file.
--
-- @args
--   - path: The file path to the binary .gla file.
--
-- @details
--   1. Gets content via 'getFileContent' (exits if fails).
--   2. Initializes VM.
--   3. Runs VM via 'tryRunVM'.
--   4. Handles Runtime errors.
--
executeFile :: String -> Bool -> IO ()
executeFile path debugMode = do
    rawContent <- getFileContent path
    content <- extractContentIfValidHeader rawContent
    result <- tryRunVM (createVMState content debugMode)
    case result of
        Left err -> putStrLn ("\ESC[31mRuntime Error:\ESC[0m " ++ show err) >>
            exitFailure
        Right _ -> exitSuccess

-- | Prints the usage instructions to stdout.
--
printUsage :: IO ()
printUsage = putStrLn "Usage: ./glados-vm <file>"

-- | The main entry point of the application.
--
-- @details
--   1. Checks arguments for the input file.
--   2. Reads the bytecode.
--   3. Wraps the execution in a 'try' block to catch runtime errors
--      (like Stack Underflow, Division by Zero, Invalid Opcode).
--   4. Prints the error message if execution fails.
--
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "\ESC[31mError: No input file provided.\ESC[0m" >>
            printUsage >> exitFailure
        [filename] -> executeFile filename False
        ["--debug", filename] -> executeFile filename True
        _  -> putStrLn "\ESC[31mError: Too many arguments.\ESC[0m" >>
            putStrLn "The VM accepts exactly one bytecode file execution." >>
            printUsage >> exitFailure
