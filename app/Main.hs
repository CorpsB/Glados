{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Main Compiler Entry Point (Refactored)
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import Control.Monad.State (runStateT)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Parser.Statement (parseALL)
import Compiler.ASM.Compiler (compileAst)
import Compiler.ASM.Assembler (assemble)
import Compiler.CompilerState (createCompilerState, csCode, csFuncs, CompilerState)
import Compiler.PsInstruction (PsInstruction(Real))
import Compiler.Instruction (Instruction(Halt))
import AST.Semantics.Check (checkAst)
import AST.Ast (Ast)

-- | Print usage message on stdout.
--
usage :: IO ()
usage = do
    putStrLn "Usage: ./glados <input_file> <output_file>"
    putStrLn "  input_file:  Path to the source code."
    putStrLn "  output_file: Path to the generated binary (.gla)."

-- | Print the given error message then exit 84.
--
die :: String -> IO a
die msg = do
    hPutStrLn stderr $ "\ESC[31mError:\ESC[0m " ++ msg
    exitWith (ExitFailure 84)

parseSource :: T.Text -> IO [Ast]
parseSource src = case parseALL src of
    Left err -> die $ "Parsing failed:\n" ++ show err
    Right asts -> return asts

compileSource :: [Ast] -> IO CompilerState
compileSource asts =
    case runStateT (mapM_ compileAst asts) createCompilerState of
        Left err -> die $ "Compilation failed: " ++ T.unpack err
        Right (_, s) -> return s

extractInstructions :: CompilerState -> [PsInstruction]
extractInstructions st =
    let mainSeq = csCode st Seq.|> Real Halt
        allSeq = mainSeq Seq.>< csFuncs st in
    toList allSeq

assembleCode :: [PsInstruction] -> IO BS.ByteString
assembleCode instructions = case assemble instructions of
    Left err -> die $ "Assembly failed: " ++ T.unpack err
    Right bs -> return bs

writeBinary :: String -> BS.ByteString -> IO ()
writeBinary path bs = BS.writeFile path bs >>
    (putStrLn $ "\ESC[32mSuccess:\ESC[0m Binary generated at " ++ path)

runCompiler :: String -> String -> IO ()
runCompiler inputPath outputPath = do
    file_content <- TIO.readFile inputPath
    parsed_ast <- parseSource file_content
    state <- compileSource parsed_ast

    case checkAst parsed_ast of
        Left err -> die $ "Semantic Error: " ++ err
        Right _  -> return ()
    bytecode <- assembleCode (extractInstructions state)
    writeBinary outputPath bytecode

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> runCompiler input output
        _ -> usage >> exitFailure
