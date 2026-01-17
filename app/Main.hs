{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Main Compiler Entry Point (Refactored)
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import Control.Monad.State (runStateT)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Parser.Statement (parseALL)
import Parser.ImportSystem (resolveImports)
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
usage = putStrLn ("Usage: ./glados <input_file> <output_file>"
    ++ "  input_file:  Path to the source code."
    ++ "  output_file: Path to the generated binary (.gla).")

-- | Print the given error message then exit 84.
--
die :: String -> IO a
die msg = hPutStrLn stderr ("\ESC[31mError:\ESC[0m " ++ msg) >>
    exitWith (ExitFailure 84)

parseSource :: T.Text -> IO [Ast]
parseSource src = case parseALL src of
    Left err -> die $ "Parsing failed:\n" ++ show err
    Right asts -> return asts


handleImports :: [Ast] -> IO [Ast]
handleImports asts = do
    res <- resolveImports asts
    case res of
        Left err -> die $ "Import Error: " ++ err
        Right r -> return r

checkSemantics :: [Ast] -> IO [Ast]
checkSemantics asts =
    case checkAst asts of
        Left err -> die $ "Semantic Error: " ++ err
        Right checkedAsts -> return checkedAsts

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
    src <- TIO.readFile inputPath
    ast <- parseSource src >>= handleImports >>= checkSemantics
    state <- compileSource ast
    bytecode <- assembleCode (extractInstructions state)
    writeBinary outputPath bytecode

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> runCompiler input output
        _ -> usage >> exitWith (ExitFailure 84)
