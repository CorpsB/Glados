{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
-}

{-|
Module      : Main
Description : Entry point, REPL, and batch execution for the ISL/Lisp interpreter.
Stability   : experimental

This module provides the program entry point and two execution modes:

* **Interactive mode (REPL)** when /stdin/ is a terminal.
  The interpreter maintains a mutable-like state across lines via:
  * a function table ('FuncTable') and
  * an evaluation environment ('Env').

* **Batch mode** when /stdin/ is redirected (e.g., piping a file).
  The whole input is parsed as a list of S-expressions, evaluated sequentially,
  and every produced value is printed.

Errors are reported to /stderr/ and, in batch mode, result in exit code @84@
(EPITECH convention).
-}
module Main (main) where

import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))
import System.IO
import Control.Exception (catch, IOException)
import Lisp (SExpr(..))
import Ast (Ast(..), Env)
import Type.Integer (intValueToInt)
import Parser.ParserISL (parseLisp, parseLispLine)
import Eval.Run (processSExpr)
import Eval.Functions (FuncTable)
import qualified Data.Text as DT

-- | Evaluate a single S-expression in the given state.
--
-- This is a small adapter around 'processSExpr' that normalizes the output to a
-- list of 'Ast' values:
--
-- * If evaluation returns @Nothing@, no value is produced (empty list).
-- * If evaluation returns @Just a@, one value is produced (@[a]@).
--
-- On success, the updated function table and environment are returned along
-- with the produced values.
processSingle :: FuncTable -> Env -> SExpr
    -> Either DT.Text (FuncTable, Env, [Ast])
processSingle ftable env s = case processSExpr ftable env s of
    Left err -> Left err
    Right (n_ftable, n_env, Nothing) -> Right (n_ftable, n_env, [])
    Right (n_ftable, n_env, Just a)  -> Right (n_ftable, n_env, [a])

-- | Evaluate a sequence of S-expressions in order, threading the interpreter state.
--
-- The function table and environment are updated after each expression.
-- All produced values are concatenated and returned.
--
-- The evaluation stops at the first error.
processMany :: FuncTable -> Env -> [SExpr]
    -> Either DT.Text [Ast]
processMany _ _ [] = Right []
processMany ftable env (x:xs) = case processSingle ftable env x of
    Left err -> Left err
    Right (n_ftable, new_env, outs) ->
        case processMany n_ftable new_env xs of
            Left err2  -> Left err2
            Right rest -> Right (outs ++ rest)

-- | Print an evaluated value in the language's user-facing representation.
--
-- This function handles the canonical output formats:
--
-- * integers via 'intValueToInt'
-- * booleans as @#t@ / @#f@
-- * symbols as plain text
-- * procedures/closures as a placeholder string
-- * lists as parenthesized sequences
--
-- Any constructor not explicitly handled falls back to 'show'.
printAst :: Ast -> IO ()
printAst (AInteger i) = putStrLn $ show $ intValueToInt i
printAst (ABool True) = putStrLn "#t"
printAst (ABool False) = putStrLn "#f"
printAst (ASymbol s) = putStrLn (DT.unpack s)
printAst (Closure _ _ _) = putStrLn "#\\<procedure\\>"
printAst (Lambda _ _) = putStrLn "#<lambda>"
printAst (AList xs) = putStrLn $ "(" ++ unwords (map showAst xs) ++ ")"
printAst other = putStrLn (show other)

-- | Convert an 'Ast' value to its user-facing string representation.
--
-- This mirrors 'printAst' but returns a pure 'String' (useful for list printing).
showAst :: Ast -> String
showAst (AInteger i) = show $ intValueToInt i
showAst (ABool True) = "#t"
showAst (ABool False) = "#f"
showAst (ASymbol s) = DT.unpack s
showAst (AList xs) = "(" ++ unwords (map showAst xs) ++ ")"
showAst (Closure _ _ _) = "#\\<procedure\\>"
showAst (Lambda _ _) = "#<lambda>"
showAst other = show other

-- | Parse and evaluate a single REPL line.
--
-- The line is parsed using 'parseLispLine'. A parse failure is mapped to a
-- user-friendly error text. On success, evaluation is delegated to 'processSExpr'.
tryEval :: FuncTable -> Env -> String
    -> Either DT.Text (FuncTable, Env, Maybe Ast)
tryEval ft env input = case parseLispLine (DT.pack input) of
    Left _      -> Left (DT.pack "*** ERROR: Parse error")
    Right sexpr -> processSExpr ft env sexpr

-- | Batch execution mode: read all input from stdin, parse it, evaluate it, print results.
--
-- This mode is used when stdin is /not/ a terminal (e.g. file redirection or piping).
--
-- Failure modes:
--
-- * If parsing fails, prints a parse error and exits with code @84@.
-- * If evaluation fails, prints the evaluation error and exits with code @84@.
runFromFile :: IO ()
runFromFile = do
    input <- getContents
    case parseLisp (DT.pack input) of
        Left perr -> hPutStrLn stderr ("Parse error: " ++ show perr) >>
            exitWith (ExitFailure 84)
        Right sexprs -> case processMany [] [] sexprs of
            Left err     -> hPutStrLn stderr (DT.unpack err) >> exitWith (ExitFailure 84)
            Right values -> mapM_ printAst values

-- | Handler used to gracefully terminate the REPL on EOF (Ctrl-D).
handleEOF :: IOException -> IO String
handleEOF _ = exitWith ExitSuccess

-- | Process a single REPL line, printing either the error or the evaluated result(s),
-- then continuing the REPL with the updated state.
processReplLine :: FuncTable -> Env -> String -> IO ()
processReplLine ft env line = case tryEval ft env line of
    Left err -> do
        hPutStrLn stderr (DT.unpack err)
        repl ft env
    Right (newFt, newEnv, res) -> do
        mapM_ printAst res
        repl newFt newEnv

-- | Interactive read-eval-print loop.
--
-- The REPL:
--
-- * prints the prompt @"> "@
-- * reads one line (EOF exits cleanly)
-- * ignores empty lines
-- * evaluates the line and prints any resulting value
-- * threads 'FuncTable' and 'Env' so definitions persist across inputs
repl :: FuncTable -> Env -> IO ()
repl ft env = do
    putStr "> " >> hFlush stdout
    line <- catch getLine handleEOF
    if null line
        then repl ft env
        else processReplLine ft env line

-- | Program entry point.
--
-- Detects whether stdin is a terminal:
--
-- * If yes, enables interactive mode ('repl').
-- * Otherwise, runs batch mode ('runFromFile').
main :: IO ()
main = do
    isTerm <- hIsTerminalDevice stdin
    if isTerm
        then do
            hSetBuffering stdout NoBuffering
            repl [] []
        else runFromFile
