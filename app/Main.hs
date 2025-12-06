{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
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

processSingle :: FuncTable -> Env -> SExpr ->
    Either String (FuncTable, Env, [Ast])
processSingle ftable env s = case processSExpr ftable env s of
    Left err -> Left err
    Right (n_ftable, n_env, Nothing) -> Right (n_ftable, n_env, [])
    Right (n_ftable, n_env, Just a)  -> Right (n_ftable, n_env, [a])

processMany :: FuncTable -> Env -> [SExpr] ->
    Either String [Ast]
processMany _ _ [] = Right []
processMany ftable env (x:xs) = case processSingle ftable env x of
    Left err -> Left err
    Right (n_ftable, new_env, outs) ->
        case processMany n_ftable new_env xs of
            Left err2  -> Left err2
            Right rest -> Right (outs ++ rest)

printAst :: Ast -> IO ()
printAst (AInteger i) = putStrLn $ show $ intValueToInt i
printAst (ABool True) = putStrLn "#t"
printAst (ABool False) = putStrLn "#f"
printAst (ASymbol s) = putStrLn s
printAst (Closure _ _ _) = putStrLn "#\\<procedure\\>"
printAst (Lambda _ _) = putStrLn "#<lambda>"
printAst other = putStrLn (show other)

tryEval :: FuncTable -> Env -> String -> Either String (FuncTable, Env, Maybe Ast)
tryEval ft env input = case parseLispLine input of
    Left _      -> Left "*** ERROR: Parse error"
    Right sexpr -> processSExpr ft env sexpr

runFromFile :: IO ()
runFromFile = do
    input <- getContents
    case parseLisp input of
        Left perr -> hPutStrLn stderr ("Parse error: " ++ show perr) >>
            exitWith (ExitFailure 84)
        Right sexprs -> case processMany [] [] sexprs of
            Left err     -> hPutStrLn stderr err >> exitWith (ExitFailure 84)
            Right values -> mapM_ printAst values

handleEOF :: IOException -> IO String
handleEOF _ = exitWith ExitSuccess

processReplLine :: FuncTable -> Env -> String -> IO ()
processReplLine ft env line = case tryEval ft env line of
    Left err -> do
        hPutStrLn stderr err
        repl ft env
    Right (newFt, newEnv, res) -> do
        mapM_ printAst res
        repl newFt newEnv

repl :: FuncTable -> Env -> IO ()
repl ft env = do
    putStr "> " >> hFlush stdout
    line <- catch getLine handleEOF
    if null line
        then repl ft env
        else processReplLine ft env line

main :: IO ()
main = do
    isTerm <- hIsTerminalDevice stdin
    if isTerm
        then do
            hSetBuffering stdout NoBuffering
            repl [] []
        else runFromFile
