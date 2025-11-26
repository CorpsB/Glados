{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Main
-}

module Main (main) where

import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import Lisp (SExpr(..))
import Ast (Ast(..), Env)
import Parser.ParserISL (parseLisp)
import Eval.Run (processSExpr)

processSingle :: [(String, [String], Ast)] -> Env -> SExpr ->
    Either String ([(String, [String], Ast)], Env, [Ast])
processSingle ft en s = case processSExpr ft en s of
    Left err -> Left err
    Right (ft', en', Nothing) -> Right (ft', en', [])
    Right (ft', en', Just a)  -> Right (ft', en', [a])

processMany :: [(String, [String], Ast)] -> Env -> [SExpr] ->
    Either String [Ast]
processMany _ _ [] = Right []
processMany ft en (x:xs) = case processSingle ft en x of
    Left err -> Left err
    Right (ft', en', outs) ->
        case processMany ft' en' xs of
            Left err2  -> Left err2
            Right rest -> Right (outs ++ rest)

printAst :: Ast -> IO ()
printAst (AInteger i) = print i
printAst (ABool True) = putStrLn "#t"
printAst (ABool False) = putStrLn "#f"
printAst (ASymbol s) = putStrLn s
printAst (Closure _ _ _) = putStrLn "#\\<procedure\\>"
printAst (Lambda _ _) = putStrLn "#<lambda>"
printAst other = putStrLn (show other)

main :: IO ()
main = do
    input <- getContents
    case parseLisp input of
        Left perr -> hPutStrLn stderr ("Parse error: " ++ show perr) >>
            exitWith (ExitFailure 84)
        Right sexprs -> case processMany [] [] sexprs of
            Left err     -> hPutStrLn stderr err >> exitWith (ExitFailure 84)
            Right values -> mapM_ printAst values
