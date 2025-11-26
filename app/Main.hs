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
processSingle ftable env s = case processSExpr ftable env s of
    Left err -> Left err
    Right (n_ftable, n_env, Nothing) -> Right (n_ftable, n_env, [])
    Right (n_ftable, n_env, Just a)  -> Right (n_ftable, n_env, [a])

processMany :: [(String, [String], Ast)] -> Env -> [SExpr] ->
    Either String [Ast]
processMany _ _ [] = Right []
processMany ftable env (x:xs) = case processSingle ftable env x of
    Left err -> Left err
    Right (n_ftable, new_env, outs) ->
        case processMany n_ftable new_env xs of
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
