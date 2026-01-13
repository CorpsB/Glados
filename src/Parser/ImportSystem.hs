{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ImportSystem.hs
-}

module Parser.ImportSystem (resolveImports) where

import Control.Exception (try, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec.Error (errorBundlePretty)
import AST.Ast (Ast(..))
import Parser.Statement (parseALL)

-- | Main function: Resolves imports within a list of AST nodes.
--
-- It traverses the list. If it encounters an Import node, it replaces it
-- with the parsed content of the target file (recursively).
-- Otherwise, it descends into child nodes (If, While, Func...) to check
-- for nested imports.
resolveImports :: [Ast] -> IO (Either String [Ast])
resolveImports [] = return (Right [])
resolveImports (node:xs) = do
    processedHead <- processNode node
    case processedHead of
        Left err -> return (Left err)
        Right nodes -> do
            processedTail <- resolveImports xs
            case processedTail of
                Left err -> return (Left err)
                Right tailNodes -> return (Right (nodes ++ tailNodes))

-- | Processes an individual node.
-- Returns a list of ASTs (since a single Import can expand into multiple instructions).
processNode :: Ast -> IO (Either String [Ast])
processNode (AImport path) = processImport path
processNode (AList subStmts) = processAList subStmts
processNode (ADefineFunc name args ret body) = processFunc name args ret body
processNode (ADefineLambda args body) = processLambda args body
processNode (AWhile cond body) = processWhile cond body
processNode (AFor i c u b) = processFor i c u b
processNode (AIf c t e) = processIf c t e
processNode (APos l c ast) = processAPos l c ast
processNode other = return (Right [other])

-- | Safely reads the content of an import file.
-- Returns an Either type to handle IO exceptions gracefully.
readImportFile :: String -> IO (Either String T.Text)
readImportFile path = do
    res <- try (TIO.readFile path) :: IO (Either IOException T.Text)
    return $ case res of
        Left ex -> Left ("IO Error reading import '"
            ++ path ++ "': " ++ show ex)
        Right txt -> Right txt

-- | Parses the content of an imported file and recursively resolves its imports.
parseImportContent :: String -> T.Text -> IO (Either String [Ast])
parseImportContent path content = case parseALL content of
    Left err -> return (Left ("Parse error in '" ++ path ++ "':\n" ++
        errorBundlePretty err))
    Right asts -> resolveImports asts

-- | Process Import Statements.
-- Handles the file reading and parsing logic via helper functions.
processImport :: T.Text -> IO (Either String [Ast])
processImport path = do
    let pathStr = T.unpack path
    content <- readImportFile pathStr
    case content of
        Left err -> return (Left err)
        Right txt -> parseImportContent pathStr txt

-- | Process Block Statements.
processAList :: [Ast] -> IO (Either String [Ast])
processAList subStmts = do
    res <- resolveImports subStmts
    case res of
        Left err -> return (Left err)
        Right newStmts -> return (Right [AList newStmts])

-- | Process Function Definitions.
processFunc :: T.Text -> [(T.Text, T.Text)] -> T.Text -> Ast ->
    IO (Either String [Ast])
processFunc name args ret body = do
    res <- processSingleNode body
    case res of
        Left err -> return (Left err)
        Right newBody -> return (Right [ADefineFunc name args ret newBody])

-- | Process Lambda Definitions.
processLambda :: [T.Text] -> Ast -> IO (Either String [Ast])
processLambda args body = do
    res <- processSingleNode body
    case res of
        Left err -> return (Left err)
        Right newBody -> return (Right [ADefineLambda args newBody])

-- | Process While Loops.
processWhile :: Ast -> Ast -> IO (Either String [Ast])
processWhile cond body = do
    resCond <- processSingleNode cond
    resBody <- processSingleNode body
    case (resCond, resBody) of
        (Right newCond, Right newBody) ->
            return (Right [AWhile newCond newBody])
        (Left err, _) -> return (Left err)
        (_, Left err) -> return (Left err)

-- | Process For Loops.
--
-- Processes the four components of the loop:
-- ni: New Initialization
-- nc: New Condition
-- nu: New Update
-- nb: New Body
processFor :: Ast -> Ast -> Ast -> Ast -> IO (Either String [Ast])
processFor i c u b = do
    results <- mapM processSingleNode [i, c, u, b]
    return $ case sequence results of
        Right [ni, nc, nu, nb] -> Right [AFor ni nc nu nb]
        Left err -> Left err
        _ -> Left
            "Internal Error: Invalid argument count in For loop processing"

-- | Process If Conditions.
processIf :: Ast -> Ast -> Ast -> IO (Either String [Ast])
processIf c t e = do
    resC <- processSingleNode c
    resT <- processSingleNode t
    resE <- processSingleNode e
    case (resC, resT, resE) of
        (Right nc, Right nt, Right ne) -> return (Right [AIf nc nt ne])
        (Left err, _, _) -> return (Left err)
        (_, Left err, _) -> return (Left err)
        (_, _, Left err) -> return (Left err)

-- | Process Position Wrappers.
processAPos :: Int -> Int -> Ast -> IO (Either String [Ast])
processAPos l c ast = do
    res <- processNode ast
    case res of
        Left err -> return (Left err)
        Right [single] -> return (Right [APos l c single])
        Right multiple -> return (Right multiple)

-- | Process a single node that must remain single (e.g., if condition).
--
-- If an import is placed where a single expression is expected, it is contextual,
-- but here we assume an import returns nothing (or a list).
-- To simplify, we take the first element or wrap it.
processSingleNode :: Ast -> IO (Either String Ast)
processSingleNode node = do
    res <- processNode node
    case res of
        Left err -> return (Left err)
        Right [] -> return (Right AVoid)
        Right [x] -> return (Right x)
        Right (x:_) -> return (Right x)
