{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ImportSystem.hs
-}

module Parser.ImportSystem (resolveImports, constructForAst) where

import Control.Exception (try, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec.Error (errorBundlePretty)
import Data.List (isSuffixOf)
import AST.Ast (Ast(..))
import Parser.Statement (parseALL)

-- | Main function: Resolves imports within a list of AST nodes.
--
-- It traverses the list. If it encounters an Import node, it replaces it
-- with the parsed content of the target file (recursively).
-- Otherwise, it descends into child nodes (If, While, Func...) to check
-- for nested imports.
resolveImports :: [Ast] -> IO (Either String [Ast])
resolveImports asts = resolveImportsRec [] asts

-- | Recursive worker that carries the list of visited files to prevent cycles.
resolveImportsRec :: [String] -> [Ast] -> IO (Either String [Ast])
resolveImportsRec _ [] = return (Right [])
resolveImportsRec visited (node:xs) = do
    processedHead <- processNode visited node
    case processedHead of
        Left err -> return (Left err)
        Right nodes -> do
            processedTail <- resolveImportsRec visited xs
            case processedTail of
                Left err -> return (Left err)
                Right tailNodes -> return (Right (nodes ++ tailNodes))

-- | Processes an individual node with cycle detection context.
-- Returns a list of ASTs (since a single Import can expand into multiple instructions).
processNode :: [String] -> Ast -> IO (Either String [Ast])
processNode visited (AImport path) = processImport visited path
processNode visited (AList subStmts) = processAList visited subStmts
processNode visited (ADefineFunc name args ret body) =
    processFunc visited name args ret body
processNode visited (ADefineLambda args body) = processLambda visited args body
processNode visited (AWhile cond body) = processWhile visited cond body
processNode visited (AFor i c u b) = processFor visited i c u b
processNode visited (AIf c t e) = processIf visited c t e
processNode visited (APos l c ast) = processAPos visited l c ast
processNode _ other = return (Right [other])

-- | Reads the content of an import file.
-- Returns an Either type to handle IO exceptions gracefully.
readImportFile :: String -> IO (Either String T.Text)
readImportFile path = do
    res <- try (TIO.readFile path) :: IO (Either IOException T.Text)
    return $ case res of
        Left ex -> Left ("IO Error reading import '"
            ++ path ++ "': " ++ show ex)
        Right txt -> Right txt

-- | Parses the content of an imported file and recursively resolves its imports.
parseImportContent :: [String] -> String -> T.Text -> IO (Either String [Ast])
parseImportContent visited path content = case parseALL content of
    Left err -> return (Left ("Parse error in '" ++ path ++ "':\n" ++
        errorBundlePretty err))
    Right asts -> resolveImportsRec (path : visited) asts

-- | Process Import Statements.
-- Checks for:
-- 1. .npy extension
-- 2. Circular dependency
processImport :: [String] -> T.Text -> IO (Either String [Ast])
processImport visited path =
    let pathStr = T.unpack path
    in if not (".npy" `isSuffixOf` pathStr)
        then return (Left $ "Import Error: File '" ++ pathStr ++
            "' must have .npy extension")
        else processImportLogic visited pathStr

-- | Logic for imports to avoid nested ifs and long functions.
processImportLogic :: [String] -> String -> IO (Either String [Ast])
processImportLogic visited pathStr
    | pathStr `elem` visited = return (Left $ "Circular import detected: " ++
        pathStr ++ " is already in the import stack " ++ show visited)
    | otherwise = do
        content <- readImportFile pathStr
        case content of
            Left err -> return (Left err)
            Right txt -> parseImportContent visited pathStr txt

-- | Process Block Statements.
processAList :: [String] -> [Ast] -> IO (Either String [Ast])
processAList visited subStmts = do
    res <- resolveImportsRec visited subStmts
    case res of
        Left err -> return (Left err)
        Right newStmts -> return (Right [AList newStmts])

-- | Process Function Definitions.
processFunc :: [String] -> T.Text -> [(T.Text, T.Text)] -> T.Text -> Ast ->
    IO (Either String [Ast])
processFunc visited name args ret body = do
    res <- processSingleNode visited body
    case res of
        Left err -> return (Left err)
        Right newBody -> return (Right [ADefineFunc name args ret newBody])

-- | Process Lambda Definitions.
processLambda :: [String] -> [T.Text] -> Ast -> IO (Either String [Ast])
processLambda visited args body = do
    res <- processSingleNode visited body
    case res of
        Left err -> return (Left err)
        Right newBody -> return (Right [ADefineLambda args newBody])

-- | Process While Loops.
processWhile :: [String] -> Ast -> Ast -> IO (Either String [Ast])
processWhile visited cond body = do
    resCond <- processSingleNode visited cond
    resBody <- processSingleNode visited body
    case (resCond, resBody) of
        (Right newCond, Right newBody) ->
            return (Right [AWhile newCond newBody])
        (Left err, _) -> return (Left err)
        (_, Left err) -> return (Left err)

-- | Function exposed for testing purposes.
-- Checks if the processed list has exactly 4 elements to rebuild the For loop.
constructForAst :: [Ast] -> Either String [Ast]
constructForAst [ni, nc, nu, nb] = Right [AFor ni nc nu nb]
constructForAst _ = Left
    "Internal Error: Invalid argument count in For loop processing"

-- | Process For Loops.
--
-- Processes the four components of the loop:
-- ni: New Initialization
-- nc: New Condition
-- nu: New Update
-- nb: New Body
processFor :: [String] -> Ast -> Ast -> Ast -> Ast -> IO (Either String [Ast])
processFor visited i c u b = do
    results <- mapM (processSingleNode visited) [i, c, u, b]
    return $ case sequence results of
        Left err -> Left err
        Right list -> constructForAst list

-- | Process If Conditions.
processIf :: [String] -> Ast -> Ast -> Ast -> IO (Either String [Ast])
processIf visited c t e = do
    resC <- processSingleNode visited c
    resT <- processSingleNode visited t
    resE <- processSingleNode visited e
    case (resC, resT, resE) of
        (Right nc, Right nt, Right ne) -> return (Right [AIf nc nt ne])
        (Left err, _, _) -> return (Left err)
        (_, Left err, _) -> return (Left err)
        (_, _, Left err) -> return (Left err)

-- | Process Position Wrappers.
processAPos :: [String] -> Int -> Int -> Ast -> IO (Either String [Ast])
processAPos visited l c ast = do
    res <- processNode visited ast
    case res of
        Left err -> return (Left err)
        Right [single] -> return (Right [APos l c single])
        Right multiple -> return (Right multiple)

-- | Process a single node that must remain single (e.g., if condition).
--
-- If an import is placed where a single expression is expected, it is contextual,
-- but here we assume an import returns nothing (or a list).
-- To simplify, we take the first element or wrap it.
processSingleNode :: [String] -> Ast -> IO (Either String Ast)
processSingleNode visited node = do
    res <- processNode visited node
    case res of
        Left err -> return (Left err)
        Right [] -> return (Right AVoid)
        Right [x] -> return (Right x)
        Right (x:_) -> return (Right x)
