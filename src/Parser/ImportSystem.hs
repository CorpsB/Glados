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

-- | Main entry point used by the Compiler/VM.
-- Resolves all 'AImport' nodes within a list of AST statements.
--
-- @param asts: The initial list of AST nodes (parsed from the main file).
-- @return: Either an error message (String) or the fully resolved AST list.
resolveImports :: [Ast] -> IO (Either String [Ast])
resolveImports asts = do
    res <- resolveImportsRec [] asts
    case res of
        Left err -> return (Left err)
        Right (newAsts, _) -> return (Right newAsts)

-- | Recursive worker that processes a list of nodes threading the 'visited' state.
--
-- @param visited: List of file paths already imported (stack trace for cycles).
-- @param nodes: The list of AST nodes to process.
-- @return: Tuple (Resolved ASTs, Updated Visited Stack).
resolveImportsRec :: [String] -> [Ast] -> IO (Either String ([Ast], [String]))
resolveImportsRec visited [] = return (Right ([], visited))
resolveImportsRec visited (node:xs) = do
    processedHead <- processNode visited node
    handleRecResult processedHead xs

-- | Helper to process the tail of the list after the head is resolved.
--
-- @param headResult: The result of processing the first node.
-- @param xs: The remaining nodes to process.
handleRecResult :: Either String ([Ast], [String]) -> [Ast] ->
    IO (Either String ([Ast], [String]))
handleRecResult (Left err) _ = return (Left err)
handleRecResult (Right (headNodes, visitedAfterHead)) xs = do
    processedTail <- resolveImportsRec visitedAfterHead xs
    return $ case processedTail of
        Left err -> Left err
        Right (tailNodes, visitedAfterTail) ->
            Right (headNodes ++ tailNodes, visitedAfterTail)

-- | Safe file reading wrapper.
--
-- @param path: Path to the .npy file.
-- @return: The file content or an IO Error message.
readImportFile :: String -> IO (Either String T.Text)
readImportFile path = do
    res <- try (TIO.readFile path) :: IO (Either IOException T.Text)
    return $ case res of
        Left ex -> Left ("IO Error reading import '"
            ++ path ++ "': " ++ show ex)
        Right txt -> Right txt

-- | Parses an imported file and recursively resolves its own imports.
--
-- @param visited: Current stack of imported files.
-- @param path: The filename being processed.
-- @param content: The raw text content of the file.
parseImportContent :: [String] -> String -> T.Text ->
    IO (Either String ([Ast], [String]))
parseImportContent visited path content = case parseALL content of
    Left err -> return (Left ("Parse error in '" ++ path ++ "':\n" ++
        errorBundlePretty err))
    Right asts -> resolveImportsRec (path : visited) asts

-- | Validates the import path (extension) before processing.
processImport :: [String] -> T.Text -> IO (Either String ([Ast], [String]))
processImport visited path =
    let pathStr = T.unpack path
    in if not (".npy" `isSuffixOf` pathStr)
        then return (Left $ "Import Error: File '" ++ pathStr ++
            "' must have .npy extension")
        else processImportLogic visited pathStr

-- | Core logic for imports: Cycle detection and File Loading.
--
-- @param visited: The import stack.
-- @param pathStr: The file to import.
processImportLogic :: [String] -> String ->
    IO (Either String ([Ast], [String]))
processImportLogic visited pathStr
    | pathStr `elem` visited = return (Left $
        "Circular or duplicate import detected: " ++ pathStr ++
        " is already in the import stack " ++ show visited)
    | otherwise = do
        content <- readImportFile pathStr
        case content of
            Left err -> return (Left err)
            Right txt -> parseImportContent visited pathStr txt

-- | Dispatcher function.
-- Routes AST nodes to their specific handlers to resolve nested imports.
processNode :: [String] -> Ast -> IO (Either String ([Ast], [String]))
processNode visited (AImport path) = processImport visited path
processNode visited (AList subStmts) = processAList visited subStmts
processNode visited (ADefineFunc name args ret body) =
    processFunc visited name args ret body
processNode visited (ADefineLambda args body) =
    processLambda visited args body
processNode visited (AWhile cond body) = processWhile visited cond body
processNode visited (AFor i c u b) = processFor visited i c u b
processNode visited (AIf c t e) = processIf visited c t e
processNode visited (APos l c ast) = processAPos visited l c ast
processNode visited other = return (Right ([other], visited))

-- | Resolves imports inside a block (AList).
processAList :: [String] -> [Ast] -> IO (Either String ([Ast], [String]))
processAList visited subStmts = do
    res <- resolveImportsRec visited subStmts
    case res of
        Left err -> return (Left err)
        Right (newStmts, newVisited) ->
            return (Right ([AList newStmts], newVisited))

-- | Resolves imports inside a Function body.
processFunc :: [String] -> T.Text -> [(T.Text, T.Text)] -> T.Text -> Ast ->
    IO (Either String ([Ast], [String]))
processFunc visited name args ret body = do
    res <- processSingleNode visited body
    case res of
        Left err -> return (Left err)
        Right (newBody, newVisited) ->
            return (Right ([ADefineFunc name args ret newBody], newVisited))

-- | Resolves imports inside a Lambda body.
processLambda :: [String] -> [T.Text] -> Ast ->
    IO (Either String ([Ast], [String]))
processLambda visited args body = do
    res <- processSingleNode visited body
    case res of
        Left err -> return (Left err)
        Right (newBody, newVisited) ->
            return (Right ([ADefineLambda args newBody], newVisited))

-- | Resolves imports inside a While loop (Condition and Body).
processWhile :: [String] -> Ast -> Ast -> IO (Either String ([Ast], [String]))
processWhile visited cond body = do
    resCond <- processSingleNode visited cond
    processWhileBody resCond body

-- | Second step of While processing (The Body).
--
-- @param resCond: Result of processing the condition.
-- @param body: The loop body to process next.
processWhileBody :: Either String (Ast, [String]) -> Ast ->
    IO (Either String ([Ast], [String]))
processWhileBody (Left err) _ = return (Left err)
processWhileBody (Right (nc, visitedAfterCond)) body = do
    resBody <- processSingleNode visitedAfterCond body
    return $ case resBody of
        Left err -> Left err
        Right (nb, visitedAfterBody) ->
            Right ([AWhile nc nb], visitedAfterBody)

-- | Helper used by unit tests to reconstruct AFor nodes.
constructForAst :: [Ast] -> Either String [Ast]
constructForAst [ni, nc, nu, nb] = Right [AFor ni nc nu nb]
constructForAst _ =
    Left "Internal Error: Invalid argument count in For loop processing"

-- | Start of For Loop processing.
--
-- Abbreviations used to fit 80 cols:
-- i = Init (Initialization AST)
-- c = Cond (Condition AST)
-- u = Update (Increment AST)
-- b = Body (Loop body AST)
processFor :: [String] -> Ast -> Ast -> Ast -> Ast ->
    IO (Either String ([Ast], [String]))
processFor visited i c u b = do
    resI <- processSingleNode visited i
    processForCond resI c u b

-- | Second step of For Loop: The Condition.
--
-- ni = New Init (Result of previous step)
-- v1 = Visited list after processing Init
processForCond :: Either String (Ast, [String]) -> Ast -> Ast -> Ast ->
    IO (Either String ([Ast], [String]))
processForCond (Left err) _ _ _ = return (Left err)
processForCond (Right (ni, v1)) c u b = do
    resC <- processSingleNode v1 c
    processForUpdate resC ni u b

-- | Third step of For Loop: The Update/Increment.
--
-- nc = New Condition
-- v2 = Visited list after processing Condition
processForUpdate :: Either String (Ast, [String]) -> Ast -> Ast -> Ast ->
    IO (Either String ([Ast], [String]))
processForUpdate (Left err) _ _ _ = return (Left err)
processForUpdate (Right (nc, v2)) ni u b = do
    resU <- processSingleNode v2 u
    processForBody resU ni nc b

-- | Final step of For Loop: The Body & Reassembly.
--
-- nu = New Update
-- v3 = Visited list after processing Update
-- nb = New Body
-- v4 = Final Visited list
processForBody :: Either String (Ast, [String]) -> Ast -> Ast -> Ast ->
    IO (Either String ([Ast], [String]))
processForBody (Left err) _ _ _ = return (Left err)
processForBody (Right (nu, v3)) ni nc b = do
    resB <- processSingleNode v3 b
    return $ case resB of
        Left err -> Left err
        Right (nb, v4) -> Right ([AFor ni nc nu nb], v4)

-- | Start of If Statement processing.
--
-- c = Condition
-- t = Then block
-- e = Else block
processIf :: [String] -> Ast -> Ast -> Ast ->
    IO (Either String ([Ast], [String]))
processIf visited c t e = do
    resC <- processSingleNode visited c
    processIfThen resC t e

-- | Second step of If: The Then Branch.
--
-- nc = New Condition
-- v1 = Visited list after processing Condition
processIfThen :: Either String (Ast, [String]) -> Ast -> Ast ->
    IO (Either String ([Ast], [String]))
processIfThen (Left err) _ _ = return (Left err)
processIfThen (Right (nc, v1)) t e = do
    resT <- processSingleNode v1 t
    processIfElse resT nc e

-- | Final step of If: The Else Branch & Reassembly.
--
-- nt = New Then block
-- v2 = Visited list after processing Then
-- ne = New Else block
processIfElse :: Either String (Ast, [String]) -> Ast -> Ast ->
    IO (Either String ([Ast], [String]))
processIfElse (Left err) _ _ = return (Left err)
processIfElse (Right (nt, v2)) nc e = do
    resE <- processSingleNode v2 e
    return $ case resE of
        Left err -> Left err
        Right (ne, v3) -> Right ([AIf nc nt ne], v3)

-- | Processes Position nodes (APos).
--
-- l = Line number
-- c = Column number
processAPos :: [String] -> Int -> Int -> Ast ->
    IO (Either String ([Ast], [String]))
processAPos visited l c ast = do
    res <- processNode visited ast
    return $ case res of
        Left err -> Left err
        Right ([single], newVisited) ->
            Right ([APos l c single], newVisited)
        Right (multiple, newVisited) -> Right (multiple, newVisited)

-- | Process a node that is expected to be a single expression.
--
-- Handles unboxing of lists if necessary (e.g., resolving AVoid).
processSingleNode :: [String] -> Ast -> IO (Either String (Ast, [String]))
processSingleNode visited node = do
    res <- processNode visited node
    return $ case res of
        Left err -> Left err
        Right ([], newVisited) -> Right (AVoid, newVisited)
        Right ([x], newVisited) -> Right (x, newVisited)
        Right (x:_, newVisited) -> Right (x, newVisited)
