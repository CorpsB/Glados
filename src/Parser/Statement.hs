{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Statement Parser (Functions, Returns, Types)
-}

module Parser.Statement (parseALL, pStatement) where

import Text.Megaparsec
import qualified Data.Text as DT
import Ast (Ast(..))
import Parser.Lexer
import Parser.Expression (pExpr)
import Data.Void (Void)

pType :: Parser DT.Text
pType = choice
    [ pKeyword (DT.pack "int")
    , pKeyword (DT.pack "bool")
    , pKeyword (DT.pack "void")
    , pIdentifier
    ] <?> "type"

pArgDeclaration :: Parser (DT.Text, DT.Text)
pArgDeclaration = do
    name <- pIdentifier
    _ <- colon
    t <- pType
    return (name, t)

pReturn :: Parser Ast
pReturn = do
    _ <- pKeyword (DT.pack "ret")
    val <- pExpr
    _ <- semicolon
    return val

pBlock :: Parser Ast
pBlock = braces $ do
    stmts <- many pStatement
    case stmts of
        [] -> fail "Empty function body not supported yet"
        xs -> return (last xs)

pFunc :: Parser Ast
pFunc = do
    _ <- pKeyword (DT.pack "func")
    name <- pIdentifier
    args <- parens (pArgDeclaration `sepBy` comma)
    maybeRetType <- optional (symbol (DT.pack "->") >> pType)
    let retType = case maybeRetType of
            Just t  -> t
            Nothing -> DT.pack "Void"
    body <- pBlock
    return (DefineFun name args retType body)

pVarDef :: Parser Ast
pVarDef = do
    _ <- pKeyword (DT.pack "var")
    name <- pIdentifier
    _ <- symbol (DT.pack "=")
    val <- pExpr
    _ <- semicolon
    return (Define name val)

pStatement :: Parser Ast
pStatement = choice
    [ pFunc
    , pReturn
    , pVarDef
    , pExpr <* semicolon 
    ]

parseALL :: DT.Text -> Either (ParseErrorBundle DT.Text Void) [Ast]
parseALL = parse (sc *> many pStatement <* eof) "ParseALL"
