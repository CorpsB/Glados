{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Statement Parser (Functions, Returns, Types)
-}

{-|
Module      : Parser.Statement
Description : Parser for language statements and declarations.
Stability   : stable

This module defines parsers for the structural elements of the language:
- Variable definitions
- Function declarations
- Return statements
- Type annotations
- Code blocks
-}
module Parser.Statement (parseALL, pStatement) where

import Text.Megaparsec
import qualified Data.Text as DT
import AST.Ast (Ast(..))
import Parser.Lexer
import Parser.Expression (pExpr)
import Data.Void (Void)

-- | Parse a list type syntax (e.g., [int]).
--
-- Recursively parses the inner type.
pListType :: Parser DT.Text
pListType = do
    _ <- symbol (DT.pack "[")
    innerType <- pType
    _ <- symbol (DT.pack "]")
    return (DT.pack "[" <> innerType <> DT.pack "]")

-- | Parse a type annotation.
--
-- Supports:
-- * Primitive types: int, bool, void
-- * List types: [int], [[char]]
-- * Custom types (via identifiers)
pType :: Parser DT.Text
pType = choice
    [ pListType
    , try (pKeyword (DT.pack "int"))
    , try (pKeyword (DT.pack "bool"))
    , try (pKeyword (DT.pack "void"))
    , pIdentifier
    ] <?> "type"

-- | Parse a function argument declaration (name and type).
--
-- Example: x: int
pArgDeclaration :: Parser (DT.Text, DT.Text)
pArgDeclaration = do
    name <- pIdentifier
    _ <- colon
    t <- pType
    return (name, t)

-- | Parse a return statement.
--
-- Example: ret 10;
pReturn :: Parser Ast
pReturn = do
    _ <- pKeyword (DT.pack "ret")
    val <- pExpr
    _ <- semicolon
    return val

-- | Parse a block of code enclosed in braces.
--
-- Returns the result of the last statement in the block (implicit return).
-- Fails if the block is empty.
pBlock :: Parser Ast
pBlock = braces $ do
    stmts <- many pStatement
    case stmts of
        [] -> fail "Empty function body not supported yet"
        xs -> return (last xs)

-- | Parse a function definition.
--
-- Syntax: func name(arg1: type, ...) -> retType { ... }
-- The return type is optional and defaults to "Void" if omitted.
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

-- | Parse a variable definition.
--
-- Syntax: name: type = value;
-- The type annotation is optional and defaults to "undefined" if omitted.
pVarDef :: Parser Ast
pVarDef = do
    name <- pIdentifier
    varType <- option (DT.pack "undefined") (symbol (DT.pack ":") >> pType)
    _ <- symbol (DT.pack "=")
    val <- pExpr
    _ <- semicolon
    return (Define name varType val)

-- | Parse a single statement.
--
-- Attempts to parse in order:
-- 1. Function definition
-- 2. Return statement
-- 3. Variable definition (requires 'try' due to ambiguity with expressions)
-- 4. Standalone expression (ending with semicolon)
pStatement :: Parser Ast
pStatement = choice
    [ pFunc
    , pReturn
    , try pVarDef 
    , pExpr <* semicolon 
    ]

-- | Main entry point for the parser.
--
-- Parses a list of statements from the input text until EOF.
-- The filename argument is used for error reporting.
parseALL :: DT.Text -> Either (ParseErrorBundle DT.Text Void) [Ast]
parseALL = parse (sc *> many pStatement <* eof) "ParseALL"
