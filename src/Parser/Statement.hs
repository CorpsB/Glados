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
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
import AST.Ast (Ast(..))
import Parser.Lexer
import Parser.Expression (pExpr)
import Data.Void (Void)
import Parser.Conditions (pIf, pWhile, pFor)

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
-- Returns an AList containing all statements, or AVoid if the block is empty.
pBlock :: Parser Ast
pBlock = braces $ do
    stmts <- many pStatement
    case stmts of
        [] -> return AVoid
        xs -> return (AList xs)

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

makeOpCall :: DT.Text -> DT.Text -> Ast -> Ast
makeOpCall op name expr = Call (ASymbol op) [ASymbol name, expr]

pAssignOp :: DT.Text -> Parser (Ast -> Ast)
pAssignOp name = choice
    [ (\e -> e) <$ symbol (DT.pack "=")
    , makeOpCall (DT.pack "+") name <$ symbol (DT.pack "+=")
    , makeOpCall (DT.pack "-") name <$ symbol (DT.pack "-=")
    , makeOpCall (DT.pack "*") name <$ symbol (DT.pack "*=")
    , makeOpCall (DT.pack "div") name <$ symbol (DT.pack "/=")
    ]

buildUpdateChain :: DT.Text -> [Ast] -> Ast -> Ast
buildUpdateChain name indices finalVal =
        foldUpdate (ASymbol name) indices finalVal
    where
        foldUpdate base [idx] val = 
            Call (ASymbol (DT.pack "update")) [base, idx, val]
        foldUpdate base (idx:rest) val =
            let inner = Call (ASymbol (DT.pack "nth")) [base, idx]
                newVal = foldUpdate inner rest val
            in Call (ASymbol (DT.pack "update")) [base, idx, newVal]
        foldUpdate _ [] _ = error "Should not happen in buildUpdateChain"


pSimpleDef :: DT.Text -> Parser Ast
pSimpleDef name = do
    varType <- optional (symbol (DT.pack ":") >> pType)
    makeValue <- pAssignOp name
    val <- pExpr
    _ <- semicolon
    let finalType = maybe (DT.pack "auto") id varType
    return (Define name finalType (makeValue val))

pArrayUpdate :: DT.Text -> [Ast] -> Parser Ast
pArrayUpdate name indices = do
    _ <- symbol (DT.pack "=")
    val <- pExpr
    _ <- semicolon
    let updateExpr = buildUpdateChain name indices val
    return (Define name (DT.pack "auto") updateExpr)

-- | Parse a variable definition (declaration or assignment).
--
-- Syntax: name: type = value; or name = value;
-- The type annotation is optional and defaults to "auto" if omitted.
pVarDef :: Parser Ast
pVarDef = do
    name <- pIdentifier
    indices <- many (symbol (DT.pack "[") *> pExpr <* symbol (DT.pack "]"))
    if null indices
        then pSimpleDef name
        else pArrayUpdate name indices

pStructField :: Parser (DT.Text, DT.Text)
pStructField = do
    name <- pIdentifier
    _ <- colon
    fType <- pType
    _ <- semicolon
    return (name, fType)

pStruct :: Parser Ast
pStruct = do
    _ <- pKeyword (DT.pack "struct")
    name <- pIdentifier
    fields <- braces (many pStructField)
    return (Struct name fields)

pImportPath :: Parser DT.Text
pImportPath = lexeme $ do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    return (DT.pack content)

pImport :: Parser Ast
pImport = do
    _ <- pKeyword (DT.pack "import")
    path <- pImportPath
    _ <- semicolon
    return (Import path)

pControlFlow :: [Parser Ast]
pControlFlow =
    [ try (pIf pVarDef pBlock)
    , try (pWhile pBlock)
    , try (pFor pVarDef pBlock)
    ]

pDeclarations :: [Parser Ast]
pDeclarations =
    [ pImport
    , pStruct
    , pFunc
    ]

pBasic:: [Parser Ast]
pBasic =
    [ pReturn
    , try pVarDef 
    , pExpr <* semicolon 
    ]

pStatement :: Parser Ast
pStatement = choice (pControlFlow ++ pDeclarations ++ pBasic)

-- | Main entry point for the parser.
--
-- Parses a list of statements from the input text until EOF.
-- The filename argument is used for error reporting.
parseALL :: DT.Text -> Either (ParseErrorBundle DT.Text Void) [Ast]
parseALL = parse (sc *> many pStatement <* eof) "ParseALL"
