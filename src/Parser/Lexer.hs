{- 
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Lexer.hs
-}

{-|
Module      : Parser.Lexer
Description : Lexical analysis tools and basic parsers.
Stability   : stable

This module defines the basic building blocks for the parser, including:
- Space consumers (handling whitespace and comments)
- Lexeme wrappers
- Symbol and keyword parsers
- Reserved words management
-}
module Parser.Lexer (
    Parser,
    sc,
    lexeme,
    symbol,
    pKeyword,
    pIdentifier,
    parens,
    braces,
    semicolon,
    comma,
    colon
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Text as DT

-- | The standard Parser type for this project.
--
-- It uses Void as the custom error component (no custom errors)
-- and DT.Text as the input stream type.
type Parser = Parsec Void DT.Text

-- | Space Consumer.
--
-- Consumes whitespace and ignores comments.
-- Supported comment styles:
--
-- * Line comments starting with //
-- * Block comments enclosed in /* ... */
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment (DT.pack "//"))
    (L.skipBlockComment (DT.pack "/*") (DT.pack "*/"))

-- | Wrapper for lexemes.
--
-- Consumes trailing whitespace after parsing the given parser 'p'.
-- This is essential for the "trailing whitespace" convention used in this parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a fixed string as a symbol.
--
-- Consumes the string exactly as matches, followed by whitespace.
symbol :: DT.Text -> Parser DT.Text
symbol = L.symbol sc

-- | Parse content between parentheses (...).
parens :: Parser a -> Parser a
parens = between (symbol (DT.pack "(")) (symbol (DT.pack ")"))

-- | Parse content between braces {...}.
braces :: Parser a -> Parser a
braces = between (symbol (DT.pack "{")) (symbol (DT.pack "}"))

-- | Parse a semicolon ; symbol.
semicolon :: Parser DT.Text
semicolon = symbol (DT.pack ";")

-- | Parse a comma , symbol.
comma :: Parser DT.Text
comma = symbol (DT.pack ",")

-- | Parse a colon : symbol.
colon :: Parser DT.Text
colon = symbol (DT.pack ":")

-- | List of reserved keywords in the language.
--
-- These words cannot be used as variable or function identifiers.
reservedWords :: [DT.Text]
reservedWords = map DT.pack ["func", "ret", "if", "else", "int",
        "bool", "void", "while"]

-- | Parse a specific keyword.
--
-- Ensures that the keyword is not followed by an alphanumeric character
-- to prevent partial matches (e.g., matching "if" inside "ifelse").
pKeyword :: DT.Text -> Parser DT.Text
pKeyword w = lexeme (string w <* notFollowedBy alphaNumChar)

-- | Parse a valid identifier.
--
-- An identifier must start with a letter or underscore, followed by
-- alphanumeric characters or underscores.
--
-- It fails if the parsed identifier is one of the 'reservedWords'.
pIdentifier :: Parser DT.Text
pIdentifier = (lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let name = DT.pack (first : rest)
    if name `elem` reservedWords
        then fail $
            "Keyword '" ++ DT.unpack name ++ "' cannot be an identifier"
        else return name) <?> "identifier"
