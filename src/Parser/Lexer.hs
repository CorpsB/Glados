{- 
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Lexer.hs
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

type Parser = Parsec Void DT.Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment (DT.pack "//"))
    (L.skipBlockComment (DT.pack "/*") (DT.pack "*/"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: DT.Text -> Parser DT.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol (DT.pack "(")) (symbol (DT.pack ")"))

braces :: Parser a -> Parser a
braces = between (symbol (DT.pack "{")) (symbol (DT.pack "}"))

semicolon :: Parser DT.Text
semicolon = symbol (DT.pack ";")

comma :: Parser DT.Text
comma = symbol (DT.pack ",")

colon :: Parser DT.Text
colon = symbol (DT.pack ":")

reservedWords :: [DT.Text]
reservedWords = map DT.pack ["func", "ret", "if", "else", "int",
        "bool", "void", "while"]

pKeyword :: DT.Text -> Parser DT.Text
pKeyword w = lexeme (string w <* notFollowedBy alphaNumChar)

pIdentifier :: Parser DT.Text
pIdentifier = (lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let name = DT.pack (first : rest)
    if name `elem` reservedWords
        then fail $
            "Keyword '" ++ DT.unpack name ++ "' cannot be an identifier"
        else return name) <?> "identifier"
