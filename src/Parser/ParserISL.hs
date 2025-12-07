{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParserISL.hs
-}

module Parser.ParserISL (parseLisp, parseLispLine) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Lisp (SExpr(..))

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pInteger :: Parser SExpr
pInteger = (lexeme $ do
    val <- L.signed (return ()) L.decimal
    return (SInteger val)) <?> "Expected integer (like: 42, -10)" 

pSymbol :: Parser SExpr
pSymbol = (lexeme $ do
    first <- letterChar <|> oneOf "+-*/<>=?!_#"
    rest <- many (alphaNumChar <|> oneOf "+-*/<>=?!_#")
    return (SSymbol (first : rest)))
      <?> "Expected symbol (like: define, +, foo)"

pList :: Parser SExpr
pList = (do
    _ <- symbol "("
    exprs <- many pExpr
    _ <- symbol ")" <?> "Expected closing paranthesis ')'"
    return (List exprs)) <?> "Expected list"

pExpr :: Parser SExpr
pExpr = try pInteger
    <|> pList
    <|> pSymbol

parseLisp :: String -> Either (ParseErrorBundle String Void) [SExpr]
parseLisp = parse (sc *> many pExpr <* eof) "ParserISL"

parseLispLine :: String -> Either (ParseErrorBundle String Void) SExpr
parseLispLine = parse (sc *> pExpr <* sc) "REPL"
