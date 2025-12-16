{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParserISL.hs
-}

module Z_old.Src.Parser.ParserISL (parseLisp, parseLispLine) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Z_old.Src.Lisp (SExpr(..))
import qualified Data.Text as DT

type Parser = Parsec Void DT.Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment (DT.pack ";")) empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: DT.Text -> Parser DT.Text
symbol = L.symbol sc

pInteger :: Parser SExpr
pInteger = (lexeme $ do
    val <- L.signed (return ()) L.decimal
    return (SInteger val)) <?> "Expected integer" 

pSymbol :: Parser SExpr
pSymbol = (lexeme $ do
    first <- letterChar <|> oneOf "+-*/<>=?!_#"
    rest <- many (alphaNumChar <|> oneOf "+-*/<>=?!_#")
    return (SSymbol (DT.pack (first : rest))))
      <?> "Expected symbol"

pList :: Parser SExpr
pList = (do
    _ <- symbol (DT.pack "(")
    exprs <- many pExpr
    _ <- symbol (DT.pack ")") <?> "Expected closing paranthesis"
    return (List exprs)) <?> "Expected list"

pExpr :: Parser SExpr
pExpr = try pInteger
    <|> pList
    <|> pSymbol

parseLisp :: DT.Text -> Either (ParseErrorBundle DT.Text Void) [SExpr]
parseLisp = parse (sc *> many pExpr <* eof) "ParserISL"

parseLispLine :: DT.Text -> Either (ParseErrorBundle DT.Text Void) SExpr
parseLispLine = parse (sc *> pExpr <* sc) "REPL"
