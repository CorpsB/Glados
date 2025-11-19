-- 
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParserISL.hs
--

module Parser.ParserISL (parseLisp) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Lisp (SExpr(..))

type Parser = Parsec Void String


sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment ";")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pInteger :: Parser SExpr
pInteger = lexeme $ do
    val <- L.signed (return ()) L.decimal
    return (SInteger val)

pExpr :: Parser SExpr
pExpr = try pInteger

parseLisp :: String -> Either (ParseErrorBundle String Void) SExpr
parseLisp = parse (sc *> pExpr <* eof) "glados"
