{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Expression.hs
-}

module Parser.Expression (pExpr) where

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import qualified Data.Text as DT
import Ast (Ast(..))
import Type.Integer (fitInteger)
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Lexer

pInteger :: Parser Ast
pInteger = (lexeme $ do
    val <- L.decimal
    return (AInteger (fitInteger val))) <?> "integer"

pVarOrCall :: Parser Ast
pVarOrCall = do
    name <- pIdentifier
    choice
        [ do
            args <- parens (pExpr `sepBy` comma)
            return (Call (ASymbol name) args)
        , return (ASymbol name)
        ]

pTerm :: Parser Ast
pTerm = choice
    [ parens pExpr
    , pInteger
    , pVarOrCall
    ]

binary :: DT.Text -> (Ast -> Ast -> Ast)
binary name = \a b -> Call (ASymbol name) [a, b]

multiplicativeOps :: [Operator Parser Ast]
multiplicativeOps =
    [ InfixL (binary (DT.pack "*") <$ symbol (DT.pack "*"))
    , InfixL (binary (DT.pack "div") <$ symbol (DT.pack "/"))
    , InfixL (binary (DT.pack "mod") <$ symbol (DT.pack "%"))
    ]

additiveOps :: [Operator Parser Ast]
additiveOps =
    [ InfixL (binary (DT.pack "+") <$ symbol (DT.pack "+"))
    , InfixL (binary (DT.pack "-") <$ symbol (DT.pack "-"))
    ]

comparisonOps :: [Operator Parser Ast]
comparisonOps =
    [ InfixL (binary (DT.pack "eq?") <$ symbol (DT.pack "=="))
    , InfixL (binary (DT.pack "<")   <$ symbol (DT.pack "<"))
    , InfixL (binary (DT.pack ">")   <$ symbol (DT.pack ">"))
    ]

opTable :: [[Operator Parser Ast]]
opTable = [multiplicativeOps, additiveOps, comparisonOps]

pExpr :: Parser Ast
pExpr = makeExprParser pTerm opTable
