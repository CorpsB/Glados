{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Expression.hs
-}

{-|
Module      : Parser.Expression
Description : Expression parser handling math, logic, and types.
Stability   : stable

This module handles the parsing of values and expressions using 'makeExprParser'.
It supports:
- Integers, Booleans, Strings, Chars, Lists
- Variables and Function calls
- Operator precedence (PEMDAS, etc.)
-}
module Parser.Expression (pExpr) where

import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Control.Monad.Combinators.Expr
import qualified Data.Text as DT
import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (fitInteger, IntValue(..))
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Lexer

-- | Parse a decimal integer.
--
-- Uses 'fitInteger' to automatically determine the smallest fitting
-- integer type (I8, I16, I32, I64).
pInteger :: Parser Ast
pInteger = (lexeme $ do
    val <- L.decimal
    return (AInteger (fitInteger val))) <?> "integer"

-- | Parse a boolean literal (True or False).
pBool :: Parser Ast
pBool = lexeme (choice
    [ ABool True <$ string (DT.pack "True")
    , ABool False <$ string (DT.pack "False")
    ]) <?> "boolean"

-- | Parse a string literal enclosed in double quotes.
--
-- Converts the string into a list of characters ([IChar]).
pString :: Parser Ast
pString = (lexeme $ do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    return $ AList (map (AInteger . IChar) content)) <?> "string"

-- | Parse a character literal enclosed in single quotes.
--
-- Example: 'c'
pChar :: Parser Ast
pChar = (lexeme $ do
    _ <- char '\''
    c <- L.charLiteral
    _ <- char '\''
    return (AInteger (IChar c))) <?> "character"

-- | Parse a list literal enclosed in brackets.
--
-- Example: [1, 2, True]
pListLiteral :: Parser Ast
pListLiteral = (do
    _ <- symbol (DT.pack "[")
    exprs <- pExpr `sepBy` comma
    _ <- symbol (DT.pack "]")
    return (AList exprs)) <?> "list"

-- | Parse a variable or a function call.
--
-- Distinguishes between the two by checking for parentheses after the identifier:
--
-- * myVar parses as ASymbol "myVar"
-- * myFunc(1, 2) parses as Call "myFunc" [1, 2]
pVarOrCall :: Parser Ast
pVarOrCall = do
    name <- pIdentifier
    choice
        [ do
            args <- parens (pExpr `sepBy` comma)
            return (Call (ASymbol name) args)
        , return (ASymbol name)
        ]

-- | Parse a term in an expression.
--
-- A term is the basic unit of an expression, such as literals,
-- variables, function calls, or parenthesized sub-expressions.
pTerm :: Parser Ast
pTerm = choice
    [ parens pExpr
    , pInteger
    , pBool
    , pChar
    , pString
    , pListLiteral
    , pVarOrCall
    ]

-- | Helper to create a binary operator AST node.
--
-- Transforms an infix operator string (e.g., "+") into a 'Call' AST node.
binary :: DT.Text -> (Ast -> Ast -> Ast)
binary name = \a b -> Call (ASymbol name) [a, b]

-- | Table of multiplicative operators (*, /, %).
multiplicativeOps :: [Operator Parser Ast]
multiplicativeOps =
    [ InfixL (binary (DT.pack "*") <$ symbol (DT.pack "*"))
    , InfixL (binary (DT.pack "div") <$ symbol (DT.pack "/"))
    , InfixL (binary (DT.pack "mod") <$ symbol (DT.pack "%"))
    ]

-- | Table of additive operators (+, -).
additiveOps :: [Operator Parser Ast]
additiveOps =
    [ InfixL (binary (DT.pack "+") <$ symbol (DT.pack "+"))
    , InfixL (binary (DT.pack "-") <$ symbol (DT.pack "-"))
    ]

-- | Table of comparison operators (==, <, >).
comparisonOps :: [Operator Parser Ast]
comparisonOps =
    [ InfixL (binary (DT.pack "eq?") <$ symbol (DT.pack "=="))
    , InfixL (binary (DT.pack "<")   <$ symbol (DT.pack "<"))
    , InfixL (binary (DT.pack ">")   <$ symbol (DT.pack ">"))
    ]

-- | Table for logical AND operator (&&).
-- It has higher precedence than OR but lower than comparison operators.
logicalAndOps :: [Operator Parser Ast]
logicalAndOps =
    [ InfixL (binary (DT.pack "&&") <$ symbol (DT.pack "&&")) ]

-- | Table for logical OR operator (||).
-- It has the lowest precedence among logical operators.
logicalOrOps :: [Operator Parser Ast]
logicalOrOps =
    [ InfixL (binary (DT.pack "||") <$ symbol (DT.pack "||")) ]

-- | Combined operator table for expression parsing.
--
-- Defines the precedence order: Multiplicative > Additive > Comparison > AND > OR.
opTable :: [[Operator Parser Ast]]
opTable = [multiplicativeOps, additiveOps, comparisonOps,
    logicalAndOps, logicalOrOps]

-- | Main expression parser.
--
-- Uses 'makeExprParser' to handle operator precedence and associativity automatically.
pExpr :: Parser Ast
pExpr = makeExprParser pTerm opTable
