{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Condition Parser (If, Loops, etc.)
-}

{-|
Module      : Parser.Condition
Description : Parser for control flow structures.
Stability   : stable
This module defines parsers for control flow:
- Conditional statements (if/else)
- Iteration statements (while)
Note: Parsers in this module often require 'pBlock' and 'pVarDef' as arguments
to avoid circular dependencies with Parser.Statement.
-}
module Parser.Conditions (
    pIf,
    pWhile
) where

import Text.Megaparsec
import qualified Data.Text as DT
import AST.Ast (Ast(..))
import Parser.Lexer
import Parser.Expression (pExpr)

-- | Parse the content inside the 'if' parentheses.
pIfCondition :: Parser Ast -> Parser (Maybe Ast, Ast)
pIfCondition pVarDefParam = parens $ do
    maybeInit <- optional (try pVarDefParam <|> try (pExpr <* semicolon))
    condition <- pExpr
    return (maybeInit, condition)

-- | Parse the optional 'else' branch.
-- Returns 'AVoid' if no else block is found.
pElseBranch :: Parser Ast -> Parser Ast -> Parser Ast
pElseBranch pVarDefParam pBlockParam = option AVoid $ do
    _ <- pKeyword (DT.pack "else")
    try (pIf pVarDefParam pBlockParam) <|> pBlockParam

-- | Construct the final Condition AST.
-- Handles the optional initialization wrapper.
buildIfAst :: Maybe Ast -> Ast -> Ast -> Ast -> Ast
buildIfAst initStmt cond thenB elseB =
    let ifNode = Condition cond thenB elseB
    in case initStmt of
        Just initInstr -> AList [initInstr, ifNode]
        Nothing        -> ifNode

-- | Parse a full conditional statement: if, else if, else.
--
-- This function uses Dependency Injection for 'pVarDef' and 'pBlock'.
pIf :: Parser Ast -> Parser Ast -> Parser Ast
pIf pVarDefParam pBlockParam = do
    _ <- pKeyword (DT.pack "if")
    (initStmt, cond) <- pIfCondition pVarDefParam
    thenBlock <- pBlockParam
    elseBlock <- pElseBranch pVarDefParam pBlockParam
    return $ buildIfAst initStmt cond thenBlock elseBlock

-- | Parse a while loop.
--
-- Syntax: while (condition) { body }
-- The body parsing relies on the injected 'pBlockParam'.
pWhile :: Parser Ast -> Parser Ast
pWhile pBlockParam = do
    _ <- pKeyword (DT.pack "while")
    cond <- parens pExpr
    body <- pBlockParam
    return (While cond body)
