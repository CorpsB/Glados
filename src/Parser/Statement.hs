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
import Data.Char (ord)
import Common.Type.Integer (IntValue(..))

-- | Intermediate representation for assignment targets
-- Allows handling chains like x.y[0].z = 10
data Accessor
    = AccIndex Ast      --  Array index: [expr]
    | AccField DT.Text  --  Struct field: .name

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
    return (AReturn val)

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
    return (ADefineFunc name args retType body)

-- | Helper to construct a binary operator call for compound assignments.
--
-- Used for +=, -=, *=, /= to transform 'x += 1' into 'x = x + 1'.
makeOpCall :: DT.Text -> DT.Text -> Ast -> Ast
makeOpCall op name expr = ACall (ASymbol op) [ASymbol name, expr]

-- | Parse assignment operators and return a transformation function.
--
-- Supports:
-- * Standard assignment (=) -> returns identity
-- * Compound assignment (+=, -=, *=, /=) -> returns transformation logic
pAssignOp :: DT.Text -> Parser (Ast -> Ast)
pAssignOp name = choice
    [ (\e -> e) <$ symbol (DT.pack "=")
    , makeOpCall (DT.pack "+") name <$ symbol (DT.pack "+=")
    , makeOpCall (DT.pack "-") name <$ symbol (DT.pack "-=")
    , makeOpCall (DT.pack "*") name <$ symbol (DT.pack "*=")
    , makeOpCall (DT.pack "div") name <$ symbol (DT.pack "/=")
    ]

-- | Converts a field name (Text) into an AST list of integers.
-- This allows passing the field name as an argument to the 'set_field' runtime function.
fieldToAst :: DT.Text -> Ast
fieldToAst txt = AList $ map charToAst (DT.unpack txt)
    where
        charToAst c = AInteger (IChar (fromIntegral (ord c)))

-- | Recursively constructs the chain of 'update' and 'set_field' calls.
-- This function handles nested modifications for both arrays and structures.
recursiveUpdate :: Ast -> [Accessor] -> Ast -> Ast
recursiveUpdate base [AccIndex idx] val =
    -- Case 1: End of chain on an Array (e.g. x[i] = val)
    ACall (ASymbol (DT.pack "update")) [base, idx, val]

-- Case 2: End of chain on a Structure (e.g. x.field = val)
recursiveUpdate base [AccField field] val =
    ACall (ASymbol (DT.pack "set_field")) [base, fieldToAst field, val]

-- Case 3: Recursion on Array (e.g. x[i]... = val)
-- We fetch the inner element using 'nth', update it recursively, and put it back using 'update'.
recursiveUpdate base (AccIndex idx : rest) val =
    let inner = ACall (ASymbol (DT.pack "nth")) [base, idx]
        newVal = recursiveUpdate inner rest val
    in ACall (ASymbol (DT.pack "update")) [base, idx, newVal]

-- Case 4: Recursion on Structure (e.g. x.field... = val)
-- We access the field, update it recursively, and put it back using 'set_field'.
recursiveUpdate base (AccField field : rest) val =
    let inner = AAccessStruct base field
        newVal = recursiveUpdate inner rest val
    in ACall (ASymbol (DT.pack "set_field")) 
       [base, fieldToAst field, newVal]

recursiveUpdate _ [] _ = error "Should not happen in buildUpdateChain"

-- | Entry point to construct a chain of updates for arrays and structures.
-- Transforms a complex assignment like `x.y[0] = 5` into nested function calls.
buildUpdateChain :: DT.Text -> [Accessor] -> Ast -> Ast
buildUpdateChain name accessors finalVal =
        recursiveUpdate (ASymbol name) accessors finalVal

-- | Parse a standard variable definition or assignment.
--
-- Handles optional type annotation.
-- Example: x: int = 10; OR x = 10;
pSimpleDef :: DT.Text -> Parser Ast
pSimpleDef name = do
    varType <- optional (symbol (DT.pack ":") >> pType)
    makeValue <- pAssignOp name
    val <- pExpr
    _ <- semicolon <?> "\";\" at the end of statement"
    let finalType = maybe (DT.pack "auto") id varType
    return (ASetVar name finalType (makeValue val))

-- | Parse a complex update (array or struct).
pComplexUpdate :: DT.Text -> [Accessor] -> Parser Ast
pComplexUpdate name accessors = do
    _ <- symbol (DT.pack "=")
    val <- pExpr
    _ <- semicolon
    let updateExpr = buildUpdateChain name accessors val
    return (ASetVar name (DT.pack "auto") updateExpr)

-- | Parser for array index accessor: [expr]
pAccIndex :: Parser Accessor
pAccIndex = do
    _ <- symbol (DT.pack "[")
    expr <- pExpr
    _ <- symbol (DT.pack "]")
    return (AccIndex expr)

-- | Parser for struct field accessor: .identifier
pAccField :: Parser Accessor
pAccField = do
    _ <- symbol (DT.pack ".")
    field <- pIdentifier
    return (AccField field)

-- | Parse a variable definition or assignment.
-- Handles mixed chains: x = 1, x[0] = 1, x.y = 1, x.y[0].z = 1
pVarDef :: Parser Ast
pVarDef = do
    name <- pIdentifier
    -- Parse mixed chain of [indices] and .fields
    accessors <- many (pAccIndex <|> pAccField)
    if null accessors
        then pSimpleDef name
        else pComplexUpdate name accessors

-- | Parse a single field definition within a structure.
--
-- Syntax: fieldName: type;
pStructField :: Parser (DT.Text, DT.Text)
pStructField = do
    name <- pIdentifier
    _ <- colon
    fType <- pType
    _ <- semicolon
    return (name, fType)

-- | Parse a structure definition.
--
-- Syntax: struct Name { fields... }
pStruct :: Parser Ast
pStruct = do
    _ <- pKeyword (DT.pack "struct")
    name <- pIdentifier
    fields <- braces (many pStructField)
    return (ADefineStruct name fields)

-- | Parse the file path string for an import.
--
-- Expects a string enclosed in double quotes.
pImportPath :: Parser DT.Text
pImportPath = lexeme $ do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    return (DT.pack content)

-- | Parse an import directive.
--
-- Syntax: import "path/to/file";
pImport :: Parser Ast
pImport = do
    _ <- pKeyword (DT.pack "import")
    path <- pImportPath
    _ <- semicolon
    return (AImport path)

-- | Group of control flow parsers (If, While, For).
pControlFlow :: [Parser Ast]
pControlFlow =
    [ try (pIf pVarDef pBlock)
    , try (pWhile pBlock)
    , try (pFor pVarDef pBlock)
    ]

-- | Group of top-level declaration parsers (Import, Struct, Func).
pDeclarations :: [Parser Ast]
pDeclarations =
    [ pImport
    , pStruct
    , pFunc
    ]

-- | Group of basic statement parsers (Return, Variable, Expression).
pBasic :: [Parser Ast]
pBasic =
    [ pReturn
    , try pVarDef 
    , pExpr <* (semicolon <?> "\";\" after expression") 
    ]

-- | Main statement parser.
--
-- Aggregates all statement types (control flow, declarations, basic instructions)
-- into a single choice. This is the top-level parser for a line of code.
pStatement :: Parser Ast
pStatement = withPos $ choice (pControlFlow ++ pDeclarations ++ pBasic)

-- | Main entry point for the parser.
--
-- Parses a list of statements from the input text until EOF.
-- The filename argument is used for error reporting.
parseALL :: DT.Text -> Either (ParseErrorBundle DT.Text Void) [Ast]
parseALL = parse (sc *> many pStatement <* eof) "ParseALL"
