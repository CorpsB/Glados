{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module AST.Ast (Ast(..), showAst, printAst, cleanAst) where

import Common.Type.Integer (IntValue(..), intValueToInt)
import qualified Data.Text as DT

-- | Abstract Syntax Tree (AST) definition for the GLADOS language.
--
-- @details
--   The AST represents the hierarchical structure of the parsed source code.
--   Each constructor corresponds to a specific syntactic construct in
--   the language, such as literals, definitions, control flow, and
--   function calls.
--
data Ast
    = AInteger IntValue
      -- ^ Represents an integer literal.
      --   @param IntValue The value of the integer.

    | ABool Bool
      -- ^ Represents a boolean literal.
      --   @param Bool True or False.

    | ASymbol DT.Text
      -- ^ Represents a symbol (identifier).
      --   Used for variable names, function names, or reference lookups.
      --   @param Text The name of the symbol.

    | AVoid
      -- ^ Represents a Void or Null value (no return value).

    | AList [Ast]
      -- ^ Represents a generic list of AST nodes.
      --   Can be used for list literals or sequence of expressions.
      --   @param [Ast] The elements of the list.

    | ADefineFunc DT.Text [(DT.Text, DT.Text)] DT.Text Ast
      -- ^ Represents a named function definition.
      --   @param Text The name of the function.
      --   @param [(Text, Text)] The list of arguments (Name, Type).
      --   @param Text The return type of the function.
      --   @param Ast The body of the function.

    | ADefineLambda [DT.Text] Ast
      -- ^ Represents an anonymous function (Lambda).
      --   @param [Text] The list of parameter names.
      --   @param Ast The body of the lambda.

    | ADefineStruct DT.Text [(DT.Text, DT.Text)]
      -- ^ Represents a structure type definition.
      --   @param Text The name of the structure.
      --   @param [(Text, Text)] The list of fields (Name, Type).

    | ASetVar DT.Text DT.Text Ast
      -- ^ Represents a variable definition or assignment.
      --   @param Text The name of the variable.
      --   @param Text The type of the variable.
      --   @param Ast The value assigned to the variable.

    | ASetStruct DT.Text [(DT.Text, Ast)]
      -- ^ Represents a structure definition statement wrapper.
      --   @param ADefineStruct The structure definition node.

    | ACall Ast [Ast]
      -- ^ Represents a function call (Application).
      --   @param Ast The callee (function expression or symbol).
      --   @param [Ast] The list of arguments passed to the function.

    | AAccessStruct Ast DT.Text
      -- ^ Represents a field access (e.g., player.x).
      --   @param Ast The object/structure being accessed.
      --   @param Text The name of the field.

    | AImport DT.Text
      -- ^ Represents an import statement.
      --   @param Text The name of the module or file to import.

    | AIf Ast Ast Ast
      -- ^ Represents a conditional control flow (If-Then-Else).
      --   @param Ast The condition expression.
      --   @param Ast The "Then" branch.
      --   @param Ast The "Else" branch.

    | AWhile Ast Ast
      -- ^ Represents a While loop.
      --   @param Ast The loop condition.
      --   @param Ast The loop body.

    | AFor Ast Ast Ast Ast
      -- ^ Represents a For loop.
      --   @param Ast The initialization step.
      --   @param Ast The loop condition.
      --   @param Ast The update/increment step.
      --   @param Ast The loop body.

    | AReturn Ast
      -- ^ Represents an explicit return statement.
      --   @param Ast The expression to return.

    | AExprStmt Ast

    | ABlock [Ast]

    | APos Int Int Ast
      -- ^ Represents a source code position wrapper.
      --   Used for precise error reporting (line, column).
      --   This node wraps another AST node without changing its semantics.
      --   @param Int The line number.
      --   @param Int The column number.
      --   @param Ast The wrapped AST node.

    deriving (Show, Eq)

showAst :: Ast -> String
showAst (AInteger i) = Prelude.show $ intValueToInt i
showAst (ABool True) = "#t"
showAst (ABool False) = "#f"
showAst (ASymbol s) = DT.unpack s
showAst (AList xs) = "(" ++ Prelude.unwords (Prelude.map showAst xs) ++ ")"
showAst (ADefineLambda _ _) = "#<lambda>"
showAst (APos _ _ ast) = showAst ast
showAst (ABlock xs) = "{ " ++ unwords (map showAst xs) ++ " }"
showAst other = Prelude.show other
-- TO DO: add new AST lines

printAst :: Ast -> IO ()
printAst ast = putStrLn (Prelude.show ast)
-- TO DO: replace function by an AST tree view

-- | Recursively removes all position wrappers (APos) from the AST.
--
-- This is useful for:
-- Pretty printing (to avoid cluttering the output with position data).
-- Testing (to compare AST structure without worrying about line numbers).
--
-- @param Ast The AST potentially containing APos nodes.
-- @return Ast The cleaned AST with purely structural nodes.
cleanAst :: Ast -> Ast
cleanAst (APos _ _ ast) = cleanAst ast
cleanAst (ABlock xs) = ABlock (map cleanAst xs)
cleanAst (AList xs) = AList (map cleanAst xs)
cleanAst (ADefineFunc n a r b) = ADefineFunc n a r (cleanAst b)
cleanAst (ADefineLambda args body) = ADefineLambda args (cleanAst body)
cleanAst (ASetVar n t expr) = ASetVar n t (cleanAst expr)
cleanAst (ASetStruct n fields) =
    ASetStruct n (map (\(f, a) -> (f, cleanAst a)) fields)
cleanAst (ACall func args) = ACall (cleanAst func) (map cleanAst args)
cleanAst (AIf c t e) = AIf (cleanAst c) (cleanAst t) (cleanAst e)
cleanAst (AWhile c b) = AWhile (cleanAst c) (cleanAst b)
cleanAst (AFor i c u b) =
    AFor (cleanAst i) (cleanAst c) (cleanAst u) (cleanAst b)
cleanAst (AReturn e) = AReturn (cleanAst e)
cleanAst (AExprStmt e) = AExprStmt (cleanAst e)
cleanAst (AAccessStruct obj field) = AAccessStruct (cleanAst obj) field
cleanAst other = other
