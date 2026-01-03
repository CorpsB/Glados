{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module AST.Ast (Ast(..), Env, showAst, printAst) where

import Z_old.Src.Type.Integer (IntValue(..), intValueToInt)
import Data.Text as DT

type Env = [(DT.Text, Ast)]

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
      --   @param Ast The loop body.
      --   @param Ast The update/increment step.

    | AReturn Ast
      -- ^ Represents an explicit return statement.
      --   @param Ast The expression to return.

    deriving (Show, Eq)

showAst :: Ast -> String
showAst (AInteger i) = Prelude.show $ intValueToInt i
showAst (ABool True) = "#t"
showAst (ABool False) = "#f"
showAst (ASymbol s) = DT.unpack s
showAst (AList xs) = "(" ++ Prelude.unwords (Prelude.map showAst xs) ++ ")"
showAst (ADefineLambda _ _) = "#<lambda>"
showAst other = Prelude.show other
-- TO DO: add new AST lines

printAst :: Ast -> IO ()
printAst ast = putStrLn (Prelude.show ast)
-- TO DO: replace function by an AST tree view
