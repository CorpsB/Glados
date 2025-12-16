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

data Ast
    = AInteger IntValue
    | ABool Bool
    | ASymbol DT.Text
    | AVoid
    | Condition Ast Ast Ast
    | Define DT.Text DT.Text Ast
    | DefineFun DT.Text [(DT.Text, DT.Text)] DT.Text Ast-- (define f [a,b] exec)
    | Call Ast [Ast]
    | Lambda [DT.Text] Ast
    | Closure [DT.Text] Ast Env
    | AList [Ast]
    | Import DT.Text
    | While Ast Ast
    deriving Show

showAst :: Ast -> String
showAst (AInteger i) = Prelude.show $ intValueToInt i
showAst (ABool True) = "#t"
showAst (ABool False) = "#f"
showAst (ASymbol s) = DT.unpack s
showAst (AList xs) = "(" ++ Prelude.unwords (Prelude.map showAst xs) ++ ")"
showAst (Closure _ _ _) = "#\\<procedure\\>"
showAst (Lambda _ _) = "#<lambda>"
showAst other = Prelude.show other
-- TO DO: add new AST lines

printAst :: Ast -> IO ()
printAst ast = putStrLn (Prelude.show ast)
-- TO DO: replace function by an AST tree view
