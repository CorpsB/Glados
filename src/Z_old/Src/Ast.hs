{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Z_old.Src.Ast (OldAst(..), OldEnv, showAst, printAst) where

import Z_old.Src.Type.Integer (IntValue(..), intValueToInt)
import Data.Text as DT

type OldEnv = [(DT.Text, OldAst)]

data OldAst
    = AInteger IntValue
    | ABool Bool
    | ASymbol DT.Text
    | AVoid
    | Condition OldAst OldAst OldAst
    | Define DT.Text DT.Text OldAst
    | DefineFun DT.Text [(DT.Text, DT.Text)] DT.Text OldAst-- (define f [a,b] exec)
    | Call OldAst [OldAst]
    | Lambda [DT.Text] OldAst
    | Closure [DT.Text] OldAst OldEnv
    | AList [OldAst]
    | Import DT.Text
    | While OldAst OldAst
    | For OldAst OldAst OldAst OldAst
    | Struct DT.Text [(DT.Text, DT.Text)]
    | New DT.Text [(DT.Text, OldAst)]
    | Return OldAst
    deriving Show

showAst :: OldAst -> String
showAst (AInteger i) = Prelude.show $ intValueToInt i
showAst (ABool True) = "#t"
showAst (ABool False) = "#f"
showAst (ASymbol s) = DT.unpack s
showAst (AList xs) = "(" ++ Prelude.unwords (Prelude.map showAst xs) ++ ")"
showAst (Closure _ _ _) = "#\\<procedure\\>"
showAst (Lambda _ _) = "#<lambda>"
showAst other = Prelude.show other

printAst :: OldAst -> IO ()
printAst ast = putStrLn (Prelude.show ast)
