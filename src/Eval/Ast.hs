{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Eval.Ast (evalAST, evalASTEnv) where

import Ast (Ast(..))
import Eval.Builtins (execBuiltin)
import Eval.Functions (getFunction, FuncTable, Env)
import Utils.List (sameLength)

evalAST :: FuncTable -> Env -> Ast -> Maybe Ast
evalAST _ _ (AInteger n) = Just (AInteger n)
evalAST _ _ (ABool b) = Just (ABool b)
evalAST _ env (ASymbol s) = lookupEnv env s
evalAST ftable env (Define name body) = do
    b2 <- evalAST ftable env body
    return (Define name b2)
evalAST _ _ (DefineFun name params body) = Just (DefineFun name params body)
evalAST ftable env (Call (ASymbol op) args) = do
    evalArgs <- traverse (evalASTEnv ftable env) args
    case execBuiltin op evalArgs of
        Just r -> Just r
        Nothing -> case getFunction ftable op of
            Just (params, body) | Utils.List.sameLength params evalArgs ->
                let localEnv = zip params evalArgs ++ env in
                evalASTEnv ftable localEnv body
            _ -> Nothing
evalAST _ _ (Call _ _) = Nothing

lookupEnv :: Env -> String -> Maybe Ast
lookupEnv [] _ = Nothing
lookupEnv ((k,v):xs) key
    | k == key  = Just v
    | otherwise = lookupEnv xs key

evalASTEnv :: FuncTable -> Env -> Ast -> Maybe Ast
evalASTEnv _ _ (AInteger n) = Just (AInteger n)
evalASTEnv _ _ (ABool b) = Just (ABool b)
evalASTEnv _ env (ASymbol s) = lookupEnv env s
evalASTEnv ftable env (Define name body) =
    evalAST ftable env (Define name body)
evalASTEnv ftable env (DefineFun name params body) =
    evalAST ftable env (DefineFun name params body)
evalASTEnv ftable env (Call f args) = evalAST ftable env (Call f args)
