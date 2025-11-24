{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Eval.Ast (evalAST, evalASTEnv) where

import Ast (Ast(..))
import Eval.Builtins (execBuiltin)
import Eval.Conditions (execCondition)
import Eval.Functions (getFunction, FuncTable, Env)
import Utils.List (sameLength)

execUserFunc :: FuncTable -> Env -> String -> [Ast] -> String -> Either String Ast
execUserFunc ft env op args err = case getFunction ft op of
    Just (p, b) | sameLength p args ->
        evalASTEnv ft (zip p args ++ env) b
    Just _ -> Left $ "*** ERROR: Argument length " ++
        "missmatch for function " ++ op
    Nothing -> Left err

evalAST :: FuncTable -> Env -> Ast -> Either String Ast
evalAST _ _ (AInteger n) = Right $ AInteger n
evalAST _ _ (ABool b) = Right $ ABool b
evalAST _ env (ASymbol s) = case lookupEnv env s of
    Just v -> Right v
    Nothing -> Left $ "*** ERROR: Undefined symbol: " ++ s
evalAST ftable env (Define name body) = do
    b2 <- evalAST ftable env body
    Right $ Define name b2
evalAST _ _ (DefineFun name params body) = Right $ DefineFun name params body
evalAST ftable env (Condition cond th el) = do
    c <- evalAST ftable env cond
    t <- evalAST ftable env th
    e <- evalAST ftable env el
    execCondition (Just c) (Just t) (Just e) >>= Right
evalAST ftable env (Call (ASymbol op) args) = do
    evalArgs <- traverse (evalASTEnv ftable env) args
    case execBuiltin op evalArgs of
        Right r -> Right r
        Left err -> execUserFunc ftable env op evalArgs err
evalAST _ _ (Call _ _) = Left "*** ERROR: Invalid function call"

lookupEnv :: Env -> String -> Maybe Ast
lookupEnv [] _ = Nothing
lookupEnv ((k,v):xs) key
    | k == key  = Just v
    | otherwise = lookupEnv xs key

evalASTEnv :: FuncTable -> Env -> Ast -> Either String Ast
evalASTEnv _ _ (AInteger n) = Right $ AInteger n
evalASTEnv _ _ (ABool b) = Right $ ABool b
evalASTEnv _ env (ASymbol s) = case lookupEnv env s of
    Just v -> Right v
    Nothing -> Left $ "*** ERROR: Undefined symbol: " ++ s
evalASTEnv ftable env (Define name body) =
    evalAST ftable env (Define name body)
evalASTEnv ftable env (DefineFun name params body) =
    evalAST ftable env (DefineFun name params body)
evalASTEnv ftable env (Call f args) = evalAST ftable env (Call f args)
