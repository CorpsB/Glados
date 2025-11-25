{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Ast
-}

module Eval.Ast (evalAST, evalASTEnv) where

import Ast (Ast(..), Env)
import Eval.Builtins (execBuiltin)
import Eval.Conditions (execCondition)
import Eval.Functions (getFunction, FuncTable)
import Utils.List (sameLength)

applyClosure :: FuncTable -> Env -> [String] -> Ast -> [Ast] -> Either String Ast
applyClosure ftable cEnv params body args
    | sameLength params args =
        evalASTEnv ftable (zip params args ++ cEnv) body
    | otherwise = Left "*** ERROR: Argument length mismatch in lambda call"

execUserFunc :: FuncTable -> Env -> String -> [Ast] -> String -> Either String Ast
execUserFunc ft env op args err = case getFunction ft op of
    Just (p, b) | sameLength p args ->
        evalASTEnv ft (zip p args ++ env) b
    Just _ -> Left $ "*** ERROR: Argument length " ++
        "missmatch for function " ++ op
    Nothing -> Left err

execNamedCall :: FuncTable -> Env -> String -> [Ast] -> Either String Ast
execNamedCall ft env op args = case execBuiltin op args of
    Right r -> Right r
    Left _ -> case lookupEnv env op of
        Just (Closure p b cEnv) -> applyClosure ft cEnv p b args
        _ -> execUserFunc ft env op args ("*** ERROR: Unknown func: " ++ op)

execExprCall :: FuncTable -> Env -> Ast -> [Ast] -> Either String Ast
execExprCall ft env func args = evalAST ft env func >>= \res ->
    case res of
        Closure p b cEnv -> applyClosure ft cEnv p b args
        _ -> Left "*** ERROR: Attempt to call a non-function"

evalAST :: FuncTable -> Env -> Ast -> Either String Ast
evalAST _ _ (AInteger n) = Right $ AInteger n
evalAST _ _ (ABool b) = Right $ ABool b
evalAST _ env (ASymbol s) = case lookupEnv env s of
    Just v -> Right v
    Nothing -> Left $ "*** ERROR: Undefined symbol: " ++ s
evalAST _ env (Lambda params body) = Right $ Closure params body env
evalAST _ _ (Closure p b e) = Right $ Closure p b e
evalAST ftable env (Define name body) = do
    b2 <- evalAST ftable env body
    Right $ Define name b2
evalAST _ _ (DefineFun name params body) = Right $ DefineFun name params body
evalAST ftable env (Condition cond th el) = do
    c <- evalAST ftable env cond
    t <- evalAST ftable env th
    e <- evalAST ftable env el
    execCondition (Just c) (Just t) (Just e) >>= Right
evalAST ftable env (Call func args) = do
    evalArgs <- traverse (evalASTEnv ftable env) args
    case func of
        ASymbol op -> execNamedCall ftable env op evalArgs
        _ -> execExprCall ftable env func evalArgs

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
evalASTEnv _ env (Lambda params body) = Right $ Closure params body env
evalASTEnv _ _ (Closure p b e) = Right $ Closure p b e
evalASTEnv ftable env (Define name body) =
    evalAST ftable env (Define name body)
evalASTEnv ftable env (DefineFun name params body) =
    evalAST ftable env (DefineFun name params body)
evalASTEnv ftable env (Call f args) = evalAST ftable env (Call f args)
evalASTEnv ftable env (Condition c t e) = evalAST ftable env (Condition c t e)
