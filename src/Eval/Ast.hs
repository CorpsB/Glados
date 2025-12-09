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
import qualified Data.Text as DT

applyClosure :: FuncTable -> Env -> [DT.Text] -> Ast -> [Ast] -> Either DT.Text Ast
applyClosure ftable cEnv params body args
    | sameLength params args =
        evalASTEnv ftable (zip params args ++ cEnv) body
    | otherwise = Left $ DT.pack "*** ERROR: Argument length mismatch in lambda call"

execUserFunc :: FuncTable -> Env -> DT.Text -> [Ast] -> DT.Text -> Either DT.Text Ast
execUserFunc ft env op args err = case getFunction ft op of
    Just (p, b) | sameLength p args ->
        evalASTEnv ft (zip p args ++ env) b
    Just _ -> Left $ DT.pack "*** ERROR: Argument length mismatch for function " <> op
    Nothing -> Left err

execNamedCall :: FuncTable -> Env -> DT.Text -> [Ast] -> Either DT.Text Ast
execNamedCall ft env op args = case execBuiltin op args of
    Right r -> Right r
    Left _ -> case lookupEnv env op of
        Just (Closure p b cEnv) -> applyClosure ft cEnv p b args
        _ -> execUserFunc ft env op args (DT.pack "*** ERROR: Unknown func: " <> op)

execExprCall :: FuncTable -> Env -> Ast -> [Ast] -> Either DT.Text Ast
execExprCall ft env func args = evalAST ft env func >>= \res ->
    case res of
        Closure p b cEnv -> applyClosure ft cEnv p b args
        _ -> Left $ DT.pack "*** ERROR: Attempt to call a non-function"

evalAST :: FuncTable -> Env -> Ast -> Either DT.Text Ast
evalAST _ _ (AInteger n) = Right $ AInteger n
evalAST _ _ (ABool b) = Right $ ABool b
evalAST _ _ (AList l) = Right $ AList l
evalAST _ _ AVoid = Right AVoid
evalAST _ env (ASymbol s) = case lookupEnv env s of
    Just v -> Right v
    Nothing -> Left $ DT.pack "*** ERROR: Undefined symbol: " <> s
evalAST _ env (Lambda params body) = Right $ Closure params body env
evalAST _ _ (Closure p b e) = Right $ Closure p b e
evalAST ftable env (Define name body) = do
    b2 <- evalAST ftable env body
    Right $ Define name b2
evalAST _ _ (DefineFun name params body) = Right $ DefineFun name params body
evalAST ftable env (Condition cond th el) = do
    c <- evalAST ftable env cond
    chosen <- execCondition c th el
    evalAST ftable env chosen
evalAST ftable env (Call func args) = do
    evalArgs <- traverse (evalASTEnv ftable env) args
    case func of
        ASymbol op -> execNamedCall ftable env op evalArgs
        _ -> execExprCall ftable env func evalArgs
evalAST _ _ (Import _) = Left $ DT.pack "*** ERROR: 'Import' is not supported in interpreter mode"

lookupEnv :: Env -> DT.Text -> Maybe Ast
lookupEnv [] _ = Nothing
lookupEnv ((k,v):xs) key
    | k == key  = Just v
    | otherwise = lookupEnv xs key

evalASTEnv :: FuncTable -> Env -> Ast -> Either DT.Text Ast
evalASTEnv = evalAST

