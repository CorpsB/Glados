{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Semantic Checker Engine
-}

module AST.Semantics.Check (checkAst, checkExpr, checkStmt) where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import AST.Ast (Ast(..))
import AST.Semantics.Type

-- | Checks if two types are semantically compatible.
-- Replaces the usage of (==) with explicit domain logic.
areTypesCompatible :: Type -> Type -> Bool
areTypesCompatible TyInt TyInt   = True
areTypesCompatible TyBool TyBool = True
areTypesCompatible TyVoid TyVoid = True
areTypesCompatible TyAuto TyAuto = True
areTypesCompatible (TyList a) (TyList b) = areTypesCompatible a b
areTypesCompatible (TyStruct a) (TyStruct b) = a == b
areTypesCompatible (TyFunc args1 ret1) (TyFunc args2 ret2) = 
    length args1 == length args2 &&
    and (zipWith areTypesCompatible args1 args2) &&
    areTypesCompatible ret1 ret2
areTypesCompatible _ _ = False

-- | Main entry point: validates a list of AST nodes.
checkAst :: [Ast] -> Either String ()
checkAst asts = case foldM checkStmt emptyEnv asts of
    Left err -> Left err
    Right _  -> Right ()

-- | Check Expression: Verifies an expression and returns its Semantic Type.
checkExpr :: CheckEnv -> Ast -> Either String Type
checkExpr _ (AInteger _) = Right TyInt
checkExpr _ (ABool _)    = Right TyBool
checkExpr _ AVoid        = Right TyVoid

checkExpr env (ASymbol name) = 
    case Map.lookup name (envVars env) of
        Just t  -> Right t
        Nothing -> Left $ "Undefined variable '" ++ T.unpack name ++ "'"

checkExpr env (AIf cond thenB elseB) = do
    tCond <- checkExpr env cond
    case tCond of
        TyBool -> do
            tThen <- checkExpr env thenB
            tElse <- checkExpr env elseB
            if areTypesCompatible tThen tElse
                then Right tThen
                else Left $ "Type mismatch in 'if' branches ("
                            ++ typeToString tThen ++ " vs " ++ typeToString tElse ++ ")"
        _ -> Left "Error: 'if' condition must be boolean"
checkExpr _ _ = Left "Error: Expression type not yet supported"

-- | Check Statement: Verifies a statement and returns the updated environment.checkStmt :: CheckEnv -> Ast -> Either String CheckEnv
checkStmt :: CheckEnv -> Ast -> Either String CheckEnv
checkStmt env (ASetVar name typeStr expr) = do
    let declaredType = parseType typeStr
    actualType <- checkExpr env expr
    
    -- Logic based purely on Pattern Matching
    -- 1. If 'auto', accept everything.
    -- 2. Otherwise, verify type compatibility.
    case declaredType of
        TyAuto -> do
            let newVars = Map.insert name actualType (envVars env)
            Right $ env { envVars = newVars }
        _ -> if areTypesCompatible declaredType actualType
             then do
                let newVars = Map.insert name declaredType (envVars env)
                Right $ env { envVars = newVars }
             else Left $ "Variable '" ++ T.unpack name ++ 
                         "' declared as " ++ typeToString declaredType ++ 
                         " but assigned " ++ typeToString actualType

checkStmt env _ = Right env
