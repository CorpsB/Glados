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

-- | Helper to insert a variable into the environment
insertVar :: CheckEnv -> T.Text -> Type -> Either String CheckEnv
insertVar env name t =
    let newVars = Map.insert name t (envVars env)
    in Right $ env { envVars = newVars }

-- | Check Expression: Verifies an expression and returns its Semantic Type.
checkExpr :: CheckEnv -> Ast -> Either String Type
checkExpr _ (AInteger _) = Right TyInt
checkExpr _ (ABool _)    = Right TyBool
checkExpr _ AVoid        = Right TyVoid
checkExpr env (ASymbol name) = checkSymbol env name
checkExpr env (AIf c t e)    = checkIf env c t e
checkExpr _ _ = Left "Error: Expression type not yet supported"

-- | Symbol lookup
checkSymbol :: CheckEnv -> T.Text -> Either String Type
checkSymbol env name = case Map.lookup name (envVars env) of
    Just t  -> Right t
    Nothing -> Left $ "Undefined variable '" ++ T.unpack name ++ "'"

-- | IF logic
checkIf :: CheckEnv -> Ast -> Ast -> Ast -> Either String Type
checkIf env cond thenB elseB = do
    tCond <- checkExpr env cond
    case tCond of
        TyBool -> checkBranches env thenB elseB
        _ -> Left "Error: 'if' condition must be boolean"

-- | Branches comparison
checkBranches :: CheckEnv -> Ast -> Ast -> Either String Type
checkBranches env thenB elseB = do
    tThen <- checkExpr env thenB
    tElse <- checkExpr env elseB
    if areTypesCompatible tThen tElse
        then Right tThen
        else Left $ "Type mismatch in 'if' branches (" ++
                    typeToString tThen ++ " vs " ++
                    typeToString tElse ++ ")"

-- | Check Statement: Verifies a statement and returns the updated environment.
checkStmt :: CheckEnv -> Ast -> Either String CheckEnv
checkStmt env (ASetVar name typeStr expr) = checkSetVar env name typeStr expr
checkStmt env _ = Right env

-- | Variable assignment
checkSetVar :: CheckEnv -> T.Text -> T.Text -> Ast -> Either String CheckEnv
checkSetVar env name typeStr expr = do
    let declaredType = parseType typeStr
    actualType <- checkExpr env expr
    applyAssignment env name declaredType actualType

-- | Applies the assignment logic based on types
applyAssignment :: CheckEnv -> T.Text -> Type -> Type -> Either String CheckEnv
applyAssignment env name TyAuto actual = insertVar env name actual
applyAssignment env name declared actual
    | areTypesCompatible declared actual = insertVar env name declared
    | otherwise = Left $ "Variable '" ++ T.unpack name ++ 
                         "' declared as " ++ typeToString declared ++ 
                         " but assigned " ++ typeToString actual
