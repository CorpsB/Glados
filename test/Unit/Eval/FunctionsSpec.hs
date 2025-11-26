{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- FunctionsSpec
-}

{-# LANGUAGE LambdaCase #-}

module Eval.FunctionsSpec (spec) where

import Test.Hspec
import Eval.Functions
import Ast (Ast(..), Env)

-- | Mock simple qui réussit toujours
mockEvalSuccess :: FuncTable -> Env -> Ast -> Either String Ast
mockEvalSuccess _ _ _ = Right (AInteger 1000)

-- | Mock simple qui échoue toujours
mockEvalFail :: FuncTable -> Env -> Ast -> Either String Ast
mockEvalFail _ _ _ = Left "Mock Error"

-- | SPY ULTIME : Force l'évaluation de TOUS les arguments (ft, env, body)
-- Résout les zones jaunes des lignes 33 et 41 en inspectant explicitement 'body'
spyForceAll :: FuncTable -> Env -> Ast -> Either String Ast
spyForceAll ft env body =
    -- 1. Force la lecture de la table (doit être non vide)
    if null ft then Left "Spy Error: Ftable is empty!"
    else
        -- 2. Force la lecture de l'environnement (va chercher au fond)
        case lookup "global" env of
            Nothing -> Left "Spy Error: 'global' not found in env"
            Just _ ->
                -- 3. Force l'évaluation du body (c'est ce qui manquait !)
                case body of
                     AInteger _ -> Right (AInteger 1000)
                     _ -> Right (AInteger 0) -- Juste pour dire qu'on l'a lu

spec :: Spec
spec = describe "Functions Management" $ do
    let emptyTable = []
    let funcName = "testFunc"
    let params = ["a"]
    let body = AInteger 1
    
    -- Table pré-remplie pour tester l'ajout par dessus (Line 20)
    let populatedTable = [(funcName, params, body)]
    
    -- Table multiple pour tester la récursion (Line 26) et le spy
    -- f2 a un body AInteger 20, qui sera vérifié par le spy
    let multiTable = [("f1", ["x"], AInteger 10), ("f2", ["y"], AInteger 20)]
    
    let args = [AInteger 2]
    -- Variable "global" placée à la fin pour forcer le parcours de la liste env
    let globalEnv = [("global", AInteger 99)] 

    describe "registerFunction" $ do
        -- COUVRE : Line 20 (body, ftable)
        -- On utilise populatedTable (non vide) et on inspecte explicitement le contenu du résultat
        it "registers a new function on top of existing ones (forcing body and ftable evaluation)" $ do
            registerFunction populatedTable "newFunc" ["b"] (AInteger 999) `shouldSatisfy` \case
                Right res ->
                    let (n, p, b) = head res -- On récupère la nouvelle fonction
                        rest = tail res      -- On récupère l'ancienne table (ftable)
                    in
                    n == "newFunc" &&
                    p == ["b"] &&
                    -- Force l'évaluation du body (résout le jaune sur 'body')
                    (case b of AInteger 999 -> True; _ -> False) &&
                    -- Force l'évaluation de ftable (résout le jaune sur 'ftable')
                    length rest == 1
                _ -> False

        it "fails if function already exists" $ do
            registerFunction populatedTable funcName params body `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Function already exists: " ++ funcName
                _ -> False

    describe "getFunction" $ do
        -- COUVRE : Line 26 (recursion)
        it "finds a function deeper in the list" $ do
            callFunction mockEvalSuccess multiTable [] "f2" [AInteger 0] `shouldSatisfy` \case
                Right _ -> True
                _ -> False

    describe "callFunction & execFunc" $ do
        it "fails for unknown function" $ do
             callFunction mockEvalSuccess emptyTable [] "unknown" [] `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Unknown function: unknown"
                 _ -> False

        it "fails on argument mismatch" $ do
             callFunction mockEvalSuccess populatedTable [] funcName [] `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Argument length mismatch for function " ++ funcName
                 _ -> False

        -- COUVRE : Lignes 33 et 41 (body, ft, env)
        -- Le spyForceAll va lire le body (AInteger 20 de f2), la table et l'env.
        it "passes evaluated ftable, env AND body to evalFn" $ do
             callFunction spyForceAll multiTable globalEnv "f2" [AInteger 0] `shouldSatisfy` \case
                 Right (AInteger 1000) -> True
                 Left err -> error err 
                 _ -> False

        it "propagates evaluation failure" $ do
             callFunction mockEvalFail populatedTable [] funcName args `shouldSatisfy` \case
                 Left err -> err == "*** ERROR: Function evaluation failed: Mock Error"
                 _ -> False
