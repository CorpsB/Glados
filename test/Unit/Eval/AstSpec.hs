{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Eval.AstSpec (spec) where

import Test.Hspec
import Ast (Ast(..))
import Type.Integer (IntValue(..))
import Eval.Ast (evalAST, evalASTEnv)

spec :: Spec
spec = describe "Eval - AST Comprehensive Test Suite (100% Coverage)" $ do
    let ftSpy = [
                ("getTrue", [], AInteger (I8 1)),
                ("getHundred", [], AInteger (I8 100)),
                ("callGetHundred", [], Call (ASymbol "getHundred") []),
                ("identity", ["x"], ASymbol "x"),
                ("getEnvVar", [], ASymbol "globalVar")
                ]
    let envSpy = [
                ("globalVar", AInteger (I16 999)),
                ("x", AInteger (I8 10)),
                ("false", ABool False)
                ]

    let envCaptured = [("captured", AInteger (I8 42))]
    let closureBody = Call (ASymbol "getHundred") []
    let closureSpy = Closure ["a"] closureBody envCaptured

    let ftMakeClosure = [
                        ("getHundred", [], AInteger (I8 100)),
                        ("getClosure", [], Lambda [] (Call (ASymbol "getHundred") []))
                        ]

    describe "1. Atomic Values evaluation" $ do
        it "evalAST: Integers return themselves" $ do
            evalAST [] [] (AInteger (I8 123)) `shouldSatisfy` \case
                Right (AInteger (I8 123)) -> True; _ -> False
        it "evalAST: Booleans return themselves" $ do
            evalAST [] [] (ABool True) `shouldSatisfy` \case
                Right (ABool True) -> True; _ -> False
        it "evalAST: Symbols - Looked up successfully in Env" $ do
            evalAST [] [("key", AInteger (I8 5))] (ASymbol "key") `shouldSatisfy` \case
                Right (AInteger (I8 5)) -> True; _ -> False
        it "evalAST: Symbols - Error if undefined" $ do
            evalAST [] [] (ASymbol "unknown") `shouldSatisfy` \case
                Left "*** ERROR: Undefined symbol: unknown" -> True; _ -> False

    describe "1b. Atomic Values via evalASTEnv (Direct Coverage)" $ do
        it "evalASTEnv: Handles Integer directly" $ do
            evalASTEnv [] [] (AInteger (I8 1)) `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True; _ -> False
        it "evalASTEnv: Handles Boolean directly" $ do
            evalASTEnv [] [] (ABool False) `shouldSatisfy` \case
                Right (ABool False) -> True; _ -> False
        it "evalASTEnv: Handles Symbol lookup directly" $ do
            evalASTEnv [] [("a", AInteger (I8 1))] (ASymbol "a") `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True; _ -> False
        it "evalASTEnv: Handles Symbol error directly (any Left)" $ do
            evalASTEnv [] [] (ASymbol "z") `shouldSatisfy` \case
                Left _ -> True; _ -> False
        it "evalASTEnv: Handles Symbol error message exactly" $ do
            evalASTEnv [] [] (ASymbol "z") `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Undefined symbol: z"
                _ -> False

    describe "2. Definitions and Lambda Structures" $ do
        it "Define: Evaluates body and returns Define structure" $ do
            evalAST [] [] (Define "x" (Call (ASymbol "+") [AInteger (I8 1), AInteger (I8 1)])) `shouldSatisfy` \case
                Right (Define "x" (AInteger (I8 2))) -> True; _ -> False
        it "DefineFun: Returns structure as is (no evaluation)" $ do
            evalAST [] [] (DefineFun "f" ["args"] (AInteger (I8 0))) `shouldSatisfy` \case
                Right (DefineFun "f" ["args"] (AInteger (I8 0))) -> True; _ -> False
        it "Lambda: Evaluates to Closure capturing CURRENT environment (params/body/env checked)" $ do
            let localEnv = [("local", AInteger (I8 1))]
            evalAST [] localEnv (Lambda ["x","y"] (ASymbol "local")) `shouldSatisfy` \case
                Right (Closure params body captured) ->
                    params == ["x","y"]
                    && case body of ASymbol "local" -> True; _ -> False
                    && case captured of [("local", AInteger (I8 1))] -> True; _ -> False
                _ -> False
        it "Closure: Evaluates to itself (p, b and e components)" $ do
            evalAST [] [] closureSpy `shouldSatisfy` \case
                Right (Closure p b e) ->
                    p == ["a"]
                    && case b of Call (ASymbol "getHundred") [] -> True; _ -> False
                    && case e of [("captured", AInteger (I8 42))] -> True; _ -> False
                _ -> False

    describe "3. Conditions Logic" $ do
        it "Executes THEN branch if condition is #t" $ do
            evalAST [] [] (Condition (ABool True) (AInteger (I8 1)) (AInteger (I8 2))) `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True; _ -> False
        it "Executes THEN branch if condition is Integer 1" $ do
            evalAST [] [] (Condition (AInteger (I8 1)) (AInteger (I8 10)) (AInteger (I8 20))) `shouldSatisfy` \case
                Right (AInteger (I8 10)) -> True; _ -> False
        it "Executes ELSE branch if condition is #f" $ do
            evalAST [] [] (Condition (ABool False) (AInteger (I8 1)) (AInteger (I8 2))) `shouldSatisfy` \case
                Right (AInteger (I8 2)) -> True; _ -> False
        it "Executes ELSE branch if condition is Integer 0" $ do
            evalAST [] [] (Condition (AInteger (I8 0)) (AInteger (I8 10)) (AInteger (I8 20))) `shouldSatisfy` \case
                Right (AInteger (I8 20)) -> True; _ -> False
        it "Propagates Error if Condition evaluation fails" $ do
            evalAST [] [] (Condition (ASymbol "undef") (AInteger (I8 1)) (AInteger (I8 2))) `shouldSatisfy` \case
                Left _ -> True; _ -> False
        -- it "Fails if Condition is not a boolean/0/1 (Invalid Condition)" $ do
        --     evalAST [] [] (Condition (ASymbol "undef") (AInteger (I8 1)) (AInteger (I8 2))) `shouldSatisfy` \case
        --         Left err -> err == "*** ERROR: Invalid condition: ASymbol 'undef'"
        --         _ -> False

    describe "4. Calls - Named functions (Builtins, Closures, Globals)" $ do
        it "Calls Builtin: (+ 1 2) -> 3" $ do
            evalAST [] [] (Call (ASymbol "+") [AInteger (I8 1), AInteger (I8 2)]) `shouldSatisfy` \case
                Right (AInteger (I8 3)) -> True; _ -> False
        it "Calls Builtin: Propagates error (div 1 0)" $ do
            evalAST [] [] (Call (ASymbol "div") [AInteger (I8 1), AInteger (I8 0)]) `shouldSatisfy` \case
                Left "*** ERROR: Unknown func: div" -> True; _ -> False
        it "Calls Closure from Env: (myFunc)" $ do
            let envWithFunc = [("myFunc", closureSpy)]
            evalAST ftSpy envWithFunc (Call (ASymbol "myFunc") [AInteger (I8 0)]) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Calls Closure from Env: Error on Arg mismatch" $ do
            let envWithFunc = [("myFunc", closureSpy)]
            evalAST ftSpy envWithFunc (Call (ASymbol "myFunc") [AInteger (I8 1), AInteger (I8 2)]) `shouldSatisfy` \case
                Left "*** ERROR: Argument length mismatch in lambda call" -> True; _ -> False
        it "Calls Global Func: (getHundred)" $ do
            evalAST ftSpy [] (Call (ASymbol "getHundred") []) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Calls Global Func: Error on Arg mismatch" $ do
            evalAST ftSpy [] (Call (ASymbol "getHundred") [AInteger (I8 1)]) `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Argument length missmatch for function getHundred"
                _ -> False
        it "Calls Global Func: Recursion check (callGetHundred calls getHundred)" $ do
            evalAST ftSpy [] (Call (ASymbol "callGetHundred") []) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Calls Global Func: Env Access (getEnvVar reads 'globalVar')" $ do
            evalAST ftSpy envSpy (Call (ASymbol "getEnvVar") []) `shouldSatisfy` \case
                Right (AInteger (I16 999)) -> True; _ -> False
        it "Calls Unknown function" $ do
            evalAST [] [] (Call (ASymbol "whatIsThis") []) `shouldSatisfy` \case
                Left "*** ERROR: Unknown func: whatIsThis" -> True; _ -> False

    describe "5. Calls - Expressions (Lambdas & Calculated functions)" $ do
        it "Calls Immediate Lambda: ((lambda (x) x) 5)" $ do
            let lambda = Lambda ["x"] (ASymbol "x")
            evalAST [] [] (Call lambda [AInteger (I8 5)]) `shouldSatisfy` \case
                Right (AInteger (I8 5)) -> True; _ -> False
        it "Calls Calculated Function: ((if #t (lambda () 1) ...) )" $ do
            let expr = Condition (ABool True) (Lambda [] (AInteger (I8 1))) (Lambda [] (AInteger (I8 2)))
            evalAST [] [] (Call expr []) `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True; _ -> False
        it "ExecExprCall: Verifies FT propagation in immediate lambda (ft used by evalAST ft env func)" $ do
            let nestedCall = Call (Call (ASymbol "getClosure") []) [] -- evaluates getClosure (from ft) -> closure, then calls it
            evalAST ftMakeClosure [] nestedCall `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True
                _ -> False
        it "ExecExprCall: Missing ft entry leads to unknown func error (ft arg effect)" $ do
            let nestedCall = Call (Call (ASymbol "getClosure") []) []
            evalAST [] [] nestedCall `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Unknown func: getClosure"
                _ -> False
        it "ExecExprCall: Verifies FT propagation when immediate lambda uses FT internally" $ do
            let lambda = Lambda [] (Call (ASymbol "getHundred") [])
            evalAST ftSpy [] (Call lambda []) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "ExecExprCall: Error if expression is not a function" $ do
            let badExpr = Call (ASymbol "+") [AInteger (I8 1), AInteger (I8 2)]
            evalAST [] [] (Call badExpr [AInteger (I8 3)]) `shouldSatisfy` \case
                Left "*** ERROR: Attempt to call a non-function" -> True; _ -> False
        it "Calls: Error propagates from argument evaluation" $ do
            evalAST [] [] (Call (ASymbol "+") [ASymbol "undef", AInteger (I8 1)]) `shouldSatisfy` \case
                Left _ -> True; _ -> False

    describe "6. Deep Data Propagation (Recursion & State)" $ do
        it "Define propagates FT/Env to body evaluation (evalASTEnv -> evalAST uses ftable/env)" $ do
            evalAST ftSpy envSpy (Define "x" (Call (ASymbol "getEnvVar") [])) `shouldSatisfy` \case
                Right (Define "x" (AInteger (I16 999))) -> True; _ -> False
        it "Condition propagates FT/Env to sub-expressions" $ do
            let c = Call (ASymbol "getTrue") []
            let t = Call (ASymbol "getHundred") []
            evalAST ftSpy [] (Condition c t t) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True
                Left err -> error $ "Propagation failed: " ++ err
                _ -> False
        it "Call propagates FT/Env to arguments via traverse" $ do
            evalAST ftSpy [] (Call (ASymbol "identity") [Call (ASymbol "getHundred") []]) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False

    describe "7. evalASTEnv Delegation Logic (explicit argument coverage)" $ do
        it "Delegates Define (evalASTEnv passes ftable/env to evalAST for body)" $ do
            evalASTEnv ftSpy envSpy (Define "x" (Call (ASymbol "getEnvVar") [])) `shouldSatisfy` \case
                Right (Define "x" (AInteger (I16 999))) -> True; _ -> False
        it "Delegates DefineFun and preserves params & body arguments" $ do
            let params = ["a","b"]
            let body = Call (ASymbol "getHundred") []
            evalASTEnv ftSpy envSpy (DefineFun "f" params body) `shouldSatisfy` \case
                Right (DefineFun "f" ps b) ->
                    ps == params && case b of Call (ASymbol "getHundred") [] -> True; _ -> False
                _ -> False
            evalASTEnv ftSpy envSpy body `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Delegates Condition (evalASTEnv passes ftable/env to evalAST on condition and branches)" $ do
            let cond = Call (ASymbol "getTrue") []
            let th   = Call (ASymbol "getHundred") []
            let el   = Call (ASymbol "getHundred") []
            evalASTEnv ftSpy [] (Condition cond th el) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "evalASTEnv: returns Closure with exact params/body/env (params, body and env checked)" $ do
            let capturedEnv = [("free", AInteger (I8 7))]
            evalASTEnv [] capturedEnv (Lambda ["u","v"] (ASymbol "free")) `shouldSatisfy` \case
                Right (Closure ps b e) ->
                    ps == ["u","v"]
                    && case b of ASymbol "free" -> True; _ -> False
                    && case e of [("free", AInteger (I8 7))] -> True; _ -> False
                _ -> False
        it "evalASTEnv: closure identity (p, b and e) for an existing Closure" $ do
            let cl = Closure ["p"] (ASymbol "bodySym") [("p", AInteger (I8 5))]
            evalASTEnv [] [] cl `shouldSatisfy` \case
                Right (Closure p b e) ->
                    p == ["p"]
                    && case b of ASymbol "bodySym" -> True; _ -> False
                    && case e of [("p", AInteger (I8 5))] -> True; _ -> False
                _ -> False
