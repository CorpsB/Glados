{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Z_old.Eval.AstSpec (spec) where

import Test.Hspec
import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (IntValue(..))
import Z_old.Src.Eval.Ast (evalAST, evalASTEnv)
import qualified Data.Text as DT
import Data.List (isInfixOf)

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Eval - AST Comprehensive Test Suite (100% Coverage)" $ do
    let ftSpy = [
                (DT.pack "getTrue", [], AInteger (I8 1)),
                (DT.pack "getHundred", [], AInteger (I8 100)),
                (DT.pack "callGetHundred", [], Call (ASymbol (DT.pack "getHundred")) []),
                (DT.pack "identity", [DT.pack "x"], ASymbol (DT.pack "x")),
                (DT.pack "getEnvVar", [], ASymbol (DT.pack "globalVar"))
                ]
    let envSpy = [
                (DT.pack "globalVar", AInteger (I16 999)),
                (DT.pack "x", AInteger (I8 10)),
                (DT.pack "false", ABool False)
                ]

    let envCaptured = [(DT.pack "captured", AInteger (I8 42))]
    let closureBody = Call (ASymbol (DT.pack "getHundred")) []
    let closureSpy = Closure [DT.pack "a"] closureBody envCaptured

    let ftMakeClosure = [
                        (DT.pack "getHundred", [], AInteger (I8 100)),
                        (DT.pack "getClosure", [], Lambda [] (Call (ASymbol (DT.pack "getHundred")) []))
                        ]

    describe "1. Atomic Values evaluation" $ do
        it "evalAST: Integers return themselves" $ do
            evalAST [] [] (AInteger (I8 123)) `shouldSatisfy` \case
                Right (AInteger (I8 123)) -> True; _ -> False
        it "evalAST: Booleans return themselves" $ do
            evalAST [] [] (ABool True) `shouldSatisfy` \case
                Right (ABool True) -> True; _ -> False
        it "evalAST: Symbols - Looked up successfully in Env" $ do
            evalAST [] [(DT.pack "key", AInteger (I8 5))] (ASymbol (DT.pack "key")) `shouldSatisfy` \case
                Right (AInteger (I8 5)) -> True; _ -> False
        it "evalAST: Symbols - Error if undefined" $ do
            evalAST [] [] (ASymbol (DT.pack "unknown")) `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Undefined symbol: unknown"
                _ -> False
        it "evalAST: AList returns itself" $ do
            evalAST [] [] (AList []) `shouldSatisfy` \case Right (AList []) -> True; _ -> False
        it "evalAST: AVoid returns itself" $ do
            evalAST [] [] AVoid `shouldSatisfy` \case Right AVoid -> True; _ -> False

    describe "1b. Atomic Values via evalASTEnv (Direct Coverage)" $ do
        it "evalASTEnv: Handles Integer directly" $ do
            evalASTEnv [] [] (AInteger (I8 1)) `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True; _ -> False
        it "evalASTEnv: Handles Boolean directly" $ do
            evalASTEnv [] [] (ABool False) `shouldSatisfy` \case
                Right (ABool False) -> True; _ -> False
        it "evalASTEnv: Handles Symbol lookup directly" $ do
            evalASTEnv [] [(DT.pack "a", AInteger (I8 1))] (ASymbol (DT.pack "a")) `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True; _ -> False
        it "evalASTEnv: Handles Symbol error directly (any Left)" $ do
            evalASTEnv [] [] (ASymbol (DT.pack "z")) `shouldSatisfy` \case
                Left _ -> True; _ -> False
        it "DefineFun: Returns structure as is (no evaluation)" $ do
            evalAST [] [] (DefineFun (DT.pack "f") [(DT.pack "args", DT.pack "Any")] (DT.pack "Void") (AInteger (I8 0))) `shouldSatisfy` \case
                Right (DefineFun name _ _ (AInteger (I8 0))) -> name == DT.pack "f"
                _ -> False

    describe "2. Definitions and Lambda Structures" $ do
        it "Define: Evaluates body and returns Define structure" $ do
            evalAST [] [] (Define (DT.pack "x") (DT.pack "int") (Call (ASymbol (DT.pack "+")) [AInteger (I8 1), AInteger (I8 1)])) `shouldSatisfy` \case
                Right (Define name _ (AInteger (I8 2))) -> name == DT.pack "x"
                _ -> False
        it "DefineFun: Returns structure as is (no evaluation)" $ do
            evalAST [] [] (DefineFun (DT.pack "f") [(DT.pack "args", DT.pack "Any")] (DT.pack "Void") (AInteger (I8 0))) `shouldSatisfy` \case
                Right (DefineFun name _ _ (AInteger (I8 0))) -> name == DT.pack "f"
                _ -> False
        it "Lambda: Evaluates to Closure capturing CURRENT environment (params/body/env checked)" $ do
            let localEnv = [(DT.pack "local", AInteger (I8 1))]
            evalAST [] localEnv (Lambda [DT.pack "x",DT.pack "y"] (ASymbol (DT.pack "local"))) `shouldSatisfy` \case
                Right (Closure params body captured) ->
                    params == [DT.pack "x",DT.pack "y"]
                    && case body of ASymbol s -> s == DT.pack "local"; _ -> False
                    && case captured of [(k, AInteger (I8 1))] -> k == DT.pack "local"; _ -> False
                _ -> False
        it "Closure: Evaluates to itself (p, b and e components)" $ do
            evalAST [] [] closureSpy `shouldSatisfy` \case
                Right (Closure params b e) ->
                    params == [DT.pack "a"]
                    && case b of Call (ASymbol s) [] -> s == DT.pack "getHundred"; _ -> False
                    && case e of [(k, AInteger (I8 42))] -> k == DT.pack "captured"; _ -> False
                _ -> False

        it "Define: Preserves typeVar" $ do
            evalAST [] [] (Define (p "x") (p "int") (AInteger (I8 1))) `shouldSatisfy` \case
                Right (Define _ t _) -> t == p "int"
                _ -> False
        
        it "DefineFun: Preserves ret type" $ do
            let args = [(p "a", p "int")]
            evalAST [] [] (DefineFun (p "f") args (p "void") (AInteger (I8 0))) `shouldSatisfy` \case
                Right (DefineFun _ _ r _) -> r == p "void"
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
            evalAST [] [] (Condition (ASymbol (DT.pack "undef")) (AInteger (I8 1)) (AInteger (I8 2))) `shouldSatisfy` \case
                Left _ -> True; _ -> False
        -- it "Fails if Condition is not a boolean/0/1 (Invalid Condition)" $ do
        --     evalAST [] [] (Condition (ASymbol "undef") (AInteger (I8 1)) (AInteger (I8 2))) `shouldSatisfy` \case
        --         Left err -> err == "*** ERROR: Invalid condition: ASymbol 'undef'"
        --         _ -> False

    describe "4. Calls - Named functions (Builtins, Closures, Globals)" $ do
        it "Calls Builtin: (+ 1 2) -> 3" $ do
            evalAST [] [] (Call (ASymbol (DT.pack "+")) [AInteger (I8 1), AInteger (I8 2)]) `shouldSatisfy` \case
                Right (AInteger (I8 3)) -> True; _ -> False
        it "Calls Builtin: Propagates error (div 1 0)" $ do
            evalAST [] [] (Call (ASymbol (DT.pack "div")) [AInteger (I8 1), AInteger (I8 0)]) `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: 'div' division by zero"
                _ -> False
        it "Calls Closure from Env: (myFunc)" $ do
            let envWithFunc = [(DT.pack "myFunc", closureSpy)]
            evalAST ftSpy envWithFunc (Call (ASymbol (DT.pack "myFunc")) [AInteger (I8 0)]) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Calls Closure from Env: Error on Arg mismatch" $ do
            let envWithFunc = [(DT.pack "myFunc", closureSpy)]
            evalAST ftSpy envWithFunc (Call (ASymbol (DT.pack "myFunc")) [AInteger (I8 1), AInteger (I8 2)]) `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Argument length mismatch in lambda call"
                _ -> False
        it "Calls Global Func: (getHundred)" $ do
            evalAST ftSpy [] (Call (ASymbol (DT.pack "getHundred")) []) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Calls Global Func: Error on Arg mismatch" $ do
            evalAST ftSpy [] (Call (ASymbol (DT.pack "getHundred")) [AInteger (I8 1)]) `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Argument length mismatch for function getHundred"
                _ -> False
        it "Calls Global Func: Recursion check (callGetHundred calls getHundred)" $ do
            evalAST ftSpy [] (Call (ASymbol (DT.pack "callGetHundred")) []) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Calls Global Func: Env Access (getEnvVar reads 'globalVar')" $ do
            evalAST ftSpy envSpy (Call (ASymbol (DT.pack "getEnvVar")) []) `shouldSatisfy` \case
                Right (AInteger (I16 999)) -> True; _ -> False
        it "Calls Unknown function" $ do
            evalAST [] [] (Call (ASymbol (DT.pack "whatIsThis")) []) `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Unknown func: whatIsThis"
                _ -> False

    describe "5. Calls - Expressions (Lambdas & Calculated functions)" $ do
        it "Calls Immediate Lambda: ((lambda (x) x) 5)" $ do
            let lambda = Lambda [DT.pack "x"] (ASymbol (DT.pack "x"))
            evalAST [] [] (Call lambda [AInteger (I8 5)]) `shouldSatisfy` \case
                Right (AInteger (I8 5)) -> True; _ -> False
        it "Calls Calculated Function: ((if #t (lambda () 1) ...) )" $ do
            let expr = Condition (ABool True) (Lambda [] (AInteger (I8 1))) (Lambda [] (AInteger (I8 2)))
            evalAST [] [] (Call expr []) `shouldSatisfy` \case
                Right (AInteger (I8 1)) -> True; _ -> False
        it "ExecExprCall: Verifies FT propagation in immediate lambda (ft used by evalAST ft env func)" $ do
            let nestedCall = Call (Call (ASymbol (DT.pack "getClosure")) []) [] 
            evalAST ftMakeClosure [] nestedCall `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True
                _ -> False
        it "ExecExprCall: Missing ft entry leads to unknown func error (ft arg effect)" $ do
            let nestedCall = Call (Call (ASymbol (DT.pack "getClosure")) []) []
            evalAST [] [] nestedCall `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Unknown func: getClosure"
                _ -> False
        it "ExecExprCall: Verifies FT propagation when immediate lambda uses FT internally" $ do
            let lambda = Lambda [] (Call (ASymbol (DT.pack "getHundred")) [])
            evalAST ftSpy [] (Call lambda []) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "ExecExprCall: Error if expression is not a function" $ do
            let badExpr = Call (ASymbol (DT.pack "+")) [AInteger (I8 1), AInteger (I8 2)]
            evalAST [] [] (Call badExpr [AInteger (I8 3)]) `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Attempt to call a non-function"
                _ -> False
        it "Calls: Error propagates from argument evaluation" $ do
            evalAST [] [] (Call (ASymbol (DT.pack "+")) [ASymbol (DT.pack "undef"), AInteger (I8 1)]) `shouldSatisfy` \case
                Left _ -> True; _ -> False

    describe "6. Deep Data Propagation (Recursion & State)" $ do
        it "Define propagates FT/Env to body evaluation (evalASTEnv -> evalAST uses ftable/env)" $ do
            evalAST ftSpy envSpy (Define (DT.pack "x") (DT.pack "Any") (Call (ASymbol (DT.pack "getEnvVar")) [])) `shouldSatisfy` \case
                Right (Define name _ (AInteger (I16 999))) -> name == DT.pack "x"
                _ -> False
        it "Condition propagates FT/Env to sub-expressions" $ do
            let c = Call (ASymbol (DT.pack "getTrue")) []
            let t = Call (ASymbol (DT.pack "getHundred")) []
            evalAST ftSpy [] (Condition c t t) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True
                Left err -> error $ "Propagation failed: " ++ DT.unpack err
                _ -> False
        it "Call propagates FT/Env to arguments via traverse" $ do
            evalAST ftSpy [] (Call (ASymbol (DT.pack "identity")) [Call (ASymbol (DT.pack "getHundred")) []]) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False

    describe "7. evalASTEnv Delegation Logic (explicit argument coverage)" $ do
        it "Delegates Define (evalASTEnv passes ftable/env to evalAST for body)" $ do
            evalASTEnv ftSpy envSpy (Define (DT.pack "x") (DT.pack "Any") (Call (ASymbol (DT.pack "getEnvVar")) [])) `shouldSatisfy` \case
                Right (Define name _ (AInteger (I16 999))) -> name == DT.pack "x"
                _ -> False
        it "Delegates DefineFun and preserves params & body arguments" $ do
            let params = [(DT.pack "a", DT.pack "Any"), (DT.pack "b", DT.pack "Any")]
            let body = Call (ASymbol (DT.pack "getHundred")) []
            evalASTEnv ftSpy envSpy (DefineFun (DT.pack "f") params (DT.pack "Void") body) `shouldSatisfy` \case
                Right (DefineFun name ps _ _) ->
                    name == DT.pack "f" && ps == params
                _ -> False
            evalASTEnv ftSpy envSpy body `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "Delegates Condition (evalASTEnv passes ftable/env to evalAST on condition and branches)" $ do
            let cond = Call (ASymbol (DT.pack "getTrue")) []
            let th   = Call (ASymbol (DT.pack "getHundred")) []
            let el   = Call (ASymbol (DT.pack "getHundred")) []
            evalASTEnv ftSpy [] (Condition cond th el) `shouldSatisfy` \case
                Right (AInteger (I8 100)) -> True; _ -> False
        it "evalASTEnv: returns Closure with exact params/body/env (params, body and env checked)" $ do
            let capturedEnv = [(DT.pack "free", AInteger (I8 7))]
            evalASTEnv [] capturedEnv (Lambda [DT.pack "u",DT.pack "v"] (ASymbol (DT.pack "free"))) `shouldSatisfy` \case
                Right (Closure ps b e) ->
                    ps == [DT.pack "u",DT.pack "v"]
                    && case b of ASymbol s -> s == DT.pack "free"; _ -> False
                    && case e of [(k, AInteger (I8 7))] -> k == DT.pack "free"; _ -> False
                _ -> False
        it "evalASTEnv: closure identity (p, b and e) for an existing Closure" $ do
            let cl = Closure [DT.pack "p"] (ASymbol (DT.pack "bodySym")) [(DT.pack "p", AInteger (I8 5))]
            evalASTEnv [] [] cl `shouldSatisfy` \case
                Right (Closure params b e) ->
                    params == [DT.pack "p"]
                    && case b of ASymbol s -> s == DT.pack "bodySym"; _ -> False
                    && case e of [(k, AInteger (I8 5))] -> k == DT.pack "p"; _ -> False
                _ -> False
    
    describe "Missing Coverage Targets" $ do

        it "evalAST: Import returns specific error" $ do
            evalAST [] [] (Import (p "lib")) `shouldSatisfy` \case
                Left err -> "Import' is not supported" `isInfixOf` DT.unpack err
                _ -> False

        it "evalASTEnv: Import returns specific error (via delegation)" $ do
            evalASTEnv [] [] (Import (p "lib")) `shouldSatisfy` \case
                Left err -> "Import' is not supported" `isInfixOf` DT.unpack err
                _ -> False

        it "lookupEnv: Finds variable deeper in env (recursion check)" $ do
            let deepEnv = [(p "a", AInteger (I8 1)), (p "b", AInteger (I8 2)), (p "target", AInteger (I8 99))]
            evalAST [] deepEnv (ASymbol (p "target")) `shouldSatisfy` \case
                Right (AInteger (I8 99)) -> True
                _ -> False
        
        it "evalASTEnv: AList returns itself" $ do
            evalASTEnv [] [] (AList []) `shouldSatisfy` \case Right (AList []) -> True; _ -> False
        it "evalASTEnv: AVoid returns itself" $ do
            evalASTEnv [] [] AVoid `shouldSatisfy` \case Right AVoid -> True; _ -> False
