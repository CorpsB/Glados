{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- RunSpec
-}

{-# LANGUAGE LambdaCase #-}

module Eval.RunSpec (spec) where

import Test.Hspec
import Data.List (find, isInfixOf)
import Data.Maybe (isJust)

import Eval.Run (processSExpr, astFromSexpr, processDefine, processCallOrEval)
import Eval.Functions (FuncTable)
import Lisp (SExpr(..))
import Ast (Ast(..))
import Type.Integer (IntValue(..))

spec :: Spec
spec = describe "Eval.Run - Comprehensive Test Suite (target: 100% coverage)" $ do
    describe "Public API: processSExpr basic behaviors (kept & extended)" $ do
        it "processSExpr: evaluates a raw integer SExpr -> returns that integer" $ do
            processSExpr [] [] (SInteger 5) `shouldSatisfy` \case
                Right (ft, env, Just (AInteger (I8 5))) -> null ft && null env
                _ -> False
        it "processSExpr: symbol undefined -> Left error" $ do
            processSExpr [] [] (SSymbol "undefSym") `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Undefined symbol: undefSym"
                _ -> False

    describe "astFromSexpr (success and error message contains show sexpr)" $ do
        it "astFromSexpr: List [SSymbol \"foo\", SInteger 7] converts to Call AST (Right)" $ do
            let sexpr = List [SSymbol "foo", SInteger 7]
            astFromSexpr sexpr `shouldSatisfy` \case
                Right (Call (ASymbol "foo") [AInteger (I8 7)]) -> True
                _ -> False
        it "astFromSexpr: malformed lambda sexpr returns Left and error message contains shown sexpr" $ do
            let malformed = List [List [], SInteger 7]
            astFromSexpr malformed `shouldSatisfy` \case
                Left err ->
                    "Syntax error: could not convert SExpr to AST:" `isInfixOf` err
                    && "List []" `isInfixOf` err
                _ -> False

    describe "processDefine (non-closure branch) - env propagation & ftable preserved" $ do
        it "processDefine: define simple integer places (name, AInteger) at head of returned env and preserves rest of env" $ do
            let initialEnv = [("pre", AInteger (I8 99))]
            let ast = Define "x" (AInteger (I8 10))
            processDefine ([] :: FuncTable) initialEnv ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    null ft' &&
                    case env' of
                        (("x", AInteger (I8 10)) : rest) ->
                            case rest of
                                [("pre", AInteger (I8 99))] -> True
                                _ -> False
                        _ -> False
                _ -> False
        it "processDefine: uses provided FuncTable during body evaluation (ft argument to evalAST) - observable via body call" $ do
            let ftWithGetTen = [("getTen", [], AInteger (I8 10))]
            let body = Call (ASymbol "getTen") []
            let ast = Define "res" body
            processDefine ftWithGetTen [] ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    isJust (find (\(n,_,_) -> n == "getTen") ft')
                    && isJust (find (\(k,v) -> k == "res" && case v of AInteger (I8 10) -> True; _ -> False) env')
                _ -> False

    describe "processDefine (closure branch) - recursive closure formation & env/ftable retention" $ do
        it "processDefine: when body is Lambda, create recursive closure containing self in its cenv" $ do
            let body = Lambda ["a"] (ASymbol "a")
            let ast = Define "f" body
            processDefine ([] :: FuncTable) [] ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    null ft' &&
                    case find (\(k,_) -> k == "f") env' of
                        Just (_, Closure params b cenv) ->
                            params == ["a"]
                            && case b of ASymbol "a" -> True; _ -> False
                            && isJust (find (\(k', _) -> k' == "f") cenv)
                        _ -> False
                _ -> False
        it "processDefine: closure stored at head of env and ftable from input preserved" $ do
            let body = Lambda [] (AInteger (I8 1))
            let ast = Define "g" body
            let initialFT = [("some", [], AInteger (I8 0))]
            processDefine initialFT [] ast `shouldSatisfy` \case
                Right (ft', envReturned, Nothing) ->
                    isJust (find (\(n,_,_) -> n == "some") ft')
                    &&
                    case envReturned of
                        (("g", Closure _ b cenv) : rest) ->
                            case b of
                                AInteger (I8 1) ->
                                    not (null cenv)
                                    &&
                                    case rest of
                                        []      -> True
                                        (_:_)   -> True
                                _ -> False
                        _ -> False
                _ -> False
        it "processDefine: the recursive closure's cenv contains a closure which itself contains 'g' in its cenv (deep recursive witness)" $ do
            let ast = Define "g" (Lambda [] (AInteger (I8 1)))
            processDefine ([] :: FuncTable) [] ast `shouldSatisfy` \case
                Right (_ft, envOut, Nothing) ->
                    case find (\(k,_) -> k == "g") envOut of
                        Just (_, Closure _ _ cenv) ->
                            case find (\(k, _) -> k == "g") cenv of
                                Just (_, Closure _ _ innerCenv) ->
                                    isJust (find (\(k',_) -> k' == "g") innerCenv)
                                _ -> False
                        _ -> False
                _ -> False

    describe "processDefine: DefineFun (registerFunction) success & failure" $ do
        it "DefineFun success: returns updated FuncTable with the new function entry and preserves env argument" $ do
            let envBefore = [("x", AInteger (I8 11))]
            let body = Call (ASymbol "+") [ASymbol "a", ASymbol "b"]
            let ast = DefineFun "add" ["a", "b"] body
            processDefine ([] :: FuncTable) envBefore ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    case env' of
                        [("x", AInteger (I8 11))] -> case ft' of
                            (n, ps, bdy) : _ -> n == "add" && ps == ["a","b"] && case bdy of Call (ASymbol "+") [ASymbol "a", ASymbol "b"] -> True; _ -> False
                            _ -> False
                        _ -> False
                _ -> False
        it "DefineFun failure if function already exists -> Left error with exact message" $ do
            let existingFT = [("dup", ["x"], AInteger (I8 0))]
            let ast = DefineFun "dup" ["x"] (AInteger (I8 1))
            processDefine existingFT [] ast `shouldSatisfy` \case
                Left err -> err == "*** ERROR: Function already exists: dup"
                _ -> False

    describe "processDefine: error on non-Define AST" $ do
        it "processDefine called with AInteger should return the specific Left error" $ do
            processDefine ([] :: FuncTable) [] (AInteger (I8 5)) `shouldSatisfy` \case
                Left err -> err == "processDefine called with non-define AST"
                _ -> False
        it "processDefine called with Call should return the specific Left error" $ do
            processDefine ([] :: FuncTable) [] (Call (ASymbol "+") []) `shouldSatisfy` \case
                Left err -> err == "processDefine called with non-define AST"
                _ -> False

    describe "processCallOrEval: evalAST receives the ftable/env arguments (via observable results)" $ do
        it "processCallOrEval: calling a function present in ftable yields result showing ftable was used and env preserved" $ do
            let ft = [("getSeven", [], AInteger (I8 7))]
            let env = [("x", AInteger (I8 1))]
            let ast = Call (ASymbol "getSeven") []
            processCallOrEval ft env ast `shouldSatisfy` \case
                Right (ft', env', Just (AInteger (I8 7))) ->
                    isJust (find (\(n,_,_) -> n == "getSeven") ft')
                    && isJust (find (\(k, v) -> k == "x" && case v of AInteger (I8 1) -> True; _ -> False) env')
                _ -> False
        it "processCallOrEval: when ftable lacks the func, evalAST returns Left and processCallOrEval propagates it" $ do
            let ftEmpty = [] :: FuncTable
            let env = []
            let ast = Call (ASymbol "noFn") []
            processCallOrEval ftEmpty env ast `shouldSatisfy` \case
                Left err -> "*** ERROR: Unknown func: noFn" `isInfixOf` err
                _ -> False
        it "processCallOrEval: argument error in evalAST is propagated (e.g., undefined symbol used inside AST)" $ do
            let ft = [] :: FuncTable
            let ast = Call (ASymbol "+") [ASymbol "someUndefined"]
            processCallOrEval ft [] ast `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "processSExpr: verifies Define{} and DefineFun{} branches use provided ftable and env" $ do
        it "processSExpr passes provided ftable and env to processDefine for Define{} branch (ftable preserved & env extended)" $ do
            let ftIn = [("preFn", [], AInteger (I8 1))]
            let envIn = [("preVar", AInteger (I8 2))]
            let def = List [SSymbol "define", SSymbol "z", SInteger 13]
            processSExpr ftIn envIn def `shouldSatisfy` \case
                Right (ftOut, envOut, Nothing) ->
                    isJust (find (\(n,_,_) -> n == "preFn") ftOut)
                    &&
                    case envOut of
                        (("z", AInteger (I8 13)) : rest) -> case rest of
                            [("preVar", AInteger (I8 2))] -> True
                            _ -> False
                        _ -> False
                _ -> False
        it "processSExpr passes provided ftable and env to processDefine for DefineFun{} branch (env preserved & ftable extended)" $ do
            let ftIn = [("preFn", [], AInteger (I8 1))]
            let envIn = [("preVar", AInteger (I8 2))]
            let header = List [SSymbol "adder", SSymbol "a"]
            let body = List [SSymbol "+", SSymbol "a", SInteger 1]
            let defFun = List [SSymbol "define", header, body]
            processSExpr ftIn envIn defFun `shouldSatisfy` \case
                Right (ftOut, envOut, Nothing) ->
                    case envOut of
                        [("preVar", AInteger (I8 2))] ->
                            case ftOut of
                                (n, ps, _) : _ -> n == "adder" && ps == ["a"]
                                _ -> False
                        _ -> False
                _ -> False

    describe "Integration: processSExpr combined behaviors (kept tests, asserting no regression)" $ do
        it "processSExpr: define then call the defined integer via processSExpr chain" $ do
            let def = List [SSymbol "define", SSymbol "z", SInteger 13]
            processSExpr [] [] def `shouldSatisfy` \case
                Right (_ft1, env1, Nothing) ->
                    isJust (find (\(k, v) -> k == "z" && case v of AInteger (I8 13) -> True; _ -> False) env1)
                _ -> False
        it "processSExpr: define lambda then ensure recursive closure exists (integration)" $ do
            let lambdaSexpr = List [SSymbol "lambda", List [SSymbol "a"], SSymbol "a"]
            let def = List [SSymbol "define", SSymbol "recf", lambdaSexpr]
            processSExpr [] [] def `shouldSatisfy` \case
                Right (_ft, env, Nothing) ->
                    case find (\(k,_) -> k == "recf") env of
                        Just (_, Closure params _ cenv) -> params == ["a"] && isJust (find (\(k',_) -> k' == "recf") cenv)
                        _ -> False
                _ -> False
