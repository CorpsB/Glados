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
import qualified Data.Text as DT

spec :: Spec
spec = describe "Eval.Run - Comprehensive Test Suite (target: 100% coverage)" $ do
    describe "Public API: processSExpr basic behaviors (kept & extended)" $ do
        it "processSExpr: evaluates a raw integer SExpr -> returns that integer" $ do
            processSExpr [] [] (SInteger 5) `shouldSatisfy` \case
                Right (ft, env, Just (AInteger (I8 5))) -> null ft && null env
                _ -> False
        it "processSExpr: symbol undefined -> Left error" $ do
            processSExpr [] [] (SSymbol (DT.pack "undefSym")) `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Undefined symbol: undefSym"
                _ -> False

    describe "astFromSexpr (success and error message contains show sexpr)" $ do
        it "astFromSexpr: List [SSymbol \"foo\", SInteger 7] converts to Call AST (Right)" $ do
            let sexpr = List [SSymbol (DT.pack "foo"), SInteger 7]
            astFromSexpr sexpr `shouldSatisfy` \case
                Right (Call (ASymbol s) [AInteger (I8 7)]) -> s == DT.pack "foo"
                _ -> False
        it "astFromSexpr: malformed lambda sexpr returns Left and error message contains shown sexpr" $ do
            let malformed = List [List [], SInteger 7]
            astFromSexpr malformed `shouldSatisfy` \case
                Left err ->
                    "Syntax error: could not convert SExpr to AST:" `isInfixOf` (DT.unpack err)
                    && "List []" `isInfixOf` (DT.unpack err)
                _ -> False

    describe "processDefine (non-closure branch) - env propagation & ftable preserved" $ do
        it "processDefine: define simple integer places (name, AInteger) at head of returned env and preserves rest of env" $ do
            let initialEnv = [(DT.pack "pre", AInteger (I8 99))]
            let ast = Define (DT.pack "x") (AInteger (I8 10))
            processDefine ([] :: FuncTable) initialEnv ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    null ft' &&
                    case env' of
                        ((k, AInteger (I8 10)) : rest) -> k == DT.pack "x" &&
                            case rest of
                                [(k2, AInteger (I8 99))] -> k2 == DT.pack "pre"
                                _ -> False
                        _ -> False
                _ -> False
        it "processDefine: uses provided FuncTable during body evaluation (ft argument to evalAST) - observable via body call" $ do
            let ftWithGetTen = [(DT.pack "getTen", [], AInteger (I8 10))]
            let body = Call (ASymbol (DT.pack "getTen")) []
            let ast = Define (DT.pack "res") body
            processDefine ftWithGetTen [] ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    isJust (find (\(n,_,_) -> n == DT.pack "getTen") ft')
                    && isJust (find (\(k,v) -> k == DT.pack "res" && case v of AInteger (I8 10) -> True; _ -> False) env')
                _ -> False

    describe "processDefine (closure branch) - recursive closure formation & env/ftable retention" $ do
        it "processDefine: when body is Lambda, create recursive closure containing self in its cenv" $ do
            let body = Lambda [DT.pack "a"] (ASymbol (DT.pack "a"))
            let ast = Define (DT.pack "f") body
            processDefine ([] :: FuncTable) [] ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    null ft' &&
                    case find (\(k,_) -> k == DT.pack "f") env' of
                        Just (_, Closure params b cenv) ->
                            params == [DT.pack "a"]
                            && case b of ASymbol s -> s == DT.pack "a"; _ -> False
                            && isJust (find (\(k', _) -> k' == DT.pack "f") cenv)
                        _ -> False
                _ -> False
        it "processDefine: closure stored at head of env and ftable from input preserved" $ do
            let body = Lambda [] (AInteger (I8 1))
            let ast = Define (DT.pack "g") body
            let initialFT = [(DT.pack "some", [], AInteger (I8 0))]
            processDefine initialFT [] ast `shouldSatisfy` \case
                Right (ft', envReturned, Nothing) ->
                    isJust (find (\(n,_,_) -> n == DT.pack "some") ft')
                    &&
                    case envReturned of
                        ((k, Closure _ b cenv) : rest) -> k == DT.pack "g" &&
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
            let ast = Define (DT.pack "g") (Lambda [] (AInteger (I8 1)))
            processDefine ([] :: FuncTable) [] ast `shouldSatisfy` \case
                Right (_ft, envOut, Nothing) ->
                    case find (\(k,_) -> k == DT.pack "g") envOut of
                        Just (_, Closure _ _ cenv) ->
                            case find (\(k, _) -> k == DT.pack "g") cenv of
                                Just (_, Closure _ _ innerCenv) ->
                                    isJust (find (\(k',_) -> k' == DT.pack "g") innerCenv)
                                _ -> False
                        _ -> False
                _ -> False

    describe "processDefine: DefineFun (registerFunction) success & failure" $ do
        it "DefineFun success: returns updated FuncTable with the new function entry and preserves env argument" $ do
            let envBefore = [(DT.pack "x", AInteger (I8 11))]
            let body = Call (ASymbol (DT.pack "+")) [ASymbol (DT.pack "a"), ASymbol (DT.pack "b")]
            let ast = DefineFun (DT.pack "add") [DT.pack "a", DT.pack "b"] body
            processDefine ([] :: FuncTable) envBefore ast `shouldSatisfy` \case
                Right (ft', env', Nothing) ->
                    case env' of
                        [(k, AInteger (I8 11))] -> k == DT.pack "x" && case ft' of
                            (n, ps, bdy) : _ -> n == DT.pack "add" && ps == [DT.pack "a",DT.pack "b"] && case bdy of Call (ASymbol s) _ -> s == DT.pack "+"; _ -> False
                            _ -> False
                        _ -> False
                _ -> False
        it "DefineFun failure if function already exists -> Left error with exact message" $ do
            let existingFT = [(DT.pack "dup", [DT.pack "x"], AInteger (I8 0))]
            let ast = DefineFun (DT.pack "dup") [DT.pack "x"] (AInteger (I8 1))
            processDefine existingFT [] ast `shouldSatisfy` \case
                Left err -> err == DT.pack "*** ERROR: Function already exists: dup"
                _ -> False

    describe "processDefine: error on non-Define AST" $ do
        it "processDefine called with AInteger should return the specific Left error" $ do
            processDefine ([] :: FuncTable) [] (AInteger (I8 5)) `shouldSatisfy` \case
                Left err -> err == DT.pack "processDefine called with non-define AST"
                _ -> False
        it "processDefine called with Call should return the specific Left error" $ do
            processDefine ([] :: FuncTable) [] (Call (ASymbol (DT.pack "+")) []) `shouldSatisfy` \case
                Left err -> err == DT.pack "processDefine called with non-define AST"
                _ -> False

    describe "processCallOrEval: evalAST receives the ftable/env arguments (via observable results)" $ do
        it "processCallOrEval: calling a function present in ftable yields result showing ftable was used and env preserved" $ do
            let ft = [(DT.pack "getSeven", [], AInteger (I8 7))]
            let env = [(DT.pack "x", AInteger (I8 1))]
            let ast = Call (ASymbol (DT.pack "getSeven")) []
            processCallOrEval ft env ast `shouldSatisfy` \case
                Right (ft', env', Just (AInteger (I8 7))) ->
                    isJust (find (\(n,_,_) -> n == DT.pack "getSeven") ft')
                    && isJust (find (\(k, v) -> k == DT.pack "x" && case v of AInteger (I8 1) -> True; _ -> False) env')
                _ -> False
        it "processCallOrEval: when ftable lacks the func, evalAST returns Left and processCallOrEval propagates it" $ do
            let ftEmpty = [] :: FuncTable
            let env = []
            let ast = Call (ASymbol (DT.pack "noFn")) []
            processCallOrEval ftEmpty env ast `shouldSatisfy` \case
                Left err -> "*** ERROR: Unknown func: noFn" `isInfixOf` (DT.unpack err)
                _ -> False
        it "processCallOrEval: argument error in evalAST is propagated (e.g., undefined symbol used inside AST)" $ do
            let ft = [] :: FuncTable
            let ast = Call (ASymbol (DT.pack "+")) [ASymbol (DT.pack "someUndefined")]
            processCallOrEval ft [] ast `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "processSExpr: verifies Define{} and DefineFun{} branches use provided ftable and env" $ do
        it "processSExpr passes provided ftable and env to processDefine for Define{} branch (ftable preserved & env extended)" $ do
            let ftIn = [(DT.pack "preFn", [], AInteger (I8 1))]
            let envIn = [(DT.pack "preVar", AInteger (I8 2))]
            let def = List [SSymbol (DT.pack "define"), SSymbol (DT.pack "z"), SInteger 13]
            processSExpr ftIn envIn def `shouldSatisfy` \case
                Right (ftOut, envOut, Nothing) ->
                    isJust (find (\(n,_,_) -> n == DT.pack "preFn") ftOut)
                    &&
                    case envOut of
                        ((k, AInteger (I8 13)) : rest) -> k == DT.pack "z" && case rest of
                            [(k2, AInteger (I8 2))] -> k2 == DT.pack "preVar"
                            _ -> False
                        _ -> False
                _ -> False
        it "processSExpr passes provided ftable and env to processDefine for DefineFun{} branch (env preserved & ftable extended)" $ do
            let ftIn = [(DT.pack "preFn", [], AInteger (I8 1))]
            let envIn = [(DT.pack "preVar", AInteger (I8 2))]
            let header = List [SSymbol (DT.pack "adder"), SSymbol (DT.pack "a")]
            let body = List [SSymbol (DT.pack "+"), SSymbol (DT.pack "a"), SInteger 1]
            let defFun = List [SSymbol (DT.pack "define"), header, body]
            processSExpr ftIn envIn defFun `shouldSatisfy` \case
                Right (ftOut, envOut, Nothing) ->
                    case envOut of
                        [(k, AInteger (I8 2))] -> k == DT.pack "preVar" &&
                            case ftOut of
                                (n, ps, _) : _ -> n == DT.pack "adder" && ps == [DT.pack "a"]
                                _ -> False
                        _ -> False
                _ -> False

    describe "Integration: processSExpr combined behaviors (kept tests, asserting no regression)" $ do
        it "processSExpr: define then call the defined integer via processSExpr chain" $ do
            let def = List [SSymbol (DT.pack "define"), SSymbol (DT.pack "z"), SInteger 13]
            processSExpr [] [] def `shouldSatisfy` \case
                Right (_ft1, env1, Nothing) ->
                    isJust (find (\(k, v) -> k == DT.pack "z" && case v of AInteger (I8 13) -> True; _ -> False) env1)
                _ -> False
        it "processSExpr: define lambda then ensure recursive closure exists (integration)" $ do
            let lambdaSexpr = List [SSymbol (DT.pack "lambda"), List [SSymbol (DT.pack "a")], SSymbol (DT.pack "a")]
            let def = List [SSymbol (DT.pack "define"), SSymbol (DT.pack "recf"), lambdaSexpr]
            processSExpr [] [] def `shouldSatisfy` \case
                Right (_ft, env, Nothing) ->
                    case find (\(k,_) -> k == DT.pack "recf") env of
                        Just (_, Closure params _ cenv) -> params == [DT.pack "a"] && isJust (find (\(k',_) -> k' == DT.pack "recf") cenv)
                        _ -> False
                _ -> False
