{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AST.AstSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)

import Z_old.Src.Type.Integer (IntValue(..))
import AST.Ast (Ast(..), Env, showAst, printAst, cleanAst)


spec :: Spec
spec = describe "AST Core Coverage" $ do

    describe "Constructor & Structure Checks (No Eq)" $ do
        
        it "Constructs AInteger" $ do
            let ast = AInteger (I32 42)
            ast `shouldSatisfy` \case AInteger (I32 42) -> True; _ -> False

        it "Constructs ABool" $ do
            let ast = ABool True
            ast `shouldSatisfy` \case ABool True -> True; _ -> False

        it "Constructs ASymbol" $ do
            let ast = ASymbol "x"
            ast `shouldSatisfy` \case ASymbol "x" -> True; _ -> False

        it "Constructs AVoid" $ do
            let ast = AVoid
            ast `shouldSatisfy` \case AVoid -> True; _ -> False

        it "Constructs AList" $ do
            let ast = AList [AVoid, ASymbol "x"]
            ast `shouldSatisfy` \case AList [AVoid, ASymbol "x"] -> True; _ -> False

        it "Constructs ADefineFunc" $ do
            let ast = ADefineFunc "f" [("x","Int"), ("y","Int")] "Int" (ASymbol "x")
            ast `shouldSatisfy` \case 
                ADefineFunc "f" [("x","Int"), ("y","Int")] "Int" (ASymbol "x") -> True
                _ -> False

        it "Constructs ADefineLambda" $ do
            let ast = ADefineLambda ["x","y"] (AList [ASymbol "x", ASymbol "y"])
            ast `shouldSatisfy` \case 
                ADefineLambda ["x","y"] (AList [ASymbol "x", ASymbol "y"]) -> True
                _ -> False

        it "Constructs ADefineStruct" $ do
            let ast = ADefineStruct "Point" [("x","Int"), ("y","Int")]
            ast `shouldSatisfy` \case 
                ADefineStruct "Point" [("x","Int"), ("y","Int")] -> True
                _ -> False

        it "Constructs ASetVar" $ do
            let ast = ASetVar "x" "Int" (AInteger (I8 1))
            ast `shouldSatisfy` \case 
                ASetVar "x" "Int" (AInteger (I8 1)) -> True
                _ -> False

        it "Constructs ASetStruct" $ do
            let ast = ASetStruct "Point" [("x", AInteger (I8 2))]
            ast `shouldSatisfy` \case 
                ASetStruct "Point" [("x", AInteger (I8 2))] -> True
                _ -> False

        it "Constructs ASetClosure" $ do
            let envVal = [("a", AInteger (I32 0))]
            let ast = ASetClosure ["x"] (ASymbol "x") envVal
            ast `shouldSatisfy` \case 
                ASetClosure ["x"] (ASymbol "x") [("a", AInteger (I32 0))] -> True
                _ -> False

        it "Constructs ACall" $ do
            let ast = ACall (ASymbol "f") [AInteger (I32 1)]
            ast `shouldSatisfy` \case 
                ACall (ASymbol "f") [AInteger (I32 1)] -> True
                _ -> False

        it "Constructs AImport" $ do
            let ast = AImport "Std"
            ast `shouldSatisfy` \case AImport "Std" -> True; _ -> False

        it "Constructs AIf" $ do
            let ast = AIf (ABool True) AVoid (ABool False)
            ast `shouldSatisfy` \case 
                AIf (ABool True) AVoid (ABool False) -> True
                _ -> False

        it "Constructs AWhile" $ do
            let ast = AWhile (ABool True) AVoid
            ast `shouldSatisfy` \case AWhile (ABool True) AVoid -> True; _ -> False

        it "Constructs AFor" $ do
            let ast = AFor AVoid (ABool True) AVoid AVoid
            ast `shouldSatisfy` \case 
                AFor AVoid (ABool True) AVoid AVoid -> True
                _ -> False

        it "Constructs AReturn" $ do
            let ast = AReturn (ASymbol "x")
            ast `shouldSatisfy` \case AReturn (ASymbol "x") -> True; _ -> False

        it "Matches Env type structure" $ do
            let env :: Env
                env = [("k", ASymbol "v")]
            env `shouldSatisfy` \case [("k", ASymbol "v")] -> True; _ -> False

    describe "showAst (S-Expression Formatting)" $ do
        
        it "Formats Integers" $ do
            showAst (AInteger (I8 42)) `shouldSatisfy` (== "42")

        it "Formats Booleans" $ do
            showAst (ABool True) `shouldSatisfy` (== "#t")
            showAst (ABool False) `shouldSatisfy` (== "#f")

        it "Formats Symbols" $ do
            showAst (ASymbol "myVar") `shouldSatisfy` (== "myVar")

        it "Formats Lists" $ do
            let list = AList [ASymbol "+", AInteger (I8 1), AInteger (I8 2)]
            showAst list `shouldSatisfy` (== "(+ 1 2)")

        it "Formats Closure" $ do
            let closure = ASetClosure ["x"] AVoid []
            showAst closure `shouldSatisfy` (== "#\\<procedure\\>")

        it "Formats Lambda" $ do
            let lambda = ADefineLambda ["x"] AVoid
            showAst lambda `shouldSatisfy` (== "#<lambda>")

        it "Formats APos (Transparency)" $ do
            let node = APos 10 5 (ASymbol "x")
            showAst node `shouldSatisfy` (== "x")

        it "Formats 'other' nodes (Deriving Show Check)" $ do
            showAst AVoid `shouldSatisfy` ("AVoid" `isInfixOf`)
            showAst (AImport "lib") `shouldSatisfy` ("AImport" `isInfixOf`)

    describe "cleanAst (Recursion & Wrappers)" $ do
        
        it "Cleans ADefineLambda body" $ do
            let dirtyBody = APos 1 1 (AInteger (I8 42))
            let lambda = ADefineLambda ["x"] dirtyBody
            cleanAst lambda `shouldSatisfy` \case 
                ADefineLambda ["x"] (AInteger (I8 42)) -> True
                _ -> False

        it "Cleans ASetClosure body and environment" $ do
            let dirtyBody = APos 1 1 AVoid
            let dirtyEnv = [("var", APos 2 2 (AInteger (I8 10)))]
            let closure = ASetClosure ["args"] dirtyBody dirtyEnv
            cleanAst closure `shouldSatisfy` \case 
                ASetClosure ["args"] AVoid [("var", AInteger (I8 10))] -> True
                _ -> False

        it "Cleans AList recursively" $ do
            let list = AList [APos 1 1 (AInteger (I8 1)), APos 2 2 (ASymbol "x")]
            cleanAst list `shouldSatisfy` \case 
                AList [AInteger (I8 1), ASymbol "x"] -> True
                _ -> False
            
        it "Cleans ASetVar recursively" $ do
            let dirtyVar = ASetVar "x" "int" (APos 5 5 (AInteger (I8 10)))
            cleanAst dirtyVar `shouldSatisfy` \case 
                ASetVar "x" "int" (AInteger (I8 10)) -> True
                _ -> False

        it "Cleans ACall recursively" $ do
            let dirtyCall = ACall (APos 1 1 (ASymbol "f")) [APos 2 2 (AInteger (I8 1))]
            cleanAst dirtyCall `shouldSatisfy` \case
                ACall (ASymbol "f") [AInteger (I8 1)] -> True
                _ -> False

        it "Cleans AIf recursively" $ do
            let dirtyIf = AIf (APos 1 1 (ABool True)) (APos 2 2 AVoid) (APos 3 3 AVoid)
            cleanAst dirtyIf `shouldSatisfy` \case
                AIf (ABool True) AVoid AVoid -> True
                _ -> False
                
        it "Cleans AWhile recursively" $ do
            let dirtyWhile = AWhile (APos 1 1 (ABool True)) (APos 2 2 AVoid)
            cleanAst dirtyWhile `shouldSatisfy` \case
                AWhile (ABool True) AVoid -> True
                _ -> False

        it "Cleans AFor recursively" $ do
            let dirtyFor = AFor (APos 1 1 AVoid) (APos 2 2 (ABool True)) (APos 3 3 AVoid) (APos 4 4 AVoid)
            cleanAst dirtyFor `shouldSatisfy` \case
                AFor AVoid (ABool True) AVoid AVoid -> True
                _ -> False
        
        it "Cleans AReturn recursively" $ do
            let dirtyRet = AReturn (APos 1 1 AVoid)
            cleanAst dirtyRet `shouldSatisfy` \case
                AReturn AVoid -> True
                _ -> False
        
        it "Cleans ASetStruct recursively" $ do
            let dirtyStruct = ASetStruct "S" [("f", APos 1 1 AVoid)]
            cleanAst dirtyStruct `shouldSatisfy` \case
                ASetStruct "S" [("f", AVoid)] -> True
                _ -> False
                
        it "Cleans ADefineFunc recursively" $ do
            let dirtyFunc = ADefineFunc "f" [] "void" (APos 1 1 AVoid)
            cleanAst dirtyFunc `shouldSatisfy` \case
                ADefineFunc "f" [] "void" AVoid -> True
                _ -> False

    describe "printAst (IO Side Effects)" $ do
        it "Executes without crashing" $ do
            printAst (AInteger (I8 10)) `shouldReturn` ()
