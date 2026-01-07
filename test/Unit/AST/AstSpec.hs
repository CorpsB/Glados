{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module AST.AstSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)

import Common.Type.Integer (IntValue(..))
import AST.Ast (Ast(..), showAst, printAst)
import Data.Text (Text)

type Env = [(Text, Ast)]

spec :: Spec
spec = describe "AST.Ast" $ do
  it "constructs and matches every Ast constructor" $ do
    let intAst = AInteger (I32 42)
    intAst `shouldBe` AInteger (I32 42)

    let boolAst = ABool True
    boolAst `shouldBe` ABool True

    let symAst = ASymbol "x"
    symAst `shouldBe` ASymbol "x"

    let voidAst = AVoid
    voidAst `shouldBe` AVoid

    let listAst = AList [voidAst, symAst]
    listAst `shouldBe` AList [AVoid, ASymbol "x"]

    let defineFuncAst = ADefineFunc "f" [("x","Int"), ("y","Int")] "Int" (ASymbol "x")
    defineFuncAst `shouldBe` ADefineFunc "f" [("x","Int"), ("y","Int")] "Int" (ASymbol "x")

    let lambdaAst = ADefineLambda ["x","y"] (AList [ASymbol "x", ASymbol "y"])
    lambdaAst `shouldBe` ADefineLambda ["x","y"] (AList [ASymbol "x", ASymbol "y"])

    let defineStructAst = ADefineStruct "Point" [("x","Int"), ("y","Int")]
    defineStructAst `shouldBe` ADefineStruct "Point" [("x","Int"), ("y","Int")]

    let setVarAst = ASetVar "x" "Int" (AInteger (I8 1))
    setVarAst `shouldBe` ASetVar "x" "Int" (AInteger (I8 1))

    let setStructAst = ASetStruct "Point" [("x", AInteger (I8 2))]
    setStructAst `shouldBe` ASetStruct "Point" [("x", AInteger (I8 2))]

    let callAst = ACall (ASymbol "f") [AInteger (I32 1)]
    callAst `shouldBe` ACall (ASymbol "f") [AInteger (I32 1)]

    let importAst = AImport "Std"
    importAst `shouldBe` AImport "Std"

    let ifAst = AIf (ABool True) AVoid (ABool False)
    ifAst `shouldBe` AIf (ABool True) AVoid (ABool False)

    let whileAst = AWhile (ABool True) AVoid
    whileAst `shouldBe` AWhile (ABool True) AVoid

    let forAst = AFor AVoid (ABool True) AVoid AVoid
    forAst `shouldBe` AFor AVoid (ABool True) AVoid AVoid

    let returnAst = AReturn (ASymbol "x")
    returnAst `shouldBe` AReturn (ASymbol "x")

  it "Env type alias works ([(Text, Ast)])" $ do
    let env :: Env
        env = [("k", ASymbol "v")]
    env `shouldBe` [("k", ASymbol "v")]

  describe "showAst" $ do
    it "covers all showAst branches" $ do
      showAst (AInteger (I32 42)) `shouldBe` "42"
      showAst (ABool True) `shouldBe` "#t"
      showAst (ABool False) `shouldBe` "#f"
      showAst (ASymbol "x") `shouldBe` "x"
      showAst (AList [ABool True, AInteger (I32 1)]) `shouldBe` "(#t 1)"
      showAst (ADefineLambda ["x"] (ASymbol "x")) `shouldBe` "#<lambda>"
      showAst (AIf (ABool True) AVoid (ABool False)) `shouldSatisfy` ("AIf" `isInfixOf`)

  describe "Derived Eq instance (coverage for generated code)" $ do
    it "Eq is used (same constructor)" $ do
      (AInteger (I32 1) == AInteger (I32 1)) `shouldBe` True
      (AInteger (I32 1) == AInteger (I32 2)) `shouldBe` False

    it "Eq is used (different constructors)" $ do
      (AInteger (I32 1) == ABool True) `shouldBe` False
      (ASymbol "x" == AVoid) `shouldBe` False

    it "Eq recurses (lists)" $ do
      (AList [ABool True, AInteger (I32 1)] == AList [ABool True, AInteger (I32 1)]) `shouldBe` True
      (AList [ABool True] == AList [ABool False]) `shouldBe` False

  describe "Derived Show instance (coverage for generated code)" $ do
    it "Show prints constructor names" $ do
      show (AInteger (I32 1)) `shouldSatisfy` ("AInteger" `isInfixOf`)
      show (ABool True) `shouldSatisfy` ("ABool" `isInfixOf`)
      show (ASymbol "x") `shouldSatisfy` ("ASymbol" `isInfixOf`)
      show (AList []) `shouldSatisfy` ("AList" `isInfixOf`)
      show (AReturn AVoid) `shouldSatisfy` ("AReturn" `isInfixOf`)
      show (ADefineFunc "f" [] "Int" AVoid) `shouldSatisfy` ("ADefineFunc" `isInfixOf`)

  describe "printAst" $ do
    it "printAst returns () (smoke)" $ do
      printAst (AInteger (I32 1)) `shouldReturn` ()
      printAst (AIf (ABool True) AVoid (ABool False)) `shouldReturn` ()
