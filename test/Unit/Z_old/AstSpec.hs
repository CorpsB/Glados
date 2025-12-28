{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Z_old.AstSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)

import Z_old.Src.Type.Integer (IntValue(..))
import AST.Ast (Ast(..), Env)

spec :: Spec
spec = describe "AST.Ast (structure only)" $ do
  it "constructs and matches every Ast constructor" $ do
    let intAst = AInteger (I32 42)
    let boolAst = ABool True
    let symAst = ASymbol "x"
    let voidAst = AVoid
    let listAst = AList [voidAst, symAst]

    let defineFuncAst = ADefineFunc "f" [("x","Int"), ("y","Int")] "Int" (ASymbol "x")
    let lambdaAst = ADefineLambda ["x","y"] (AList [ASymbol "x", ASymbol "y"])

    let defineStructAst = ADefineStruct "Point" [("x","Int"), ("y","Int")]
    let setVarAst = ASetVar "x" "Int" (AInteger (I8 1))
    let setStructAst = ASetStruct "Point" [("x","Int")]

    let env :: Env
        env = [("a", AInteger (I32 0))]

    let setClosureAst = ASetClosure ["x"] (ASymbol "x") env
    let callAst = ACall (ASymbol "f") [AInteger (I32 1)]
    let importAst = AImport "Std"

    let ifAst = AIf (ABool True) AVoid (ABool False)
    let whileAst = AWhile (ABool True) AVoid
    let forAst = AFor AVoid (ABool True) AVoid AVoid
    let returnAst = AReturn (ASymbol "x")

    let AInteger i = intAst
    i `shouldBe` I32 42

    let ABool b = boolAst
    b `shouldBe` True

    let ASymbol s = symAst
    s `shouldBe` "x"

    let AVoid = voidAst
    pure ()

    let AList [AVoid, ASymbol s2] = listAst
    s2 `shouldBe` "x"

    let ADefineFunc name args ret body = defineFuncAst
    name `shouldBe` "f"
    args `shouldBe` [("x","Int"), ("y","Int")]
    ret `shouldBe` "Int"
    let ASymbol bodyS = body
    bodyS `shouldBe` "x"

    let ADefineLambda params lamBody = lambdaAst
    params `shouldBe` ["x","y"]
    let AList [ASymbol a, ASymbol bb] = lamBody
    a `shouldBe` "x"
    bb `shouldBe` "y"

    let ADefineStruct stName fields = defineStructAst
    stName `shouldBe` "Point"
    fields `shouldBe` [("x","Int"), ("y","Int")]

    let ASetVar vName vTy vVal = setVarAst
    vName `shouldBe` "x"
    vTy `shouldBe` "Int"
    let AInteger (I8 one) = vVal
    one `shouldBe` 1

    let ASetStruct ssName ssFields = setStructAst
    ssName `shouldBe` "Point"
    ssFields `shouldBe` [("x","Int")]

    let ASetClosure capParams capBody capEnv = setClosureAst
    capParams `shouldBe` ["x"]
    let ASymbol capBodyS = capBody
    capBodyS `shouldBe` "x"
    let [("a", AInteger (I32 zero))] = capEnv
    zero `shouldBe` 0

    let ACall callF callArgs = callAst
    let ASymbol callFS = callF
    callFS `shouldBe` "f"
    let [AInteger (I32 one2)] = callArgs
    one2 `shouldBe` 1

    let AImport modName = importAst
    modName `shouldBe` "Std"

    let AIf (ABool c) t e = ifAst
    c `shouldBe` True
    let AVoid = t
    let ABool ee = e
    ee `shouldBe` False

    let AWhile (ABool wc) wb = whileAst
    wc `shouldBe` True
    let AVoid = wb
    pure ()

    let AFor fi (ABool fc) fs fb = forAst
    let AVoid = fi
    fc `shouldBe` True
    let AVoid = fs
    let AVoid = fb
    pure ()

    let AReturn rv = returnAst
    let ASymbol rs = rv
    rs `shouldBe` "x"

  it "Env type alias works ([(Text, Ast)])" $ do
    let env :: Env
        env = [("k", ASymbol "v")]
    let [("k", ASymbol vv)] = env
    vv `shouldBe` "v"

  it "Show derivation prints constructor names" $ do
    show (AInteger (I32 1)) `shouldSatisfy` ("AInteger" `isInfixOf`)
    show (ABool True) `shouldSatisfy` ("ABool" `isInfixOf`)
    show (ASymbol "x") `shouldSatisfy` ("ASymbol" `isInfixOf`)
    show (AList []) `shouldSatisfy` ("AList" `isInfixOf`)
    show (AReturn AVoid) `shouldSatisfy` ("AReturn" `isInfixOf`)
