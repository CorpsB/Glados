{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Z_old.AstSpec (spec) where

import Test.Hspec
import Z_old.Src.Type.Integer (IntValue(..))
import Z_old.Src.Ast (OldAst(..), showAst, printAst)

spec :: Spec
spec = describe "Z_old.Src.Ast" $ do
  describe "showAst" $ do
    it "covers all showAst branches" $ do
      showAst (AInteger (I8 42)) `shouldBe` "42"
      showAst (ABool True) `shouldBe` "#t"
      showAst (ABool False) `shouldBe` "#f"
      showAst (ASymbol "x") `shouldBe` "x"
      showAst (AList [AInteger (I8 1), AInteger (I8 2)]) `shouldBe` "(1 2)"
      showAst (Closure ["x"] (ASymbol "x") []) `shouldBe` "#\\<procedure\\>"
      showAst (Lambda ["x"] (ASymbol "x")) `shouldBe` "#<lambda>"
      showAst AVoid `shouldContain` "AVoid"

  describe "printAst" $ do
    it "runs and returns ()" $ do
      printAst (AInteger (I8 1)) `shouldReturn` ()
