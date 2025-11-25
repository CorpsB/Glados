{-# LANGUAGE LambdaCase #-}

module LispTest (spec) where

import Test.Hspec
import Lisp (SExpr(..), getInteger, getSymbol, getList)

spec :: Spec
spec = describe "Lisp Data Structures" $ do
    describe "getInteger" $ do
        it "extracts integer" $ do
            getInteger (SInteger 42) `shouldSatisfy` \case
                Just 42 -> True
                _ -> False
        it "returns Nothing for Symbol" $ do
            getInteger (SSymbol "a") `shouldSatisfy` \case
                Nothing -> True
                _ -> False

    describe "getSymbol" $ do
        it "extracts symbol" $ do
            getSymbol (SSymbol "test") `shouldSatisfy` \case
                Just "test" -> True
                _ -> False

    describe "getList" $ do
        it "extracts list" $ do
            getList (List []) `shouldSatisfy` \case
                Just [] -> True
                _ -> False
