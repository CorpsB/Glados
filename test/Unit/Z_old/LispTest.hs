{-# LANGUAGE LambdaCase #-}

module Z_old.LispTest (spec) where

import Test.Hspec
import Z_old.Src.Lisp (SExpr(..), getInteger, getSymbol, getList)
import qualified Data.Text as DT

spec :: Spec
spec = describe "Lisp Data Structures" $ do
    
    describe "getInteger" $ do
        it "extracts integer" $ do
            getInteger (SInteger 42) `shouldSatisfy` \case
                Just 42 -> True
                _ -> False
        it "returns Nothing for Symbol" $ do
            getInteger (SSymbol (DT.pack "a")) `shouldSatisfy` \case
                Nothing -> True
                _ -> False

    describe "getSymbol" $ do
        it "extracts symbol" $ do
            getSymbol (SSymbol (DT.pack "test")) `shouldSatisfy` \case
                Just s -> s == DT.pack "test"
                _ -> False
        it "returns Nothing for Integer" $ do
            getSymbol (SInteger 42) `shouldSatisfy` \case
                Nothing -> True
                _ -> False

    describe "getList" $ do
        it "extracts list" $ do
            getList (List []) `shouldSatisfy` \case
                Just [] -> True
                _ -> False
        it "returns Nothing for Symbol" $ do
            getList (SSymbol (DT.pack "foo")) `shouldSatisfy` \case
                Nothing -> True
                _ -> False

    describe "Show Instance" $ do
        it "shows SInteger correctly" $ do
            show (SInteger 42) `shouldSatisfy` \case
                "SInteger 42" -> True
                _ -> False
        it "shows SSymbol correctly" $ do
            show (SSymbol (DT.pack "foo")) `shouldSatisfy` \case
                "SSymbol \"foo\"" -> True
                _ -> False
        it "shows List correctly" $ do
            show (List []) `shouldSatisfy` \case
                "List []" -> True
                _ -> False
