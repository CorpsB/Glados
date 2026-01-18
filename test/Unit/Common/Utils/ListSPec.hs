{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Common.Utils.List unit tests
-}

module Common.Utils.ListSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Common.Utils.List (listEq, zipWith3M_)

spec :: Spec
spec = describe "Common.Utils.List" $ do

  describe "listEq (coverage of all branches)" $ do
    it "covers [] [] = True" $ do
      evaluate (listEq ([] :: [Int]) ([] :: [Char])) `shouldReturn` True

    it "covers (_:xs) (_:ys) recursion branch" $ do
      evaluate (listEq [1,2,3 :: Int] ['a','b','c']) `shouldReturn` True

    it "covers fallback _ _ = False (length mismatch)" $ do
      evaluate (listEq [1,2 :: Int] ['a']) `shouldReturn` False
      evaluate (listEq ([] :: [Int]) ['x']) `shouldReturn` False
      evaluate (listEq [1 :: Int] ([] :: [Char])) `shouldReturn` False

  describe "zipWith3M_" $ do
    it "runs without crashing (basic smoke test)" $ do
      zipWith3M_ (\_ _ _ -> pure ()) [1::Int] [2::Int] [3::Int]
