{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ListSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Utils.ListSpec (spec) where

import Test.Hspec
import Utils.List (sameLength)

spec :: Spec
spec = describe "Utils - List unit tests" $ do
    describe "sameLength" $ do
        it "Both lists empty" $ do
            sameLength ([] :: [Int]) ([] :: [Int]) `shouldBe` True
        it "Both lists non-empty and same length" $ do
            sameLength ([1, 2, 3] :: [Int]) ([4, 5, 6] :: [Int]) `shouldBe` True
        it "First list shorter" $ do
            sameLength ([1, 2] :: [Int]) ([4, 5, 6] :: [Int]) `shouldBe` False
        it "Second list shorter" $ do
            sameLength ([1, 2, 3] :: [Int]) ([4, 5] :: [Int]) `shouldBe` False
