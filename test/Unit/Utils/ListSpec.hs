{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ListSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Utils.ListSpec (spec) where

import Test.Hspec
import Utils.List (sameLength, astToList, listToAst)
import Ast (Ast(..))
import Type.Integer (IntValue(..))
import qualified Data.Text as DT
import Data.List (isInfixOf)

spec :: Spec
spec = describe "Utils - List unit tests" $ do
    
    describe "sameLength" $ do
        it "Both lists empty" $ do
            sameLength ([] :: [Int]) ([] :: [Int]) `shouldSatisfy` (== True)
        it "Both lists non-empty and same length" $ do
            sameLength ([1, 2, 3] :: [Int]) ([4, 5, 6] :: [Int]) `shouldSatisfy` (== True)
        it "First list shorter" $ do
            sameLength ([1, 2] :: [Int]) ([4, 5, 6] :: [Int]) `shouldSatisfy` (== False)
        it "Second list shorter" $ do
            sameLength ([1, 2, 3] :: [Int]) ([4, 5] :: [Int]) `shouldSatisfy` (== False)

    describe "astToList" $ do
        it "Successfully extracts list content from AList" $ do
            let content = [AInteger (I8 1), AInteger (I8 2)]
            astToList (AList content) `shouldSatisfy` \case
                Right xs -> show xs == show content
                _ -> False

        it "Returns error when input is not AList" $ do
            let input = AInteger (I8 42)
            astToList input `shouldSatisfy` \case
                Left err -> "*** ERROR: Expected list, got:" `isInfixOf` DT.unpack err
                _ -> False

    describe "listToAst" $ do
        it "Wraps a list of Ast into AList constructor" $ do
            let content = [AInteger (I8 10), ABool True]
            listToAst content `shouldSatisfy` \case
                AList xs -> show xs == show content
                _ -> False
