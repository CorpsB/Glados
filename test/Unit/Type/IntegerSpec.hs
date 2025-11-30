{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- IntegerSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Type.IntegerSpec (spec) where

import Test.Hspec
import Type.Integer (IntValue(..), fitInteger, toInt64, fromInt64, intValueEq, intValueToInt)

spec :: Spec
spec = describe "Type.Integer - Smart Integer Storage" $ do
    describe "fromInt64 (Compression Logic)" $ do
        it "Compresses small numbers into Int8 (-128 to 127)" $ do
            fromInt64 0 `shouldBe` I8 0
            fromInt64 127 `shouldBe` I8 127
            fromInt64 (-128) `shouldBe` I8 (-128)
        it "Compresses medium numbers into Int16" $ do
            fromInt64 128 `shouldBe` I16 128
            fromInt64 (-129) `shouldBe` I16 (-129)
            fromInt64 32767 `shouldBe` I16 32767
            fromInt64 (-32768) `shouldBe` I16 (-32768)
        it "Compresses large numbers into Int32" $ do
            fromInt64 32768 `shouldBe` I32 32768
            fromInt64 (-32769) `shouldBe` I32 (-32769)
            fromInt64 2147483647 `shouldBe` I32 2147483647
            fromInt64 (-2147483648) `shouldBe` I32 (-2147483648)
        it "Keeps huge numbers in Int64" $ do
            fromInt64 2147483648 `shouldBe` I64 2147483648
            fromInt64 (-2147483649) `shouldBe` I64 (-2147483649)
            fromInt64 9000000000000000000 `shouldBe` I64 9000000000000000000

    describe "toInt64 (Expansion Logic)" $ do
        it "Expands I8 correctly" $ do
            toInt64 (I8 100) `shouldBe` 100
            toInt64 (I8 (-100)) `shouldBe` (-100)
        it "Expands I16 correctly" $ do
            toInt64 (I16 1000) `shouldBe` 1000
        it "Expands I32 correctly" $ do
            toInt64 (I32 100000) `shouldBe` 100000
        it "Expands I64 correctly" $ do
            toInt64 (I64 5000000000) `shouldBe` 5000000000

    describe "fitInteger" $ do
        it "Adapts standard Int to appropriate IntValue" $ do
            fitInteger 42 `shouldBe` I8 42
            fitInteger 1000 `shouldBe` I16 1000
            fitInteger 100000 `shouldBe` I32 100000

    describe "intValueToInt" $ do
        it "Converts I8 to Int" $ do
            intValueToInt (I8 42) `shouldBe` (42 :: Int)
        it "Converts I16 to Int" $ do
            intValueToInt (I16 1000) `shouldBe` (1000 :: Int)
        it "Converts I32 to Int" $ do
            intValueToInt (I32 100000) `shouldBe` (100000 :: Int)
        it "Converts I64 to Int" $ do
            intValueToInt (I64 100000) `shouldBe` (100000 :: Int)

    describe "intValueEq" $ do
        it "Returns True for equal values regardless of container" $ do
            intValueEq (I8 10) 10 `shouldBe` True
            intValueEq (I16 10) 10 `shouldBe` True
        it "Returns False for distinct values" $ do
            intValueEq (I8 10) 11 `shouldBe` False
            intValueEq (I32 100) 200 `shouldBe` False

    describe "Derived Instances" $ do
        describe "Eq Instance" $ do
            it "Eq instance works between IntValues" $ do
                (I8 10) == (I8 10) `shouldBe` True
                (I8 10) /= (I8 11) `shouldBe` True

        describe "Show Instance (All Constructors)" $ do
            it "Formats I8" $ do
                show (I8 127) `shouldBe` "I8 127"
            it "Formats I16" $ do
                show (I16 32000) `shouldBe` "I16 32000"
            it "Formats I32" $ do
                show (I32 100000) `shouldBe` "I32 100000"
            it "Formats I64" $ do
                show (I64 999999999) `shouldBe` "I64 999999999"

        describe "Ord Instance (All Comparators & Constructors)" $ do
            it "Compares values inside same constructor" $ do
                (I8 10) < (I8 20) `shouldBe` True
                (I8 20) > (I8 10) `shouldBe` True
                (I8 10) <= (I8 10) `shouldBe` True
                (I8 10) >= (I8 10) `shouldBe` True
                compare (I8 10) (I8 20) `shouldBe` LT
                compare (I8 20) (I8 10) `shouldBe` GT
                compare (I8 10) (I8 10) `shouldBe` EQ
            it "Compares constructors based on definition order (I8 < I16 < I32 < I64)" $ do
                (I8 100) < (I16 0) `shouldBe` True  
                (I16 100) < (I32 0) `shouldBe` True 
                (I32 100) < (I64 0) `shouldBe` True
                (I64 0) > (I32 100) `shouldBe` True
                (I32 0) > (I16 100) `shouldBe` True
                (I16 0) > (I8 100) `shouldBe` True
