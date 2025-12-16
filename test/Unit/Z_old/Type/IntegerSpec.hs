{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- IntegerSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Z_old.Type.IntegerSpec (spec) where

import Test.Hspec
import Z_old.Src.Type.Integer (IntValue(..), fitInteger, toInt64, fromInt64, intValueEq, intValueToInt)

spec :: Spec
spec = describe "Type.Integer - Smart Integer Storage" $ do
    describe "fromInt64 (Compression Logic)" $ do
        it "Compresses small numbers into Int8 (-128 to 127)" $ do
            fromInt64 0 `shouldSatisfy` (== I8 0)
            fromInt64 127 `shouldSatisfy` (== I8 127)
            fromInt64 (-128) `shouldSatisfy` (== I8 (-128))
        it "Compresses medium numbers into Int16" $ do
            fromInt64 128 `shouldSatisfy` (== I16 128)
            fromInt64 (-129) `shouldSatisfy` (== I16 (-129))
            fromInt64 32767 `shouldSatisfy` (== I16 32767)
            fromInt64 (-32768) `shouldSatisfy` (== I16 (-32768))
        it "Compresses large numbers into Int32" $ do
            fromInt64 32768 `shouldSatisfy` (== I32 32768)
            fromInt64 (-32769) `shouldSatisfy` (== I32 (-32769))
            fromInt64 2147483647 `shouldSatisfy` (== I32 2147483647)
            fromInt64 (-2147483648) `shouldSatisfy` (== I32 (-2147483648))
        it "Keeps huge numbers in Int64" $ do
            fromInt64 2147483648 `shouldSatisfy` (== I64 2147483648)
            fromInt64 (-2147483649) `shouldSatisfy` (== I64 (-2147483649))
            fromInt64 9000000000000000000 `shouldSatisfy` (== I64 9000000000000000000)

    describe "toInt64 (Expansion Logic)" $ do
        it "Expands I8 correctly" $ do
            toInt64 (I8 100) `shouldSatisfy` (== 100)
            toInt64 (I8 (-100)) `shouldSatisfy` (== (-100))
        it "Expands I16 correctly" $ do
            toInt64 (I16 1000) `shouldSatisfy` (== 1000)
        it "Expands I32 correctly" $ do
            toInt64 (I32 100000) `shouldSatisfy` (== 100000)
        it "Expands I64 correctly" $ do
            toInt64 (I64 5000000000) `shouldSatisfy` (== 5000000000)

    describe "fitInteger" $ do
        it "Adapts standard Int to appropriate IntValue" $ do
            fitInteger 42 `shouldSatisfy` (== I8 42)
            fitInteger 1000 `shouldSatisfy` (== I16 1000)
            fitInteger 100000 `shouldSatisfy` (== I32 100000)

    describe "intValueToInt" $ do
        it "Converts I8 to Int" $ do
            intValueToInt (I8 42) `shouldSatisfy` (== 42)
        it "Converts I16 to Int" $ do
            intValueToInt (I16 1000) `shouldSatisfy` (== 1000)
        it "Converts I32 to Int" $ do
            intValueToInt (I32 100000) `shouldSatisfy` (== 100000)
        it "Converts I64 to Int" $ do
            intValueToInt (I64 100000) `shouldSatisfy` (== 100000)

    describe "intValueEq" $ do
        it "Returns True for equal values regardless of container" $ do
            intValueEq (I8 10) 10 `shouldSatisfy` (== True)
            intValueEq (I16 10) 10 `shouldSatisfy` (== True)
        it "Returns False for distinct values" $ do
            intValueEq (I8 10) 11 `shouldSatisfy` (== False)
            intValueEq (I32 100) 200 `shouldSatisfy` (== False)

    describe "Derived Instances" $ do
        describe "Eq Instance" $ do
            it "Eq instance works between IntValues" $ do
                ((I8 10) == (I8 10)) `shouldSatisfy` (== True)
                ((I8 10) /= (I8 11)) `shouldSatisfy` (== True)

        describe "Show Instance (All Constructors)" $ do
            it "Formats I8" $ do
                show (I8 127) `shouldSatisfy` (== "I8 127")
            it "Formats I16" $ do
                show (I16 32000) `shouldSatisfy` (== "I16 32000")
            it "Formats I32" $ do
                show (I32 100000) `shouldSatisfy` (== "I32 100000")
            it "Formats I64" $ do
                show (I64 999999999) `shouldSatisfy` (== "I64 999999999")

        describe "Ord Instance (All Comparators & Constructors)" $ do
            it "Compares values inside same constructor" $ do
                ((I8 10) < (I8 20)) `shouldSatisfy` (== True)
                ((I8 20) > (I8 10)) `shouldSatisfy` (== True)
                ((I8 10) <= (I8 10)) `shouldSatisfy` (== True)
                ((I8 10) >= (I8 10)) `shouldSatisfy` (== True)
                compare (I8 10) (I8 20) `shouldSatisfy` (== LT)
                compare (I8 20) (I8 10) `shouldSatisfy` (== GT)
                compare (I8 10) (I8 10) `shouldSatisfy` (== EQ)
            it "Compares constructors based on definition order (I8 < I16 < I32 < I64)" $ do
                ((I8 100) < (I16 0)) `shouldSatisfy` (== True)
                ((I16 100) < (I32 0)) `shouldSatisfy` (== True)
                ((I32 100) < (I64 0)) `shouldSatisfy` (== True)
                ((I64 0) > (I32 100)) `shouldSatisfy` (== True)
                ((I32 0) > (I16 100)) `shouldSatisfy` (== True)
                ((I16 0) > (I8 100)) `shouldSatisfy` (== True)

        describe "Unsigned & Char Types Support (Coverage)" $ do
            it "toInt64 supports Unsigned & Char" $ do
                toInt64 (U8 255) `shouldSatisfy` (== 255)
                toInt64 (U16 65000) `shouldSatisfy` (== 65000)
                toInt64 (U32 4000000000) `shouldSatisfy` (== 4000000000)
                toInt64 (U64 9000000000000000000) `shouldSatisfy` (== 9000000000000000000)
                toInt64 (IChar 'A') `shouldSatisfy` (== 65)

            it "intValueToInt supports Unsigned & Char" $ do
                intValueToInt (U8 255) `shouldSatisfy` (== 255)
                intValueToInt (U16 65000) `shouldSatisfy` (== 65000)
                intValueToInt (U32 100000) `shouldSatisfy` (== 100000)
                intValueToInt (U64 100000) `shouldSatisfy` (== 100000)
                intValueToInt (IChar 'A') `shouldSatisfy` (== 65)

            it "Show instance for Unsigned & Char" $ do
                show (U8 10) `shouldSatisfy` (== "U8 10")
                show (U16 10) `shouldSatisfy` (== "U16 10")
                show (U32 10) `shouldSatisfy` (== "U32 10")
                show (U64 10) `shouldSatisfy` (== "U64 10")
                show (IChar 'c') `shouldSatisfy` (== "IChar 'c'")

            it "Ord instance for Unsigned & Char" $ do
                ((U8 10) < (U8 20)) `shouldSatisfy` (== True)
                ((IChar 'a') < (IChar 'b')) `shouldSatisfy` (== True)
                compare (U32 100) (U32 100) `shouldSatisfy` (== EQ)
