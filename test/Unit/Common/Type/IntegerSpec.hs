{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- IntegerSpec.hs
-}

module Common.Type.IntegerSpec (spec) where

import Test.Hspec
import Common.Type.Integer (IntValue(..), fitInteger, toInt64, fromInt64, intValueToInt)
import Data.Char (ord)
import Data.Int (Int8, Int64)
import Data.Word (Word8)
import Data.List (sort)

cInt8 :: Char -> Int8
cInt8 c = fromIntegral (ord c)

spec :: Spec
spec = describe "Common.Type.Integer (full branch coverage)" $ do

  describe "Derived instances (Show / Eq / Ord) - coverage for deriving" $ do
    it "Show is usable (smoke)" $ do
      show (I8 1) `shouldBe` "I8 1"
      show (UI8 1) `shouldBe` "UI8 1"
      show (IChar 65) `shouldBe` "IChar 65"
      show (UIChar 65) `shouldBe` "UIChar 65"

    it "Eq works" $ do
      I16 10 `shouldBe` I16 10
      I16 10 `shouldNotBe` I16 11

    it "Ord works (values + compare + sort)" $ do
      (I8 1 < I8 2) `shouldBe` True
      compare (I8 1) (I16 1) `shouldBe` LT
      sort [I8 2, I8 1, I8 3] `shouldBe` [I8 1, I8 2, I8 3]

  describe "intValueToInt (covers ALL constructors)" $ do
    it "covers signed + unsigned + char variants" $ do
      intValueToInt (I8 (-1)) `shouldBe` (-1)
      intValueToInt (UI8 (255 :: Word8)) `shouldBe` 255
      intValueToInt (I16 1000) `shouldBe` 1000
      intValueToInt (UI16 65000) `shouldBe` 65000
      intValueToInt (I32 100000) `shouldBe` 100000
      intValueToInt (UI32 42) `shouldBe` 42
      intValueToInt (I64 2000000000) `shouldBe` 2000000000
      intValueToInt (UI64 100000) `shouldBe` 100000
      intValueToInt (IChar (cInt8 'A')) `shouldBe` 65
      intValueToInt (UIChar (fromIntegral (ord 'A'))) `shouldBe` 65

  describe "toInt64 (covers ALL constructors)" $ do
    it "covers signed + unsigned + char variants" $ do
      toInt64 (I8 (-10)) `shouldBe` (-10)
      toInt64 (UI8 200) `shouldBe` 200
      toInt64 (I16 1000) `shouldBe` 1000
      toInt64 (UI16 65000) `shouldBe` 65000
      toInt64 (I32 70000) `shouldBe` 70000
      toInt64 (UI32 3000000000) `shouldBe` 3000000000
      toInt64 (I64 5000000000) `shouldBe` 5000000000
      toInt64 (UI64 42) `shouldBe` 42
      toInt64 (IChar (cInt8 'c')) `shouldBe` 99
      toInt64 (UIChar 99) `shouldBe` 99

  describe "fromInt64 (forces every guard branch)" $ do
    it "I8 branch" $ do
      fromInt64 127 `shouldBe` I8 127
      fromInt64 (-128) `shouldBe` I8 (-128)

    it "UI8 branch" $ do
      fromInt64 200 `shouldBe` UI8 200

    it "I16 branch" $ do
      fromInt64 256 `shouldBe` I16 256
      fromInt64 (-129) `shouldBe` I16 (-129)

    it "UI16 branch" $ do
      fromInt64 40000 `shouldBe` UI16 40000

    it "I32 branch" $ do
      fromInt64 70000 `shouldBe` I32 70000

    it "UI32 branch" $ do
      fromInt64 3000000000 `shouldBe` UI32 3000000000

    it "I64 branch (otherwise)" $ do
      fromInt64 5000000000 `shouldBe` I64 5000000000
      fromInt64 (-(2147483648 :: Int64) - 1) `shouldBe` I64 (-(2147483648 :: Int64) - 1)

  describe "fitInteger (forces every guard branch, including I16)" $ do
    it "I8 + UI8" $ do
      fitInteger 42 `shouldBe` I8 42
      fitInteger 200 `shouldBe` UI8 200

    it "I16 branch" $ do
      fitInteger 256 `shouldBe` I16 256
      fitInteger (-129) `shouldBe` I16 (-129)

    it "UI16 / I32 / UI32" $ do
      fitInteger 40000 `shouldBe` UI16 40000
      fitInteger 70000 `shouldBe` I32 70000
      fitInteger 3000000000 `shouldBe` UI32 3000000000

    it "I64 otherwise" $ do
      fitInteger 50000000000 `shouldBe` I64 50000000000
