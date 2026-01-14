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
import Data.Int (Int8)
import Data.Word (Word8)
import Data.List (sort, isInfixOf)

cInt8 :: Char -> Int8
cInt8 c = fromIntegral (ord c)

spec :: Spec
spec = describe "Common.Type.Integer (full branch coverage)" $ do

  describe "Derived instances (Show / Eq / Ord)" $ do

    it "Show works deeply (forces showsPrec with precedence)" $ do
        show (I8 1) `shouldBe` "I8 1"

        show (Just (I8 42)) `shouldSatisfy` ("(I8 42)" `isInfixOf`)
        show [UI16 10, UI16 20] `shouldSatisfy` ("UI16 10" `isInfixOf`)

    it "Eq works (Structural Equality)" $ do
      I16 10 `shouldBe` I16 10
      I16 10 `shouldNotBe` I16 11
      I8 1 `shouldNotBe` UI8 1
      IChar 65 `shouldNotBe` UIChar 65

    it "Ord works (Constructor Order + Value Order)" $ do
      (I8 1 < I8 2) `shouldBe` True
      compare (I8 100) (UI8 1) `shouldBe` LT 
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

  describe "fromInt64 (Boundary Checks)" $ do
    it "I8 branch (-128 to 127)" $ do
      fromInt64 127 `shouldBe` I8 127
      fromInt64 (-128) `shouldBe` I8 (-128)
      fromInt64 0 `shouldBe` I8 0

    it "UI8 branch (128 to 255)" $ do
      fromInt64 128 `shouldBe` UI8 128
      fromInt64 255 `shouldBe` UI8 255

    it "I16 branch (Outside 8-bit, up to 32767)" $ do
      fromInt64 256 `shouldBe` I16 256
      fromInt64 (-129) `shouldBe` I16 (-129)
      fromInt64 32767 `shouldBe` I16 32767

    it "UI16 branch (32768 to 65535)" $ do
      fromInt64 32768 `shouldBe` UI16 32768
      fromInt64 65535 `shouldBe` UI16 65535

    it "I32 branch (Outside 16-bit)" $ do
      fromInt64 65536 `shouldBe` I32 65536
      fromInt64 (-32769) `shouldBe` I32 (-32769)

    it "UI32 branch (High positive)" $ do
      fromInt64 2147483648 `shouldBe` UI32 2147483648

    it "I64 branch (Everything else)" $ do
      fromInt64 4294967296 `shouldBe` I64 4294967296
      fromInt64 (-2147483649) `shouldBe` I64 (-2147483649)

  describe "fitInteger (Boundary Checks)" $ do
    it "I8 branch" $ do
      fitInteger 42 `shouldBe` I8 42
      fitInteger (-128) `shouldBe` I8 (-128)

    it "UI8 branch" $ do
      fitInteger 128 `shouldBe` UI8 128

    it "I16 branch" $ do
      fitInteger 256 `shouldBe` I16 256
      fitInteger (-1000) `shouldBe` I16 (-1000)

    it "UI16 branch" $ do
      fitInteger 40000 `shouldBe` UI16 40000

    it "I32 branch" $ do
      fitInteger 70000 `shouldBe` I32 70000

    it "UI32 branch" $ do
      fitInteger 3000000000 `shouldBe` UI32 3000000000

    it "I64 otherwise" $ do
      fitInteger 50000000000 `shouldBe` I64 50000000000
