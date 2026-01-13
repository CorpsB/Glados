{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VMValueSpec
-}

module VM.VMValueSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V

import VM.VMValue (VMValue(..), valueToString, valueToInt, castValue)
import Common.Type.Integer (IntValue(..))

spec :: Spec
spec = describe "VM.VMValue" $ do

  describe "valueToString - VInt" $ do
    it "prints an integer using intValueToInt (positive)" $ do
      valueToString (VInt (I8 42)) `shouldBe` "42"

    it "prints an integer using intValueToInt (negative)" $ do
      valueToString (VInt (I8 (-1))) `shouldBe` "-1"

  describe "valueToString - VBool" $ do
    it "prints True as #t" $ do
      valueToString (VBool True) `shouldBe` "#t"

    it "prints False as #f" $ do
      valueToString (VBool False) `shouldBe` "#f"

  describe "valueToString - VList" $ do
    it "prints an empty list as '()" $ do
      valueToString (VList V.empty) `shouldBe` "'()"

    it "prints a non-empty list with elements separated by spaces" $ do
      let v = V.fromList [VInt (I8 1), VBool True, VVoid]
      valueToString (VList v) `shouldBe` "'(1 #t void)"

    it "prints nested lists recursively" $ do
      let inner = VList V.empty
      let outer = VList (V.fromList [inner])
      valueToString outer `shouldBe` "'('())"

  describe "valueToString - VStruct" $ do
    it "prints an empty struct as {struct:}" $ do
      valueToString (VStruct V.empty) `shouldBe` "{struct:}"

    it "prints a non-empty struct with fields separated by spaces" $ do
      let fields = V.fromList [VInt (I8 1), VBool False, VVoid]
      valueToString (VStruct fields) `shouldBe` "{struct:1 #f void}"

    it "prints nested values inside a struct (including lists)" $ do
      let lst = VList (V.fromList [VInt (I8 2), VBool True])
      let st  = VStruct (V.fromList [VInt (I8 1), lst])
      valueToString st `shouldBe` "{struct:1 '(2 #t)}"

  describe "valueToString - VClosure / VFuncPtr / VVoid" $ do
    it "prints a closure with address and capture count" $ do
      let caps = V.fromList [VInt (I8 1), VBool False]
      valueToString (VClosure 123 caps) `shouldBe` "#<procedure @123 captures:2>"

    it "prints a closure with zero captures" $ do
      valueToString (VClosure 0 V.empty) `shouldBe` "#<procedure @0 captures:0>"

    it "prints a function pointer with address" $ do
      valueToString (VFuncPtr 77) `shouldBe` "#<function @77>"

    it "prints void as \"void\"" $ do
      valueToString VVoid `shouldBe` "void"

  describe "valueToInt" $ do
    it "VInt returns the integer" $ do
      valueToInt (VInt (I64 42)) `shouldBe` 42

    it "VBool maps to 1/0" $ do
      valueToInt (VBool True) `shouldBe` 1
      valueToInt (VBool False) `shouldBe` 0

    it "VFuncPtr returns its address" $ do
      valueToInt (VFuncPtr 999) `shouldBe` 999

    it "fallback returns 0" $ do
      valueToInt (VList V.empty) `shouldBe` 0

  describe "castValue (covers all branches)" $ do
    it "casts to Bool (0x00) using valueToInt != 0" $ do
      castValue 0x00 (VInt (I64 0)) `shouldBe` VBool False
      castValue 0x00 (VInt (I64 2)) `shouldBe` VBool True

    it "casts signed/unsigned/char variants" $ do
      castValue 0x01 (VInt (I64 42)) `shouldBe` VInt (I8 42)
      castValue 0x02 (VInt (I64 42)) `shouldBe` VInt (UI8 42)
      castValue 0x03 (VInt (I64 42)) `shouldBe` VInt (I16 42)
      castValue 0x04 (VInt (I64 42)) `shouldBe` VInt (UI16 42)
      castValue 0x05 (VInt (I64 42)) `shouldBe` VInt (I32 42)
      castValue 0x06 (VInt (I64 42)) `shouldBe` VInt (UI32 42)
      castValue 0x07 (VInt (I64 42)) `shouldBe` VInt (I64 42)
      castValue 0x08 (VInt (I64 42)) `shouldBe` VInt (UI64 42)
      castValue 0x09 (VInt (I64 65)) `shouldBe` VInt (IChar 65)
      castValue 0x10 (VInt (I64 65)) `shouldBe` VInt (UIChar 65)

    it "unknown typeId returns the value unchanged (fallback)" $ do
      castValue 0x99 (VBool True) `shouldBe` VBool True

  describe "deriving (Eq, Show)" $ do
    it "Eq works" $ do
      let a = VList (V.fromList [VInt (I8 1), VBool True])
      let b = VList (V.fromList [VInt (I8 1), VBool True])
      a `shouldBe` b

    it "Show produces a constructor-based representation" $ do
      show (VBool True) `shouldBe` "VBool True"
      show VVoid `shouldBe` "VVoid"
