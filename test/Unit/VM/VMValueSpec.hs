{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VMValueSpec
-}

module VM.VMValueSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T

import VM.VMValue (VMValue(..), valueToString)
import Common.Type.Integer (IntValue(..))

spec :: Spec
spec = describe "VM.VMValue" $ do

  describe "valueToString - VInt" $ do
    it "prints an integer using intValueToInt (positive)" $ do
      valueToString (VInt (I8 42)) `shouldBe` (T.pack "42")

    it "prints an integer using intValueToInt (negative)" $ do
      valueToString (VInt (I8 (-1))) `shouldBe` (T.pack "-1")

  describe "valueToString - VBool" $ do
    it "prints True as True" $ do
      valueToString (VBool True) `shouldBe` (T.pack "True")

    it "prints False as False" $ do
      valueToString (VBool False) `shouldBe` (T.pack "False")

  describe "valueToString - VList" $ do
    it "prints an empty list as '()" $ do
      valueToString (VList V.empty) `shouldBe` (T.pack "[]")

    it "prints a non-empty list with elements separated by spaces" $ do
      let v = V.fromList [VInt (I8 1), VBool True, VVoid]
      valueToString (VList v) `shouldBe` (T.pack "[1, True, void]")

    it "prints nested lists recursively" $ do
      let inner = VList V.empty
      let outer = VList (V.fromList [inner])
      valueToString outer `shouldBe` (T.pack "[[]]")

  describe "valueToString - VStruct" $ do
    it "prints an empty struct as {struct:}" $ do
      valueToString (VStruct V.empty) `shouldBe` (T.pack "{}")

    it "prints a non-empty struct with fields separated by spaces" $ do
      let fields = V.fromList [VInt (I8 1), VBool False, VVoid]
      valueToString (VStruct fields) `shouldBe` (T.pack "{1, False, void}")

    it "prints nested values inside a struct (including lists)" $ do
      let lst = VList (V.fromList [VInt (I8 2), VBool True])
      let st  = VStruct (V.fromList [VInt (I8 1), lst])
      valueToString st `shouldBe` (T.pack "{1, [2, True]}")

  describe "valueToString - VClosure / VFuncPtr / VVoid" $ do
    it "prints a closure with address and capture count" $ do
      let caps = V.fromList [VInt (I8 1), VBool False]
      valueToString (VClosure 123 caps) `shouldBe` (T.pack "#<procedure @123 captures:2>")

    it "prints a closure with zero captures" $ do
      valueToString (VClosure 0 V.empty) `shouldBe` (T.pack "#<procedure @0 captures:0>")

    it "prints a function pointer with address" $ do
      valueToString (VFuncPtr 77) `shouldBe` (T.pack "#<function @77>")

    it "prints void as \"void\"" $ do
      valueToString VVoid `shouldBe` (T.pack "void")

  describe "deriving (Eq, Show)" $ do
    it "Eq: considers identical values equal" $ do
      let a = VList (V.fromList [VInt (I8 1), VBool True])
      let b = VList (V.fromList [VInt (I8 1), VBool True])
      a `shouldBe` b

    it "Eq: considers different values not equal" $ do
      let a = VClosure 10 (V.fromList [VInt (I8 1)])
      let b = VClosure 11 (V.fromList [VInt (I8 1)])
      a `shouldNotBe` b

    it "Show: produces a constructor-based representation" $ do
      show (VBool True) `shouldBe` "VBool True"
      show VVoid `shouldBe` "VVoid"
