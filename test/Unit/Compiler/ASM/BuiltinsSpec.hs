{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Compiler.ASM.Builtins unit tests
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ASM.BuiltinsSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)

import Compiler.ASM.Builtins
  ( builtinMap
  , getInnerListType
  , getBuiltinReturnType
  )
import Compiler.Instruction (Instruction(..))

isLeftWith :: Either Text a -> Text -> Bool
isLeftWith (Left msg) expected = expected `T.isInfixOf` msg
isLeftWith _ _                 = False

isRightEq :: Eq a => Either e a -> a -> Bool
isRightEq (Right x) y = x == y
isRightEq _ _         = False

spec :: Spec
spec = describe "Compiler.ASM.Builtins" $ do

  describe "builtinMap" $ do
    it "contains some well-known entries" $ do
      Map.lookup "print" builtinMap `shouldBe` Just Print
      Map.lookup "+" builtinMap `shouldBe` Just Add
      Map.lookup "cons" builtinMap `shouldBe` Just Cons
      Map.lookup "open" builtinMap `shouldBe` Just Open

    it "contains cast builtins mapping to Cast opcodes" $ do
      Map.lookup "int8" builtinMap `shouldBe` Just (Cast 0x01)
      Map.lookup "uchar" builtinMap `shouldBe` Just (Cast 0x10)

  describe "getInnerListType" $ do
    it "extracts inner type for [int]" $ do
      getInnerListType "[int]" `shouldBe` Right "int"

    it "extracts inner type for nested [[char]] -> [char]" $ do
      getInnerListType "[[char]]" `shouldBe` Right "[char]"

    it "fails when not bracketed" $ do
      getInnerListType "int"
        `shouldSatisfy` (`isLeftWith` "Expected a list type")

    it "fails when too short / malformed" $ do
      getInnerListType "[]"
        `shouldSatisfy` (`isLeftWith` "Expected a list type")

      getInnerListType "["
        `shouldSatisfy` (`isLeftWith` "Expected a list type")

      getInnerListType "]"
        `shouldSatisfy` (`isLeftWith` "Expected a list type")

  describe "getBuiltinReturnType" $ do

    describe "list builtins (nth/head/tail/cons)" $ do
      it "nth returns inner element type of a list" $ do
        getBuiltinReturnType "nth" ["[int]", "int"] `shouldBe` Right "int"
        getBuiltinReturnType "nth" ["[[char]]", "int"] `shouldBe` Right "[char]"

      it "nth propagates getInnerListType error if not a list type" $ do
        getBuiltinReturnType "nth" ["int", "int"]
          `shouldSatisfy` (`isLeftWith` "Expected a list type")

      it "head returns inner element type" $ do
        getBuiltinReturnType "head" ["[bool]"] `shouldBe` Right "bool"

      it "head propagates error if arg is not a list type" $ do
        getBuiltinReturnType "head" ["bool"]
          `shouldSatisfy` (`isLeftWith` "Expected a list type")

      it "tail returns the list type unchanged" $ do
        getBuiltinReturnType "tail" ["[int]"] `shouldBe` Right "[int]"

      it "cons returns the list type unchanged (second arg is list type)" $ do
        getBuiltinReturnType "cons" ["int", "[int]"] `shouldBe` Right "[int]"

    describe "arith builtins -> int" $ do
      it "covers + - * div mod and also / % (even if not in builtinMap)" $ do
        getBuiltinReturnType "+" [] `shouldBe` Right "int"
        getBuiltinReturnType "-" ["int","int"] `shouldBe` Right "int"
        getBuiltinReturnType "*" ["int","int"] `shouldBe` Right "int"
        getBuiltinReturnType "div" ["int","int"] `shouldBe` Right "int"
        getBuiltinReturnType "mod" ["int","int"] `shouldBe` Right "int"
        getBuiltinReturnType "/" ["int","int"] `shouldBe` Right "int"
        getBuiltinReturnType "%" ["int","int"] `shouldBe` Right "int"

    describe "logic/comparison builtins -> bool" $ do
      it "covers eq?/neq?/teq?/tneq? and comparisons and boolean ops" $ do
        getBuiltinReturnType "eq?" ["int","int"] `shouldBe` Right "bool"
        getBuiltinReturnType "neq?" ["int","int"] `shouldBe` Right "bool"
        getBuiltinReturnType "teq?" ["int","int"] `shouldBe` Right "bool"
        getBuiltinReturnType "tneq?" ["int","int"] `shouldBe` Right "bool"

        getBuiltinReturnType "<" ["int","int"] `shouldBe` Right "bool"
        getBuiltinReturnType ">" ["int","int"] `shouldBe` Right "bool"
        getBuiltinReturnType "<=" ["int","int"] `shouldBe` Right "bool"
        getBuiltinReturnType ">=" ["int","int"] `shouldBe` Right "bool"

        getBuiltinReturnType "!" ["bool"] `shouldBe` Right "bool"
        getBuiltinReturnType "&&" ["bool","bool"] `shouldBe` Right "bool"
        getBuiltinReturnType "||" ["bool","bool"] `shouldBe` Right "bool"

    describe "io/system builtins" $ do
      it "open/close/write return int; read/typeof return [char]" $ do
        getBuiltinReturnType "open" ["[char]","int"] `shouldBe` Right "int"
        getBuiltinReturnType "close" ["int"] `shouldBe` Right "int"
        getBuiltinReturnType "write" ["int","[char]"] `shouldBe` Right "int"
        getBuiltinReturnType "read" ["int","int"] `shouldBe` Right "[char]"
        getBuiltinReturnType "typeof" ["int"] `shouldBe` Right "[char]"

      it "ffread returns [[char]] and ffwrite returns bool" $ do
        getBuiltinReturnType "ffread" ["[char]"] `shouldBe` Right "[[char]]"
        getBuiltinReturnType "ffwrite" ["[char]","[[char]]"] `shouldBe` Right "bool"

    describe "unknown builtin" $ do
      it "returns Left with an error message" $ do
        getBuiltinReturnType "does_not_exist" ["int"]
          `shouldSatisfy` (`isLeftWith` "Unknown builtin or invalid arguments")
