{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Unit tests for Semantic Types
-}

{-# LANGUAGE LambdaCase #-}

module AST.Semantics.TypeSpec (spec) where

import Test.Hspec
import AST.Semantics.Type
import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Semantic Type System" $ do

    describe "Data Structures Coverage" $ do
        
        it "Type: Show instance covers all constructors" $ do
            show TyInt `shouldBe` "TyInt"
            show TyBool `shouldBe` "TyBool"
            show TyVoid `shouldBe` "TyVoid"
            show TyAuto `shouldBe` "TyAuto"
            show (TyList TyInt) `shouldSatisfy` ("TyList" `isInfixOf`)
            show (TyFunc [TyInt] TyVoid) `shouldSatisfy` ("TyFunc" `isInfixOf`)
            show (TyStruct (p "S")) `shouldSatisfy` ("TyStruct" `isInfixOf`)

        it "StructDef: Accessors and Show" $ do
            let sd = StructDef (p "Point") Map.empty
            show sd `shouldSatisfy` ("StructDef" `isInfixOf`)
            structName sd `shouldBe` p "Point"
            structFields sd `shouldSatisfy` Map.null

        it "CheckEnv: Accessors, Show and emptyEnv" $ do
            let env = emptyEnv
            show env `shouldSatisfy` ("CheckEnv" `isInfixOf`)
            envVars env `shouldSatisfy` Map.null
            envStructs env `shouldSatisfy` Map.null

    describe "parseType" $ do
        it "Parses primitive types" $ do
            parseType (p "int")  `shouldSatisfy` \case TyInt -> True; _ -> False
            parseType (p "bool") `shouldSatisfy` \case TyBool -> True; _ -> False
            parseType (p "void") `shouldSatisfy` \case TyVoid -> True; _ -> False
            parseType (p "auto") `shouldSatisfy` \case TyAuto -> True; _ -> False

        it "Parses list types" $ do
            parseType (p "[int]")  `shouldSatisfy` \case TyList TyInt -> True; _ -> False
            
        it "Parses nested list types" $ do
            parseType (p "[[int]]") `shouldSatisfy` \case 
                TyList (TyList TyInt) -> True
                _ -> False

        it "Parses custom structure types (fallback)" $ do
            parseType (p "Point")  `shouldSatisfy` \case TyStruct n -> n == p "Point"; _ -> False

    describe "typeToString" $ do
        it "Formats primitives" $ do
            typeToString TyInt `shouldSatisfy` (== "int")
            typeToString TyBool `shouldSatisfy` (== "bool")
            typeToString TyVoid `shouldSatisfy` (== "void")
            typeToString TyAuto `shouldSatisfy` (== "auto")
        
        it "Formats lists" $ do
            typeToString (TyList TyInt) `shouldSatisfy` (== "[int]")

        it "Formats structs" $ do
            typeToString (TyStruct (p "Point")) `shouldSatisfy` (== "Point")

        it "Formats functions" $ do
            let tf = TyFunc [TyInt, TyBool] TyVoid
            typeToString tf `shouldSatisfy` (== "(int bool -> void)")
