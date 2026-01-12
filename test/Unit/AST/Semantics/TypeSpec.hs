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

    describe "Show Instances (Yellow Removal)" $ do
        it "Prints all Type constructors" $ do
            show TyInt `shouldBe` "TyInt"
            show TyBool `shouldBe` "TyBool"
            show TyVoid `shouldBe` "TyVoid"
            show TyAuto `shouldBe` "TyAuto"
            show (TyList TyInt) `shouldSatisfy` ("TyList" `isInfixOf`)
            show (TyStruct (p "S")) `shouldSatisfy` ("TyStruct" `isInfixOf`)
            show (TyFunc [TyInt] TyVoid) `shouldSatisfy` ("TyFunc" `isInfixOf`)

        it "Prints Data Structures" $ do
            let sDef = StructDef (p "Pt") (Map.singleton (p "x") TyInt)
            show sDef `shouldSatisfy` ("StructDef" `isInfixOf`)
            
            let env = CheckEnv (Map.singleton (p "v") TyInt) (Map.singleton (p "S") sDef)
            show env `shouldSatisfy` ("CheckEnv" `isInfixOf`)
            
            show emptyEnv `shouldSatisfy` ("fromList []" `isInfixOf`)

    describe "Accessors" $ do
        it "Accesses StructDef fields" $ do
            let sDef = StructDef (p "Pt") Map.empty
            structName sDef `shouldBe` p "Pt"
            structFields sDef `shouldSatisfy` Map.null

        it "Accesses CheckEnv fields" $ do
            let env = emptyEnv
            envVars env `shouldSatisfy` Map.null
            envStructs env `shouldSatisfy` Map.null

    describe "parseType" $ do
        it "Handles primitives" $ do
            parseType (p "int") `shouldSatisfy` \case TyInt -> True; _ -> False
            parseType (p "bool") `shouldSatisfy` \case TyBool -> True; _ -> False
            parseType (p "void") `shouldSatisfy` \case TyVoid -> True; _ -> False
            parseType (p "auto") `shouldSatisfy` \case TyAuto -> True; _ -> False

        it "Handles lists" $ do
            parseType (p "[int]") `shouldSatisfy` \case TyList TyInt -> True; _ -> False
            parseType (p "[[bool]]") `shouldSatisfy` \case TyList (TyList TyBool) -> True; _ -> False
        
        it "Handles structs (Default case)" $ do
            parseType (p "UserType") `shouldSatisfy` \case 
                TyStruct n -> n == p "UserType"
                _ -> False

    describe "typeToString" $ do
        it "Formats all types" $ do
            typeToString TyInt `shouldSatisfy` (== "int")
            typeToString TyBool `shouldSatisfy` (== "bool")
            typeToString TyVoid `shouldSatisfy` (== "void")
            typeToString TyAuto `shouldSatisfy` (== "auto")
            typeToString (TyList TyInt) `shouldSatisfy` (== "[int]")
            typeToString (TyStruct (p "S")) `shouldSatisfy` (== "S")
            typeToString (TyFunc [TyInt, TyBool] TyVoid) `shouldSatisfy` (== "(int bool -> void)")
            typeToString (TyFunc [] TyVoid) `shouldSatisfy` (== "( -> void)")
