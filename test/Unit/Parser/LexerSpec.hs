{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- LexerSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.LexerSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as DT
import Parser.Lexer (pIdentifier, sc)
import Text.Megaparsec.Char (string)
import Data.List (isInfixOf)

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Parser.Lexer - Low level coverage" $ do

    describe "pIdentifier" $ do
        
        it "Parses a valid identifier" $ do
            parse pIdentifier "" (p "myVar") `shouldSatisfy` \case
                Right val -> val == p "myVar"
                _ -> False

        it "Parses an identifier with underscores" $ do
            parse pIdentifier "" (p "_hidden_var") `shouldSatisfy` \case
                Right val -> val == p "_hidden_var"
                _ -> False

        it "Fails when using a reserved keyword 'if' as identifier" $ do
            parse pIdentifier "" (p "if") `shouldSatisfy` \case
                Left err -> "Keyword 'if' cannot be an identifier" `isInfixOf` errorBundlePretty err
                _ -> False

        it "Fails when using a reserved keyword 'int' as identifier" $ do
            parse pIdentifier "" (p "int") `shouldSatisfy` \case
                Left err -> "Keyword 'int' cannot be an identifier" `isInfixOf` errorBundlePretty err
                _ -> False

        it "Fails when using a reserved keyword 'func' as identifier" $ do
            parse pIdentifier "" (p "func") `shouldSatisfy` \case
                Left err -> "Keyword 'func' cannot be an identifier" `isInfixOf` errorBundlePretty err
                _ -> False

    describe "Space Consumer (sc) - Comments" $ do
        it "Skips block comments /* ... */ correctly" $ do
            let parser = sc >> string "AB"
            let code = "/* Ceci est un \n commentaire bloc */ AB"
            
            parse parser "" (p code) `shouldSatisfy` \case
                Right val -> val == "AB"
                _ -> False

        it "Skips nested line comments // correctly" $ do
            let parser = sc >> string "CD"
            let code = "// Commentaire ligne\nCD"
            
            parse parser "" (p code) `shouldSatisfy` \case
                Right val -> val == "CD"
                _ -> False

        it "Handles block comments inside code" $ do
            let parser = sc >> string "End"
            let code = "/* debut */ /* fin */ End"
            parse parser "" (p code) `shouldSatisfy` \case
                Right val -> val == "End"
                _ -> False
