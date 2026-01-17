{-# LANGUAGE OverloadedStrings #-}

module Common.ErrorSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, chunk, Parsec, label, failure, ErrorItem(..), (<|>))
import Data.Void (Void)
import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Common.Error (formatError, formatParseError)

type Parser = Parsec Void Text

spec :: Spec
spec = do
    describe "Common.Error" $ do
        
        describe "formatError" $ do
            it "formats error message correctly" $ do
                let tag = "Test Error"
                let msg = "Something went wrong"
                let res = formatError tag msg
                res `shouldSatisfy` (== "\x1b[31m[Test Error]\x1b[0m Something went wrong")

        describe "formatParseError" $ do

            it "formats a simple mismatch error correctly" $ do
                let parser = chunk "hello" :: Parser Text
                let result = parse parser "test_file" "world"
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "\x1b[31m[Parse Error]\x1b[0m" `isInfixOf` s)
                        output `shouldSatisfy` (\s -> "expected \"hello\"" `isInfixOf` s)
                        output `shouldSatisfy` (\s -> "but got \"world\"" `isInfixOf` s)

            it "handles end of input error" $ do
                let parser = chunk "x" :: Parser Text
                let result = parse parser "test_file" ""
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "end of input" `isInfixOf` s)

            it "handles custom error (FancyError)" $ do
                let parser = fail "Boom" :: Parser Text
                let result = parse parser "test_file" ""
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "Boom" `isInfixOf` s)
                        output `shouldSatisfy` (\s -> "\x1b[31m[Parse Error]\x1b[0m" `isInfixOf` s)
    
        describe "formatError" $ do
            it "formats error message correctly" $ do
                let tag = "Test Error"
                let msg = "Something went wrong"
                let res = formatError tag msg
                res `shouldSatisfy` (== "\x1b[31m[Test Error]\x1b[0m Something went wrong")

        describe "formatParseError" $ do
            it "formats a simple mismatch error correctly" $ do
                let parser = chunk "hello" :: Parser Text
                let result = parse parser "test_file" "world"
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "\x1b[31m[Parse Error]\x1b[0m" `isInfixOf` s)
                        output `shouldSatisfy` (\s -> "expected \"hello\"" `isInfixOf` s)
                        output `shouldSatisfy` (\s -> "but got \"world\"" `isInfixOf` s)

            it "handles end of input error" $ do
                let parser = chunk "x" :: Parser Text
                let result = parse parser "test_file" ""
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "end of input" `isInfixOf` s)

            it "handles unknown expected items" $ do
                let parser = failure (Just (Tokens ('x' NE.:| []))) Set.empty :: Parser Text
                let result = parse parser "test_file" "input"
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "expected unknown" `isInfixOf` s)

            it "handles multiple labels in expected items" $ do
                let parser = label "A" (chunk "x") <|> label "B" (chunk "y") :: Parser Text
                let result = parse parser "test_file" "z"
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "expected A or B" `isInfixOf` s)

            it "handles multiple tokens in expected items" $ do
                let parser = chunk "foo" <|> chunk "bar" <|> chunk "baz" :: Parser Text
                let result = parse parser "test_file" "xxx"
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "\"bar\", \"baz\", \"foo\"" `isInfixOf` s)

            it "handles unexpected label item" $ do
                let parser = failure (Just (Label ('M' NE.:| "yLabel"))) Set.empty :: Parser Text
                let result = parse parser "test_file" ""
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "but got MyLabel" `isInfixOf` s)

            it "handles end of input error (getUnexpected Nothing)" $ do
                let parser = chunk "x" :: Parser Text
                let result = parse parser "test_file" ""
                case result of
                    Right _ -> expectationFailure "Parser should have failed"
                    Left bundle -> do
                        let output = formatParseError bundle
                        output `shouldSatisfy` (\s -> "but got end of input" `isInfixOf` s)