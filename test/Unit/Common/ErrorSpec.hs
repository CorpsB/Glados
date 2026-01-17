{-# LANGUAGE OverloadedStrings #-}

module Common.ErrorSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Data.Void (Void)
import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Common.Error
import Data.List.NonEmpty (fromList)

type Parser = Parsec Void Text

dummyPos :: Int
dummyPos = 0

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

        describe "formatFancyError" $ do
            it "Formats ErrorFail correctly" $ do
                let errs = Set.singleton (ErrorFail "Something went wrong")
                formatFancyError errs `shouldBe` "Something went wrong"

            it "Formats ErrorIndentation correctly" $ do
                let err = ErrorIndentation EQ (mkPos 4) (mkPos 2)
                let errs = Set.singleton err
                formatFancyError errs `shouldBe` "incorrect indentation (got 2, should be 4)"

            it "Joins multiple fancy errors with semicolon" $ do
                let err1 = ErrorFail "Error 1"
                let err2 = ErrorFail "Error 2"
                let errs = Set.fromList [err1, err2]
                let res = formatFancyError errs
                res `shouldSatisfy` (\s -> "Error 1" `isInfixOf` s && "Error 2" `isInfixOf` s)

    describe "getExpected" $ do
        it "Returns 'custom error' for FancyError" $ do
            let err = FancyError dummyPos (Set.singleton (ErrorFail "Boom")) :: ParseError Text Void
            getExpected err `shouldBe` "custom error"

    describe "getUnexpected" $ do
        it "Returns 'end of input' for TrivialError with Nothing" $ do
            let err = TrivialError dummyPos Nothing Set.empty :: ParseError Text Void
            getUnexpected err `shouldBe` "end of input"

        it "Returns 'custom error' for FancyError" $ do
            let err = FancyError dummyPos (Set.singleton (ErrorFail "Boom")) :: ParseError Text Void
            getUnexpected err `shouldBe` "custom error"

        it "Returns the unexpected item for TrivialError with Just" $ do
            let unex = Label (fromList "foo")
            let err = TrivialError dummyPos (Just unex) Set.empty :: ParseError Text Void
            getUnexpected err `shouldNotBe` "end of input"
            getUnexpected err `shouldNotBe` "custom error"
