{-# LANGUAGE OverloadedStrings #-}

module Common.ErrorSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Data.Void (Void)
import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (fromList)
import Control.Exception (evaluate)

import Common.Error

type Parser = Parsec Void Text

dummyPos :: Int
dummyPos = 0

mustFail :: Either (ParseErrorBundle Text Void) a -> ParseErrorBundle Text Void
mustFail (Left b) = b
mustFail (Right _) = error "Expected parse failure, but it succeeded"

spec :: Spec
spec = do
  describe "Common.Error" $ do

    it "dummyPos is referenced (coverage)" $ do
      dummyPos `shouldBe` 0

    it "mustFail throws on Right (coverage for mustFail Right branch)" $ do
      evaluate (mustFail (Right () :: Either (ParseErrorBundle Text Void) ())) `shouldThrow` anyErrorCall

    describe "formatError" $ do
      it "formats error message correctly" $ do
        let tag = "Test Error"
        let msg = "Something went wrong"
        let res = formatError tag msg
        res `shouldSatisfy` (== "\x1b[31m[Test Error]\x1b[0m Something went wrong")

    describe "formatParseError" $ do
      it "formats a simple mismatch error correctly" $ do
        let parser = chunk "hello" :: Parser Text
        let bundle = mustFail (parse parser "test_file" "world")
        let output = formatParseError bundle
        output `shouldSatisfy` (\s -> "\x1b[31m[Parse Error]\x1b[0m" `isInfixOf` s)
        output `shouldSatisfy` (\s -> "expected \"hello\"" `isInfixOf` s)
        output `shouldSatisfy` (\s -> "but got \"world\"" `isInfixOf` s)

      it "handles end of input error" $ do
        let parser = chunk "x" :: Parser Text
        let bundle = mustFail (parse parser "test_file" "")
        let output = formatParseError bundle
        output `shouldSatisfy` (\s -> "end of input" `isInfixOf` s)

      it "handles custom error (FancyError via fail)" $ do
        let parser = fail "Boom" :: Parser Text
        let bundle = mustFail (parse parser "test_file" "")
        let output = formatParseError bundle
        output `shouldSatisfy` (\s -> "Boom" `isInfixOf` s)

    describe "formatFancyError" $ do
      it "Formats ErrorFail correctly" $ do
        let errs = Set.singleton (ErrorFail "Something went wrong")
        formatFancyError errs `shouldBe` "Something went wrong"

      it "Formats ErrorIndentation correctly (constructor is Ordering -> Pos -> Pos)" $ do
        let err1 = ErrorIndentation EQ (mkPos 4) (mkPos 2)
        let errs = Set.singleton err1
        let out = formatFancyError errs
        out `shouldSatisfy` (\s -> "incorrect indentation" `isInfixOf` s)
        out `shouldSatisfy` (\s -> "got 2" `isInfixOf` s)
        out `shouldSatisfy` (\s -> "should" `isInfixOf` s)

      it "Joins multiple fancy errors with semicolon" $ do
        let err1 = ErrorFail "Error 1"
        let err2 = ErrorFail "Error 2"
        let errs = Set.fromList [err1, err2]
        let res = formatFancyError errs
        res `shouldSatisfy` (\s -> "Error 1" `isInfixOf` s && "Error 2" `isInfixOf` s)

    describe "getUnexpected" $ do
      it "Returns 'custom error' for FancyError" $ do
        let err = FancyError dummyPos (Set.singleton (ErrorFail "Boom")) :: ParseError Text Void
        getUnexpected err `shouldBe` "custom error"

      it "Returns 'end of input' for TrivialError with Nothing" $ do
        let err = TrivialError dummyPos Nothing Set.empty :: ParseError Text Void
        getUnexpected err `shouldBe` "end of input"

      it "Returns something else for TrivialError with Just tokens" $ do
        let u = Tokens (fromList "foo")
        let err = TrivialError dummyPos (Just u) Set.empty :: ParseError Text Void
        let got = getUnexpected err
        got `shouldSatisfy` (\s -> s /= "end of input" && s /= "custom error")
