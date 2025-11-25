module Parser.ParserISLTest (spec) where

import Test.Hspec
import Parser.ParserISL (parseLisp)
import Lisp (SExpr(..))
import Text.Megaparsec (errorBundlePretty)

shouldFail :: String -> Expectation
shouldFail input = case parseLisp input of
    Left _ -> return ()
    Right res -> expectationFailure $ "Should have failed but succeeded with: " ++ show res

spec :: Spec
spec = describe "Parser LISP - SExpr Conversion" $ do
    
    describe "Integers" $ do
      it "parses positive integer" $ do
        case parseLisp "42" of
            Right (SInteger 42) -> return ()
            Right res -> expectationFailure $ "Expected (SInteger 42) but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed: " ++ errorBundlePretty err
      
      it "parses negative integer" $ do
        case parseLisp "-42" of
            Right (SInteger (-42)) -> return ()
            Right res -> expectationFailure $ "Expected (SInteger -42) but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed: " ++ errorBundlePretty err

      it "parses large integer" $ do
        case parseLisp "123456789" of
            Right (SInteger 123456789) -> return ()
            Right res -> expectationFailure $ "Expected large integer but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed: " ++ errorBundlePretty err
    
    describe "Symbols" $ do
      it "parses simple symbols" $ do
        case parseLisp "foo" of
            Right (SSymbol "foo") -> return ()
            Right res -> expectationFailure $ "Expected (SSymbol \"foo\") but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err
            
      it "parses 'define' keyword" $ do
        case parseLisp "define" of
            Right (SSymbol "define") -> return ()
            Right res -> expectationFailure $ "Expected (SSymbol \"define\") but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err

      it "parses operator symbols" $ do
        case parseLisp ">=" of
            Right (SSymbol ">=") -> return ()
            Right res -> expectationFailure $ "Expected (SSymbol \">=\") but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err

      it "parses complex symbol containing numbers" $ do
        case parseLisp "func_42_?" of
            Right (SSymbol "func_42_?") -> return ()
            Right res -> expectationFailure $ "Expected complex symbol but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err
      
      it "parses single character operator symbol" $ do
        case parseLisp "*" of
            Right (SSymbol "*") -> return ()
            Right res -> expectationFailure $ "Expected (SSymbol \"*\") but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err

      it "parses symbol starting with underscore or special character" $ do
        case parseLisp "_variable" of
            Right (SSymbol "_variable") -> return ()
            Right res -> expectationFailure $ "Expected (SSymbol \"_variable\") but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err

    describe "Lists" $ do
      it "parses empty list" $ do
        case parseLisp "()" of
            Right (List []) -> return ()
            Right res -> expectationFailure $ "Expected empty list but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed: " ++ errorBundlePretty err

      it "parses simple list (+ 1 2)" $ do
        case parseLisp "(+ 1 2)" of
            Right (List [SSymbol "+", SInteger 1, SInteger 2]) -> return ()
            Right res -> expectationFailure $ "Expected list (+ 1 2) but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err

      it "parses nested list (define x (* 2 3))" $ do
        case parseLisp "(define x (* 2 3))" of
            Right (List [SSymbol "define", SSymbol "x", List [SSymbol "*", SInteger 2, SInteger 3]]) -> return ()
            Right res -> expectationFailure $ "Expected nested list but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err

      it "parses deeply nested list" $ do
        case parseLisp "(((1 (2) 3)))" of
            Right (List [List [List [SInteger 1, List [SInteger 2], SInteger 3]]]) -> return ()
            Right res -> expectationFailure $ "Expected deeply nested list but got: " ++ show res
            Left err  -> expectationFailure $ "Parsing failed" ++ errorBundlePretty err

    describe "Whitespace and Comments" $ do
      it "handles multiple spaces and newlines" $ do
        case parseLisp " ( \n + \t 1 \n 2 \t ) " of
            Right (List [SSymbol "+", SInteger 1, SInteger 2]) -> return ()
            Right res -> expectationFailure $ "Whitespace failure: got " ++ show res
            Left err  -> expectationFailure $ "Parsing failed with bad spacing: " ++ errorBundlePretty err

      it "ignores line comments" $ do
        case parseLisp " ( + 1 ; This is a comment\n 2 ) " of
            Right (List [SSymbol "+", SInteger 1, SInteger 2]) -> return ()
            Right res -> expectationFailure $ "Comment failure: got " ++ show res
            Left err  -> expectationFailure $ "Parsing failed with comment: " ++ errorBundlePretty err

    describe "Error Handling" $ do
      it "fails on empty input" $ do
        shouldFail ""

      it "fails on unclosed parenthesis" $ do
        shouldFail "(+ 1 2"

      it "fails on extraneous token at end" $ do
        shouldFail "(+ 1 2) foo"

      it "fails on unexpected opening bracket" $ do
        shouldFail "["

      it "fails on too many closing parentheses" $ do
        shouldFail "(+ 1 2))"
