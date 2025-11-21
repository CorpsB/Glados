{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ParseTest.hs
-}


import Test.Hspec
import Parser.ParserISL (parseLisp)
import Lisp (SExpr(..))

shouldParse :: String -> SExpr -> Expectation
shouldParse input expected = case parseLisp input of
    Right result -> result `shouldSatisfy` (== expected)
    Left _       -> expectationFailure $ "Parsing failed for input: " ++ input

shouldFail :: String -> Expectation
shouldFail input = case parseLisp input of
    Left _  -> return ()
    Right res -> expectationFailure $ "Parsing should have failed but succeded with: " ++ show res

main :: IO ()
main = hspec $ do
  describe "Parser LISP" $ do
    
    describe "Integers" $ do
      it "parses positive integers" $ do
        shouldParse "42" (SInteger 42)
        shouldParse "123456" (SInteger 123456)
      
      it "parses negative integers" $ do
        shouldParse "-42" (SInteger (-42))
        shouldParse "-0" (SInteger 0)

    describe "Symbols" $ do
      it "parses simple symbols" $ do
        shouldParse "foo" (SSymbol "foo")
        shouldParse "define" (SSymbol "define")
      
      it "parses operators as symbols" $ do
        shouldParse "+" (SSymbol "+")
        shouldParse ">=" (SSymbol ">=")

    describe "Lists" $ do
      it "parses empty list" $ do
        shouldParse "()" (List [])
        
      it "parses simple list" $ do
        shouldParse "(+ 1 2)" (List [SSymbol "+", SInteger 1, SInteger 2])

      it "parses nested lists" $ do
        shouldParse "(define x (* 2 3))" 
          (List [SSymbol "define", SSymbol "x", List [SSymbol "*", SInteger 2, SInteger 3]])

    describe "Error Handling" $ do
      it "fails on unclosed parenthesis" $ do
        shouldFail "(+ 1 2"
      
      it "fails on empty input" $ do
        shouldFail ""