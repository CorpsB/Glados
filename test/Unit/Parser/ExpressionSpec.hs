{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ExpressionSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ExpressionSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse)
import Parser.Expression (pExpr)
import AST.Ast (Ast(..), cleanAst)
import Common.Type.Integer (IntValue(..))
import qualified Data.Text as DT
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Int (Int8)
import Data.Char (ord)

parseExpr :: DT.Text -> Either (ParseErrorBundle DT.Text Void) Ast
parseExpr input = fmap cleanAst (parse pExpr "" input)

p :: String -> DT.Text
p = DT.pack

charToInt8 :: Char -> Int8
charToInt8 c = fromIntegral (ord c)

spec :: Spec
spec = describe "Parser.Expression - Full Coverage" $ do

    describe "Booleans" $ do
        it "Parses True" $ do
            parseExpr "True" `shouldSatisfy` \case
                Right (ABool True) -> True
                _ -> False
        
        it "Parses False (Coverage Target)" $ do
            parseExpr "False" `shouldSatisfy` \case
                Right (ABool False) -> True
                _ -> False

    describe "Parentheses & Priority" $ do
        it "Parses parenthesized expression" $ do
            parseExpr "(42)" `shouldSatisfy` \case
                Right (AInteger (I8 42)) -> True
                _ -> False

        it "Parses nested parentheses" $ do
            parseExpr "((10))" `shouldSatisfy` \case
                Right (AInteger (I8 10)) -> True
                _ -> False

    describe "Operators Coverage" $ do
        
        it "Parses Division (/ -> div)" $ do
            parseExpr "10 / 2" `shouldSatisfy` \case
                Right (ACall (ASymbol op) [AInteger (I8 10), AInteger (I8 2)]) -> op == p "div"
                _ -> False

        it "Parses Modulo (% -> mod)" $ do
            parseExpr "10 % 3" `shouldSatisfy` \case
                Right (ACall (ASymbol op) [AInteger (I8 10), AInteger (I8 3)]) -> op == p "mod"
                _ -> False

        it "Parses Subtraction (-)" $ do
            parseExpr "10 - 5" `shouldSatisfy` \case
                Right (ACall (ASymbol op) [AInteger (I8 10), AInteger (I8 5)]) -> op == p "-"
                _ -> False

        it "Parses Equality (== -> eq?)" $ do
            parseExpr "1 == 1" `shouldSatisfy` \case
                Right (ACall (ASymbol op) [AInteger (I8 1), AInteger (I8 1)]) -> op == p "eq?"
                _ -> False

        it "Parses Less Than (<)" $ do
            parseExpr "1 < 2" `shouldSatisfy` \case
                Right (ACall (ASymbol op) [AInteger (I8 1), AInteger (I8 2)]) -> op == p "<"
                _ -> False
        
        it "Parses Greater Than (>)" $ do
            parseExpr "2 > 1" `shouldSatisfy` \case
                Right (ACall (ASymbol op) [AInteger (I8 2), AInteger (I8 1)]) -> op == p ">"
                _ -> False

    describe "Other Types (Strings, Chars, Lists)" $ do
        it "Parses String" $ do
            parseExpr "\"abc\"" `shouldSatisfy` \case
                Right (AList _) -> True
                _ -> False

        it "Parses Char" $ do
            parseExpr "'c'" `shouldSatisfy` \case
                Right (AInteger (IChar v)) -> v == charToInt8 'c'
                _ -> False

        it "Parses List Literal" $ do
            parseExpr "[1, 2]" `shouldSatisfy` \case
                Right (AList _) -> True
                _ -> False
        
        it "Parses Function ACall" $ do
            parseExpr "foo(1)" `shouldSatisfy` \case
                Right (ACall (ASymbol s) _) -> s == p "foo"
                _ -> False

    describe "Coverage: Parser Helpers" $ do
        
        it "Triggers incrementOps fallback (non-symbol argument)" $ do
            let code = "++5"
            parseExpr code `shouldSatisfy` \case
                Right (ACall (ASymbol op) [AInteger (I8 5)]) -> op == p "++"
                _ -> False

        it "Triggers decrementOps fallback (non-symbol argument)" $ do
            let code = "--(x+1)"
            parseExpr code `shouldSatisfy` \case
                Right (ACall (ASymbol op) _) -> op == p "--"
                _ -> False

    describe "Expression Parser - Structure Access" $ do

        it "Parses simple field access (obj.field)" $ do
            let input = "player.hp"
            parseExpr input `shouldBe` Right (AAccessStruct (ASymbol (p "player")) (p "hp"))

        it "Parses nested field access (obj.sub.field)" $ do
            let input = "game.player.pos"
            parseExpr input `shouldBe` Right (AAccessStruct 
                                                (AAccessStruct (ASymbol (p "game")) (p "player")) 
                                                (p "pos"))

        it "Parses mixed array and struct access (arr[0].x)" $ do
            let input = "grid[0].x"
            
            case parseExpr input of
                Right (AAccessStruct _ name) | name == p "x" -> return ()
                
                Right ast -> expectationFailure $ "Expected AAccessStruct, got: " ++ show ast
                Left err -> expectationFailure $ "Parse error: " ++ show err

        it "Parses complex chain (obj.method(arg).res)" $ do
            let input = "getPlayer().stats.hp"
            case parseExpr input of
                Right (AAccessStruct (AAccessStruct (ACall _ _) sub) field) 
                    | sub == p "stats" && field == p "hp" -> return ()
                    
                Right ast -> expectationFailure $ "Structure incorrecte: " ++ show ast
                Left err -> expectationFailure $ "Parse error: " ++ show err
    
    describe "Function Calls (pCallSuffix)" $ do
            it "parses a function call with multiple arguments" $ do
                let result = parseExpr "myFunc(1, 2)"
                result `shouldSatisfy` (\r -> case r of
                    Right (ACall (ASymbol "myFunc") [AInteger (I8 1), AInteger (I8 2)]) -> True
                    _ -> False)

            it "parses a function call with no arguments" $ do
                let result = parseExpr "empty()"
                result `shouldSatisfy` (\r -> case r of
                    Right (ACall (ASymbol "empty") []) -> True
                    _ -> False)

            it "parses nested function calls" $ do
                let result = parseExpr "outer(inner(10))"
                result `shouldSatisfy` (\r -> case r of
                    Right (ACall (ASymbol "outer") [ACall (ASymbol "inner") [AInteger (I8 10)]]) -> True
                    _ -> False)

            it "parses complex mixed arguments" $ do
                let result = parseExpr "mix(1, True, variable)"
                result `shouldSatisfy` (\r -> case r of
                    Right (ACall (ASymbol "mix") [AInteger (I8 1), ABool True, ASymbol "variable"]) -> True
                    _ -> False)
    
    describe "Function Calls (pCallSuffix)" $ do
        it "parses a function call with multiple arguments" $ do
            let result = parseExpr "myFunc(1, 2)"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "myFunc") [AInteger (I8 1), AInteger (I8 2)]) -> True
                _ -> False)

        it "parses a function call with no arguments" $ do
            let result = parseExpr "empty()"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "empty") []) -> True
                _ -> False)

        it "parses nested function calls" $ do
            let result = parseExpr "outer(inner(10))"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "outer") [ACall (ASymbol "inner") [AInteger (I8 10)]]) -> True
                _ -> False)

        it "parses complex mixed arguments" $ do
            let result = parseExpr "mix(1, True, variable)"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "mix") [AInteger (I8 1), ABool True, ASymbol "variable"]) -> True
                _ -> False)

    describe "Lambda Expressions (pLambda)" $ do
        it "parses lambda with arrow syntax" $ do
            let result = parseExpr "lambda (x, y) -> x + y"
            result `shouldSatisfy` (\case
                Right (ADefineLambda ["x", "y"] (ACall (ASymbol "+") [ASymbol "x", ASymbol "y"])) -> True
                _ -> False)

        it "parses lambda without optional arrow" $ do
            let result = parseExpr "lambda (x) x"
            result `shouldSatisfy` (\case
                Right (ADefineLambda ["x"] (ASymbol "x")) -> True
                _ -> False)

        it "parses lambda with no arguments" $ do
            let result = parseExpr "lambda () 42"
            result `shouldSatisfy` (\case
                Right (ADefineLambda [] (AInteger (I8 42))) -> True
                _ -> False)

    describe "Control Flow (pIfExpr)" $ do
        it "parses complete if-else expression" $ do
            let result = parseExpr "if (True) { 1 } else { 0 }"
            result `shouldSatisfy` (\case
                Right (AIf (ABool True) (AInteger (I8 1)) (AInteger (I8 0))) -> True
                _ -> False)

        it "parses if without else (defaults to AVoid)" $ do
            let result = parseExpr "if (x < 10) { x }"
            result `shouldSatisfy` (\case
                Right (AIf (ACall (ASymbol "<") [ASymbol "x", AInteger (I8 10)]) (ASymbol "x") AVoid) -> True
                _ -> False)

        it "parses nested if expressions" $ do
            let result = parseExpr "if (a) { if (b) { 1 } }"
            result `shouldSatisfy` (\case
                Right (AIf (ASymbol "a") (AIf (ASymbol "b") (AInteger (I8 1)) AVoid) AVoid) -> True
                _ -> False)
    
    describe "String Escape Sequences (pEscapeCode & pStringChar)" $ do
        it "parses newline escape (\\n)" $ do
            let result = parseExpr "\"\\n\""
            result `shouldSatisfy` (\case
                Right (AList [AInteger (IChar 10)]) -> True
                _ -> False)

        it "parses carriage return escape (\\r)" $ do
            let result = parseExpr "\"\\r\""
            result `shouldSatisfy` (\case
                Right (AList [AInteger (IChar 13)]) -> True
                _ -> False)

        it "parses tab escape (\\t)" $ do
            let result = parseExpr "\"\\t\""
            result `shouldSatisfy` (\case
                Right (AList [AInteger (IChar 9)]) -> True
                _ -> False)

        it "parses null byte escape (\\0)" $ do
            let result = parseExpr "\"\\0\""
            result `shouldSatisfy` (\case
                Right (AList [AInteger (IChar 0)]) -> True
                _ -> False)

        it "parses backslash escape (\\\\)" $ do
            let result = parseExpr "\"\\\\\""
            result `shouldSatisfy` (\case
                Right (AList [AInteger (IChar 92)]) -> True
                _ -> False)

        it "parses double quote escape (\\\")" $ do
            let result = parseExpr "\"\\\"\""
            result `shouldSatisfy` (\case
                Right (AList [AInteger (IChar 34)]) -> True
                _ -> False)

        it "parses mixed normal and escaped chars" $ do
            let result = parseExpr "\"A\\nB\""
            result `shouldSatisfy` (\case
                Right (AList [AInteger (IChar 65), AInteger (IChar 10), AInteger (IChar 66)]) -> True
                _ -> False)

    describe "Syntactic Sugar Operators (sugarSyntOps)" $ do
        it "parses logical NOT (!)" $ do
            let result = parseExpr "!True"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "!") [ABool True]) -> True
                _ -> False)

        it "parses prefix increment (++) on variable" $ do
            let result = parseExpr "++x"
            result `shouldSatisfy` (\case
                Right (ASetVar "x" "auto" (ACall (ASymbol "+") [ASymbol "x", AInteger (I8 1)])) -> True
                _ -> False)

        it "parses prefix decrement (--) on variable" $ do
            let result = parseExpr "--x"
            result `shouldSatisfy` (\case
                Right (ASetVar "x" "auto" (ACall (ASymbol "-") [ASymbol "x", AInteger (I8 1)])) -> True
                _ -> False)

        it "parses postfix increment (++) on variable" $ do
            let result = parseExpr "x++"
            result `shouldSatisfy` (\case
                Right (ASetVar "x" "auto" (ACall (ASymbol "+") [ASymbol "x", AInteger (I8 1)])) -> True
                _ -> False)

        it "parses postfix decrement (--) on variable" $ do
            let result = parseExpr "x--"
            result `shouldSatisfy` (\case
                Right (ASetVar "x" "auto" (ACall (ASymbol "-") [ASymbol "x", AInteger (I8 1)])) -> True
                _ -> False)

    describe "Comparison Operators (comparisonOps)" $ do
        it "parses equality (==)" $ do
            let result = parseExpr "a == b"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "eq?") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False)

        it "parses inequality (!=)" $ do
            let result = parseExpr "a != b"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "neq?") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False)

        it "parses less than or equal (<=)" $ do
            let result = parseExpr "a <= b"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "<=") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False)

        it "parses greater than or equal (>=)" $ do
            let result = parseExpr "a >= b"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol ">=") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False)

        it "parses less than (<)" $ do
            let result = parseExpr "a < b"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "<") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False)

        it "parses greater than (>)" $ do
            let result = parseExpr "a > b"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol ">") [ASymbol "a", ASymbol "b"]) -> True
                _ -> False)
    
    describe "Increment/Decrement Internals (APos & Fallback)" $ do
        it "Traverses APos for increment (++x resolves to ASetVar)" $ do
            let result = parseExpr "++x"
            result `shouldSatisfy` (\case
                Right (ASetVar "x" "auto" (ACall (ASymbol "+") [ASymbol "x", AInteger (I8 1)])) -> True
                _ -> False)

        it "Uses fallback for increment on non-symbol (++5 resolves to ACall)" $ do
            let result = parseExpr "++5"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "++") [AInteger (I8 5)]) -> True
                _ -> False)

        it "Traverses APos for decrement (--x resolves to ASetVar)" $ do
            let result = parseExpr "--x"
            result `shouldSatisfy` (\case
                Right (ASetVar "x" "auto" (ACall (ASymbol "-") [ASymbol "x", AInteger (I8 1)])) -> True
                _ -> False)

        it "Uses fallback for decrement on non-symbol (--5 resolves to ACall)" $ do
            let result = parseExpr "--5"
            result `shouldSatisfy` (\case
                Right (ACall (ASymbol "--") [AInteger (I8 5)]) -> True
                _ -> False)
