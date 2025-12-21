{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- StatementSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.StatementSpec (spec) where

import Test.Hspec
import Parser.Statement (parseALL)
import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (IntValue(..))
import qualified Data.Text as DT

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Parser - Statement & Expression" $ do

    describe "Literals & Types" $ do
        
        it "Parses integers" $ do
            let code = "42;"
            parseALL (p code) `shouldSatisfy` \case
                Right [AInteger (I8 42)] -> True
                _ -> False

        it "Parses booleans (True)" $ do
            let code = "True;"
            parseALL (p code) `shouldSatisfy` \case
                Right [ABool True] -> True
                _ -> False

        it "Parses characters ('c')" $ do
            let code = "'c';"
            parseALL (p code) `shouldSatisfy` \case
                Right [AInteger (IChar 'c')] -> True
                _ -> False

        it "Parses strings as list of chars" $ do
            let code = "\"Hi\";"
            parseALL (p code) `shouldSatisfy` \case
                Right [AList [AInteger (IChar 'H'), AInteger (IChar 'i')]] -> True
                _ -> False

        it "Parses list literals" $ do
            let code = "[1, 2];"
            parseALL (p code) `shouldSatisfy` \case
                Right [AList [AInteger (I8 1), AInteger (I8 2)]] -> True
                _ -> False

    describe "Edge Cases & Error Handling (Coverage Target)" $ do

        it "Parses empty function body (returns AVoid)" $ do
            let code = "func fail() {}"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun _ _ _ AVoid] -> True
                _ -> False

        it "Returns a list of statements for a block" $ do
            let code = "func last() { 1; 2; 3; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun _ _ _ (AList [AInteger (I8 1), AInteger (I8 2), AInteger (I8 3)])] -> True
                _ -> False

        it "Triggers parse error (covers parseALL error formatting)" $ do
            let code = "func ("
            parseALL (p code) `shouldSatisfy` \case
                Left _ -> True
                _ -> False

    describe "Variable Definitions" $ do

        it "Parses untyped assignment (defaults to auto)" $ do
            let code = "x = 10;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar (AInteger (I8 10))] -> 
                    name == p "x" && typeVar == p "auto"
                _ -> False

        it "Parses typed assignment (int)" $ do
            let code = "y: int = 20;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar (AInteger (I8 20))] -> 
                    name == p "y" && typeVar == p "int"
                _ -> False

        it "Parses specific types (int8, uint64)" $ do
            let code = "z: int8 = 100;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar _] -> 
                    name == p "z" && typeVar == p "int8"
                _ -> False

        it "Parses list types ([char])" $ do
            let code = "str: [char] = \"hello\";"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name typeVar _] -> 
                    name == p "str" && typeVar == p "[char]"
                _ -> False

    describe "Expressions & Precedence" $ do

        it "Parses binary operations (+)" $ do
            let code = "1 + 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol op) [AInteger (I8 1), AInteger (I8 2)]] -> op == p "+"
                _ -> False

        it "Respects precedence (* before +)" $ do
            let code = "1 + 2 * 3;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol opPlus) [AInteger (I8 1), Call (ASymbol opMul) [AInteger (I8 2), AInteger (I8 3)]]] -> 
                    opPlus == p "+" && opMul == p "*"
                _ -> False

        it "Parses function calls in expressions" $ do
            let code = "add(x, 5);"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol func) [ASymbol arg1, AInteger (I8 5)]] -> 
                    func == p "add" && arg1 == p "x"
                _ -> False

    describe "Logic Operators" $ do

        it "Parses logical AND (&&)" $ do
            let code = "a && b;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol op) [ASymbol a, ASymbol b]] -> 
                    op == p "&&" && a == p "a" && b == p "b"
                _ -> False

        it "Parses logical OR (||)" $ do
            let code = "a || b;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol op) [ASymbol a, ASymbol b]] -> 
                    op == p "||" && a == p "a" && b == p "b"
                _ -> False

        it "Parses unary NOT (!)" $ do
            let code = "!x;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol op) [ASymbol x]] -> 
                    op == p "!" && x == p "x"
                _ -> False

        it "Respects logic precedence (! before &&)" $ do
            let code = "!a && b;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol opAnd) [Call (ASymbol opNot) [ASymbol a], ASymbol b]] -> 
                    opAnd == p "&&" && opNot == p "!" && a == p "a" && b == p "b"
                _ -> False

    describe "Function Definitions" $ do

        it "Parses simple function with return type" $ do
            let code = "func add(a: int, b: int) -> int { ret a + b; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun name args retType _body] -> 
                    name == p "add" &&
                    args == [(p "a", p "int"), (p "b", p "int")] &&
                    retType == p "int"
                _ -> False

        it "Parses function without return type (implicit Void)" $ do
            let code = "func main() { ret 0; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun name args retType _] -> 
                    name == p "main" &&
                    args == [] &&
                    retType == p "Void"
                _ -> False

        it "Parses function with complex arguments" $ do
            let code = "func printList(lst: [char]) -> void { ret 0; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [DefineFun _ args retType _] -> 
                    args == [(p "lst", p "[char]")] &&
                    retType == p "void"
                _ -> False

    describe "Control Flow" $ do
        
        it "Parses return statement" $ do
            let code = "ret 42;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Return (AInteger (I8 42))] -> True
                _ -> False

    describe "Unary Operators (Increment/Decrement)" $ do
        
        it "Parses ++x as x = x + 1" $ do
            let code = "++x;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call (ASymbol op) _)] -> 
                    name == p "x" && op == p "+"
                _ -> False

        it "Parses --y as y = y - 1" $ do
            let code = "--y;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call (ASymbol op) _)] -> 
                    name == p "y" && op == p "-"
                _ -> False

        it "Parses ++ inside an expression (complex)" $ do
            let code = "z = ++x * 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call _ [incrX, _])] -> 
                    case incrX of
                        Define x _ _ -> x == p "x"
                        _ -> False
                _ -> False
    
        it "Parses ++ on non-variable (literal) as a function call" $ do
            let code = "++5;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol op) [AInteger (I8 5)]] -> op == p "++"
                _ -> False

        it "Parses -- on expression as a function call" $ do
            let code = "--(x + 1);"
            parseALL (p code) `shouldSatisfy` \case
                Right [Call (ASymbol op) [Call (ASymbol plus) _]] -> 
                    op == p "--" && plus == p "+"
                _ -> False

    describe "Compound Assignment Operators" $ do
        
        it "Parses += as addition (x = x + 5)" $ do
            let code = "x += 5;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "x" && 
                    (case op of ASymbol s -> s == p "+"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "x"; _ -> False)
                _ -> False

        it "Parses -= as subtraction (y -= 2)" $ do
            let code = "y -= 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "y" && 
                    (case op of ASymbol s -> s == p "-"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "y"; _ -> False)
                _ -> False

        it "Parses *= as multiplication (z *= 10)" $ do
            let code = "z *= 10;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "z" && 
                    (case op of ASymbol s -> s == p "*"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "z"; _ -> False)
                _ -> False

        it "Parses /= as division (w /= 2)" $ do
            let code = "w /= 2;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call op [arg1, _])] -> 
                    name == p "w" && 
                    (case op of ASymbol s -> s == p "div"; _ -> False) &&
                    (case arg1 of ASymbol s -> s == p "w"; _ -> False)
                _ -> False

    describe "Array Indexing (Postfix)" $ do
        
        it "Parses simple indexing: arr[0]" $ do
            let code = "x = arr[0];"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call nthOp [arrArg, idxArg])] -> 
                    (case nthOp of ASymbol s -> s == p "nth"; _ -> False) &&
                    (case arrArg of ASymbol s -> s == p "arr"; _ -> False) &&
                    (case idxArg of AInteger (I8 0) -> True; _ -> False)
                _ -> False

        it "Parses nested indexing: matrix[x][y]" $ do
            let code = "val = matrix[x][y];"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call nthOuter [innerCall, idxY])] -> 
                    (case nthOuter of ASymbol s -> s == p "nth"; _ -> False) &&
                    (case idxY of ASymbol s -> s == p "y"; _ -> False) &&
                    (case innerCall of 
                        Call nthInner [matrixArg, idxX] ->
                            (case nthInner of ASymbol s -> s == p "nth"; _ -> False) &&
                            (case matrixArg of ASymbol s -> s == p "matrix"; _ -> False) &&
                            (case idxX of ASymbol s -> s == p "x"; _ -> False)
                        _ -> False)
                _ -> False

        it "Parses indexing on expression: [1, 2][0]" $ do
            let code = "first = [1, 2][0];"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call nthOp [listArg, idxArg])] -> 
                    (case nthOp of ASymbol s -> s == p "nth"; _ -> False) &&
                    (case listArg of AList _ -> True; _ -> False) &&
                    (case idxArg of AInteger (I8 0) -> True; _ -> False)
                _ -> False
    
    describe "Array Modification" $ do
        
        it "Parses array assignment: arr[0] = 5" $ do
            let code = "arr[0] = 5;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call updateOp [arrArg, idxArg, valArg])] -> 
                    name == p "arr" &&
                    (case updateOp of ASymbol s -> s == p "update"; _ -> False) &&
                    (case arrArg of ASymbol s -> s == p "arr"; _ -> False) &&
                    (case idxArg of AInteger (I8 0) -> True; _ -> False) &&
                    (case valArg of AInteger (I8 5) -> True; _ -> False)
                _ -> False

        it "Parses nested array assignment: mat[1][2] = 9" $ do
            let code = "mat[1][2] = 9;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define name _ (Call outerUpdate [matArg, idx1, innerUpdate])] -> 
                    name == p "mat" &&
                    (case outerUpdate of ASymbol s -> s == p "update"; _ -> False) &&
                    (case matArg of ASymbol s -> s == p "mat"; _ -> False) &&
                    (case idx1 of AInteger (I8 1) -> True; _ -> False) &&
                    (case innerUpdate of 
                        Call innerOp [nthCall, idx2, valArg] ->
                            (case innerOp of ASymbol s -> s == p "update"; _ -> False) &&
                            (case nthCall of Call (ASymbol nth) _ -> nth == p "nth"; _ -> False) &&
                            (case idx2 of AInteger (I8 2) -> True; _ -> False) &&
                            (case valArg of AInteger (I8 9) -> True; _ -> False)
                        _ -> False)
                _ -> False
    
    describe "Struct Definitions" $ do
        
        it "Parses a simple struct definition" $ do
            let code = "struct Point { x: int; y: int; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [Struct name fields] -> 
                    name == p "Point" &&
                    fields == [(p "x", p "int"), (p "y", p "int")]
                _ -> False

        it "Parses a struct with mixed types" $ do
            let code = "struct User { id: int; active: bool; name: [char]; }"
            parseALL (p code) `shouldSatisfy` \case
                Right [Struct name fields] -> 
                    name == p "User" &&
                    fields == [(p "id", p "int"), (p "active", p "bool"), (p "name", p "[char]")]
                _ -> False

        it "Parses an empty struct" $ do
            let code = "struct Empty {}"
            parseALL (p code) `shouldSatisfy` \case
                Right [Struct name fields] -> name == p "Empty" && null fields
                _ -> False

    describe "Struct Member Access (Dot Operator)" $ do
        
        it "Parses simple member access: p.x" $ do
            let code = "val = p.x;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call getField [objArg, fieldArg])] -> 
                    (case getField of ASymbol s -> s == p "get_field"; _ -> False) &&
                    (case objArg of ASymbol s -> s == p "p"; _ -> False) &&
                    (case fieldArg of AList [AInteger (IChar 'x')] -> True; _ -> False)
                _ -> False

        it "Parses nested access: user.profile.id" $ do
            let code = "id = user.profile.id;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call outerGet [innerCall, _])] -> 
                    (case outerGet of ASymbol s -> s == p "get_field"; _ -> False) &&
                    (case innerCall of 
                        Call innerGet [userArg, _] ->
                            (case innerGet of ASymbol s -> s == p "get_field"; _ -> False) &&
                            (case userArg of ASymbol s -> s == p "user"; _ -> False)
                        _ -> False)
                _ -> False

        it "Parses mixed array and member access: users[0].name" $ do
            let code = "name = users[0].name;"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (Call getField [nthCall, _])] -> 
                    (case getField of ASymbol s -> s == p "get_field"; _ -> False) &&
                    (case nthCall of Call (ASymbol nth) _ -> nth == p "nth"; _ -> False)
                _ -> False

    describe "Struct Instantiation (new)" $ do
        
        it "Parses simple instantiation" $ do
            let code = "p = new Point { x: 10, y: 20 };"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (New name fields)] -> 
                    name == p "Point" && length fields == 2
                _ -> False

        it "Parses instantiation with expressions" $ do
            let code = "c = new Circle { r: 5 + 5, origin: p };"
            parseALL (p code) `shouldSatisfy` \case
                Right [Define _ _ (New name fields)] -> 
                    name == p "Circle" &&
                    (case lookup (p "r") fields of
                        Just (Call (ASymbol s) _) -> s == p "+"
                        _ -> False)
                _ -> False
    
    describe "Import Statements" $ do
        
        it "Parses import statement" $ do
            let code = "import \"std/math.gld\";"
            parseALL (p code) `shouldSatisfy` \case
                Right [Import path] -> path == p "std/math.gld"
                _ -> False

        it "Parses import with complex path" $ do
            let code = "import \"../lib/utils_v2.gld\";"
            parseALL (p code) `shouldSatisfy` \case
                Right [Import path] -> path == p "../lib/utils_v2.gld"
                _ -> False
