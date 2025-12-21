{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- BuiltinsSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Z_old.Eval.BuiltinsSpec (spec) where

import Test.Hspec
import AST.Ast (Ast(..))
import Z_old.Src.Type.Integer (IntValue(..))
import Z_old.Src.Eval.Builtins (execBuiltin)
import qualified Data.Text as DT
import Data.List (isInfixOf)

p :: String -> DT.Text
p = DT.pack

spec :: Spec
spec = describe "Builtins Coverage 100%" $ do
    
    describe "Comparison Ops" $ do
        it "eq? integers equal" $ do
            execBuiltin (p "eq?") [AInteger (I8 10), AInteger (I8 10)] `shouldSatisfy` \case 
                Right (ABool True) -> True
                _ -> False

        it "eq? integers not equal" $ do
            execBuiltin (p "eq?") [AInteger (I8 10), AInteger (I8 20)] `shouldSatisfy` \case 
                Right (ABool False) -> True
                _ -> False

        it "eq? booleans equal" $ do
            execBuiltin (p "eq?") [ABool True, ABool True] `shouldSatisfy` \case 
                Right (ABool True) -> True
                _ -> False

        it "eq? mixed types (Error)" $ do
            execBuiltin (p "eq?") [AInteger (I8 1), ABool True] `shouldSatisfy` \case 
                Left err -> "expects two integers or two booleans" `isInfixOf` DT.unpack err
                _ -> False
        
        it "< true" $ do
            execBuiltin (p "<") [AInteger (I8 1), AInteger (I8 2)] `shouldSatisfy` \case 
                Right (ABool True) -> True
                _ -> False

        it "< false" $ do
            execBuiltin (p "<") [AInteger (I8 2), AInteger (I8 1)] `shouldSatisfy` \case 
                Right (ABool False) -> True
                _ -> False

        it "< arg mismatch" $ do
             execBuiltin (p "<") [AInteger (I8 1)] `shouldSatisfy` \case 
                Left err -> "expects two integers" `isInfixOf` DT.unpack err
                _ -> False

        it "> true" $ do
            execBuiltin (p ">") [AInteger (I8 2), AInteger (I8 1)] `shouldSatisfy` \case 
                Right (ABool True) -> True
                _ -> False

        it "> false" $ do
            execBuiltin (p ">") [AInteger (I8 1), AInteger (I8 2)] `shouldSatisfy` \case 
                Right (ABool False) -> True
                _ -> False

        it "> arg mismatch" $ do
             execBuiltin (p ">") [AInteger (I8 1)] `shouldSatisfy` \case 
                Left err -> "expects two integers" `isInfixOf` DT.unpack err
                _ -> False

    describe "Arithmetic Ops" $ do
        it "+ success" $ do
            execBuiltin (p "+") [AInteger (I8 10), AInteger (I8 20)] `shouldSatisfy` \case 
                Right (AInteger (I8 30)) -> True
                _ -> False

        it "+ arg mismatch" $ do
            execBuiltin (p "+") [AInteger (I8 10)] `shouldSatisfy` \case 
                Left err -> "expects two integers" `isInfixOf` DT.unpack err
                _ -> False

        it "- success" $ do
            execBuiltin (p "-") [AInteger (I8 10), AInteger (I8 3)] `shouldSatisfy` \case 
                Right (AInteger (I8 7)) -> True
                _ -> False

        it "- arg mismatch" $ do
            execBuiltin (p "-") [AInteger (I8 10)] `shouldSatisfy` \case 
                Left err -> "expects two integers" `isInfixOf` DT.unpack err
                _ -> False

        it "* success" $ do
            execBuiltin (p "*") [AInteger (I8 10), AInteger (I8 2)] `shouldSatisfy` \case 
                Right (AInteger (I8 20)) -> True
                _ -> False

        it "* arg mismatch" $ do
            execBuiltin (p "*") [AInteger (I8 10)] `shouldSatisfy` \case 
                Left err -> "expects two integers" `isInfixOf` DT.unpack err
                _ -> False

        it "div success" $ do
            execBuiltin (p "div") [AInteger (I8 10), AInteger (I8 2)] `shouldSatisfy` \case 
                Right (AInteger (I8 5)) -> True
                _ -> False

        it "div by zero" $ do
            execBuiltin (p "div") [AInteger (I8 10), AInteger (I8 0)] `shouldSatisfy` \case 
                Left err -> "division by zero" `isInfixOf` DT.unpack err
                _ -> False

        it "div arg mismatch" $ do
            execBuiltin (p "div") [AInteger (I8 10)] `shouldSatisfy` \case 
                Left err -> "expects two integers" `isInfixOf` DT.unpack err
                _ -> False

        it "mod success" $ do
            execBuiltin (p "mod") [AInteger (I8 10), AInteger (I8 3)] `shouldSatisfy` \case 
                Right (AInteger (I8 1)) -> True
                _ -> False

        it "mod by zero" $ do
            execBuiltin (p "mod") [AInteger (I8 10), AInteger (I8 0)] `shouldSatisfy` \case 
                Left err -> "division by zero" `isInfixOf` DT.unpack err
                _ -> False

        it "mod arg mismatch" $ do
            execBuiltin (p "mod") [AInteger (I8 10)] `shouldSatisfy` \case 
                Left err -> "expects two integers" `isInfixOf` DT.unpack err
                _ -> False

    describe "List Ops" $ do
        
        it "list: creates list" $ do
            execBuiltin (p "list") [AInteger (I8 1), AInteger (I8 2)] `shouldSatisfy` \case 
                Right (AList [AInteger (I8 1), AInteger (I8 2)]) -> True
                _ -> False

        it "cons: success" $ do
            execBuiltin (p "cons") [AInteger (I8 1), AList [AInteger (I8 2)]] `shouldSatisfy` \case 
                Right (AList [AInteger (I8 1), AInteger (I8 2)]) -> True
                _ -> False

        it "cons: error (not a list)" $ do
            execBuiltin (p "cons") [AInteger (I8 1), AInteger (I8 2)] `shouldSatisfy` \case 
                Left err -> "Expected list" `isInfixOf` DT.unpack err
                _ -> False

        it "cons: error (arg count)" $ do
            execBuiltin (p "cons") [AInteger (I8 1)] `shouldSatisfy` \case 
                Left err -> "expects two args" `isInfixOf` DT.unpack err
                _ -> False

        it "car: success" $ do
            execBuiltin (p "car") [AList [AInteger (I8 1), AInteger (I8 2)]] `shouldSatisfy` \case 
                Right (AInteger (I8 1)) -> True
                _ -> False

        it "car: error (empty)" $ do
            execBuiltin (p "car") [AList []] `shouldSatisfy` \case 
                Left err -> "called on empty list" `isInfixOf` DT.unpack err
                _ -> False

        it "car: error (not a list)" $ do
            execBuiltin (p "car") [AInteger (I8 1)] `shouldSatisfy` \case 
                Left err -> "Expected list" `isInfixOf` DT.unpack err
                _ -> False

        it "car: error (arg count)" $ do
            execBuiltin (p "car") [] `shouldSatisfy` \case 
                Left err -> "expects one arg" `isInfixOf` DT.unpack err
                _ -> False

        it "cdr: success" $ do
            execBuiltin (p "cdr") [AList [AInteger (I8 1), AInteger (I8 2)]] `shouldSatisfy` \case 
                Right (AList [AInteger (I8 2)]) -> True
                _ -> False

        it "cdr: error (empty)" $ do
            execBuiltin (p "cdr") [AList []] `shouldSatisfy` \case 
                Left err -> "called on empty list" `isInfixOf` DT.unpack err
                _ -> False

        it "cdr: error (not a list)" $ do
            execBuiltin (p "cdr") [AInteger (I8 1)] `shouldSatisfy` \case 
                Left err -> "Expected list" `isInfixOf` DT.unpack err
                _ -> False

        it "cdr: error (arg count)" $ do
            execBuiltin (p "cdr") [] `shouldSatisfy` \case 
                Left err -> "expects one arg" `isInfixOf` DT.unpack err
                _ -> False

        it "list?: true" $ do
            execBuiltin (p "list?") [AList []] `shouldSatisfy` \case 
                Right (ABool True) -> True
                _ -> False

        it "list?: false" $ do
            execBuiltin (p "list?") [AInteger (I8 1)] `shouldSatisfy` \case 
                Right (ABool False) -> True
                _ -> False

        it "list?: error (arg count)" $ do
             execBuiltin (p "list?") [] `shouldSatisfy` \case 
                Left err -> "expects one arg" `isInfixOf` DT.unpack err
                _ -> False

        it "append: success" $ do
            let l1 = AList [AInteger (I8 1)]
            let l2 = AList [AInteger (I8 2)]
            execBuiltin (p "append") [l1, l2] `shouldSatisfy` \case 
                Right (AList [AInteger (I8 1), AInteger (I8 2)]) -> True
                _ -> False

        it "append: error (not a list)" $ do
            execBuiltin (p "append") [AList [], AInteger (I8 1)] `shouldSatisfy` \case 
                Left err -> "Expected list" `isInfixOf` DT.unpack err
                _ -> False

        it "length: success" $ do
            execBuiltin (p "length") [AList [AInteger (I8 1)]] `shouldSatisfy` \case
                Right (AInteger _) -> True 
                _ -> False

        it "length: error (not a list)" $ do
            execBuiltin (p "length") [AInteger (I8 1)] `shouldSatisfy` \case 
                Left err -> "Expected list" `isInfixOf` DT.unpack err
                _ -> False

        it "length: error (arg count)" $ do
            execBuiltin (p "length") [] `shouldSatisfy` \case 
                Left err -> "expects one arg" `isInfixOf` DT.unpack err
                _ -> False

    describe "Unknown" $ do
        it "unknown builtin returns error" $ do
            execBuiltin (p "what?") [] `shouldSatisfy` \case 
                Left err -> "Unknown builtin" `isInfixOf` DT.unpack err
                _ -> False

    describe "Logical Operators" $ do

        describe "AND (&&)" $ do
            it "Returns True when both operands are True" $ do
                execBuiltin (DT.pack "&&") [ABool True, ABool True] `shouldSatisfy` \case
                    Right (ABool True) -> True
                    _ -> False

            it "Returns False when first operand is False" $ do
                execBuiltin (DT.pack "&&") [ABool False, ABool True] `shouldSatisfy` \case
                    Right (ABool False) -> True
                    _ -> False

            it "Returns False when second operand is False" $ do
                execBuiltin (DT.pack "&&") [ABool True, ABool False] `shouldSatisfy` \case
                    Right (ABool False) -> True
                    _ -> False

            it "Returns False when both operands are False" $ do
                execBuiltin (DT.pack "&&") [ABool False, ABool False] `shouldSatisfy` \case
                    Right (ABool False) -> True
                    _ -> False

            it "Fails with invalid arguments (non-boolean)" $ do
                execBuiltin (DT.pack "&&") [AInteger (I8 1), ABool True] `shouldSatisfy` \case
                    Left _ -> True
                    _ -> False

        describe "OR (||)" $ do
            it "Returns True when first operand is True" $ do
                execBuiltin (DT.pack "||") [ABool True, ABool False] `shouldSatisfy` \case
                    Right (ABool True) -> True
                    _ -> False

            it "Returns True when second operand is True" $ do
                execBuiltin (DT.pack "||") [ABool False, ABool True] `shouldSatisfy` \case
                    Right (ABool True) -> True
                    _ -> False

            it "Returns True when both operands are True" $ do
                execBuiltin (DT.pack "||") [ABool True, ABool True] `shouldSatisfy` \case
                    Right (ABool True) -> True
                    _ -> False

            it "Returns False when both operands are False" $ do
                execBuiltin (DT.pack "||") [ABool False, ABool False] `shouldSatisfy` \case
                    Right (ABool False) -> True
                    _ -> False

            it "Fails with argument count mismatch" $ do
                execBuiltin (DT.pack "||") [ABool True] `shouldSatisfy` \case
                    Left _ -> True
                    _ -> False
        
        describe "NOT (!)" $ do
            it "Returns False when operand is True" $ do
                execBuiltin (DT.pack "!") [ABool True] `shouldSatisfy` \case
                    Right (ABool False) -> True
                    _ -> False

            it "Returns True when operand is False" $ do
                execBuiltin (DT.pack "!") [ABool False] `shouldSatisfy` \case
                    Right (ABool True) -> True
                    _ -> False

            it "Fails with invalid argument count" $ do
                execBuiltin (DT.pack "!") [ABool True, ABool False] `shouldSatisfy` \case
                    Left _ -> True
                    _ -> False

            it "Fails with non-boolean argument" $ do
                execBuiltin (DT.pack "!") [AInteger (I8 1)] `shouldSatisfy` \case
                    Left _ -> True
                    _ -> False

        describe "List Modification (update)" $ do
            it "Updates value at index" $ do
                let list = AList [AInteger (I8 1), AInteger (I8 2), AInteger (I8 3)]
                execBuiltin (p "update") [list, AInteger (I8 1), AInteger (I8 99)] `shouldSatisfy` \case
                    Right (AList [AInteger (I8 1), AInteger (I8 99), AInteger (I8 3)]) -> True
                    _ -> False

            it "Returns error on index out of bounds" $ do
                let list = AList [AInteger (I8 1)]
                execBuiltin (p "update") [list, AInteger (I8 5), AInteger (I8 99)] `shouldSatisfy` \case
                    Left err -> "Index out of bounds" `isInfixOf` DT.unpack err
                    _ -> False
