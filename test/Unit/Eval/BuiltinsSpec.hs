{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- BuiltinsSpec.hs
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval.BuiltinsSpec (spec) where

import Test.Hspec
import Ast (Ast(..))
import Type.Integer (IntValue(..))
import Eval.Builtins (execBuiltin)
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
