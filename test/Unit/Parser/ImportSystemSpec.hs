{-
-- EPITECH PROJECT, 2025
-- npydos
-- File description:
-- Unit tests for Import System
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.ImportSystemSpec (spec) where

import Test.Hspec
import System.Directory ( removeFile)
import Control.Exception (bracket_, catch, SomeException)
import qualified Data.Text as T
import Data.List (isInfixOf)

import AST.Ast (Ast(..), cleanAst)
import Parser.ImportSystem (resolveImports, constructForAst)
import Common.Type.Integer (IntValue(..))

p :: String -> T.Text
p = T.pack

withTempFile :: FilePath -> String -> IO a -> IO a
withTempFile path content action = bracket_
    (writeFile path content)
    (ignoringIOErrors (removeFile path))
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors action = action `catch` (\(_ :: SomeException) -> return ())

spec :: Spec
spec = describe "Import System Resolution" $ do

    describe "Structural Recursion (No Imports)" $ do
        it "Preserves ADefineFunc structure" $ do
            let input = [ADefineFunc (p "main") [] (p "void") (AInteger (I8 0))]
            result <- resolveImports input
            result `shouldSatisfy` \case 
                Right [ADefineFunc "main" [] "void" (AInteger (I8 0))] -> True
                _ -> False

        it "Preserves AIf structure recursively" $ do
            let input = [AIf (ABool True) (AInteger (I8 1)) (AInteger (I8 2))]
            result <- resolveImports input
            result `shouldSatisfy` \case
                Right [AIf (ABool True) (AInteger (I8 1)) (AInteger (I8 2))] -> True
                _ -> False

        it "Preserves AWhile structure recursively" $ do
            let input = [AWhile (ABool True) AVoid]
            result <- resolveImports input
            result `shouldSatisfy` \case
                Right [AWhile (ABool True) AVoid] -> True
                _ -> False

        it "Preserves AFor structure recursively" $ do
            let input = [AFor (AInteger (I8 0)) (ABool True) (AInteger (I8 1)) AVoid]
            result <- resolveImports input
            result `shouldSatisfy` \case
                Right [AFor (AInteger (I8 0)) (ABool True) (AInteger (I8 1)) AVoid] -> True
                _ -> False
        
        it "Preserves APos wrapper" $ do
            let input = [APos 1 1 (AInteger (I8 42))]
            result <- resolveImports input
            result `shouldSatisfy` \case
                Right asts -> case map cleanAst asts of
                    [AInteger (I8 42)] -> True
                    _ -> False
                _ -> False

    describe "Import Resolution (IO)" $ do

        it "Resolves a simple valid import" $ do
            let fileName = "test_simple.npy"
            let fileContent = "x = 42;"
            
            withTempFile fileName fileContent $ do
                let input = [AImport (p fileName)]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Right asts -> case map cleanAst asts of
                        [AExprStmt (ASetVar "x" "auto" (AInteger (I8 42)))] -> True
                        _ -> False
                    _ -> False

        it "Resolves imports inside a block (AList)" $ do
            let fileName = "test_block.npy"
            let fileContent = "ret 1;"
            
            withTempFile fileName fileContent $ do
                let input = [AList [AImport (p fileName)]]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Right asts -> case map cleanAst asts of
                        [AList [AReturn (AInteger (I8 1))]] -> True
                        _ -> False
                    _ -> False

        it "Handles non-existent files gracefully" $ do
            let input = [AImport (p "non_existent_file_XYZ.npy")]
            result <- resolveImports input
            
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Handles syntax errors in imported files" $ do
            let fileName = "test_bad_syntax.npy"
            let fileContent = "this is not valid npydos code"
            
            withTempFile fileName fileContent $ do
                let input = [AImport (p fileName)]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Left err -> "Parse error" `isInfixOf` err
                    _ -> False

    describe "Nested Import Logic" $ do
        
        it "Resolves nested imports (A -> B)" $ do
            let fileA = "test_A.npy"
            let fileB = "test_B.npy"
            
            let contentB = "val = 100;"
            let contentA = "import \"test_B.npy\";"
            
            withTempFile fileB contentB $ do
                withTempFile fileA contentA $ do
                    let input = [AImport (p fileA)]
                    result <- resolveImports input
                    
                    result `shouldSatisfy` \case
                        Right asts -> case map cleanAst asts of
                            [AExprStmt (ASetVar "val" "auto" (AInteger (I8 100)))] -> True
                            _ -> False
                        _ -> False

        it "Resolves import inside a Function body" $ do
            let fileName = "test_func_body.npy"
            let content = "y = 1;"
            
            withTempFile fileName content $ do
                let body = AList [AImport (p fileName)]
                let func = ADefineFunc (p "f") [] (p "void") body
                
                result <- resolveImports [func]
                
                result `shouldSatisfy` \case
                    Right asts -> case map cleanAst asts of
                        [ADefineFunc "f" [] "void" (AList [AExprStmt (ASetVar "y" "auto" (AInteger (I8 1)))])] -> True
                        _ -> False
                    _ -> False

    describe "Specific Coverage Tests" $ do
        
        it "Propagates error from subsequent nodes" $ do
            let input = [AVoid, AImport (p "missing_tail_file.npy")]
            result <- resolveImports input
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Resolves import inside Lambda body" $ do
            let fileName = "lambda_test.npy"
            withTempFile fileName "val = 0;" $ do
                let input = [ADefineLambda ["args"] (AImport (p fileName))]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Right asts -> case map cleanAst asts of
                        [ADefineLambda ["args"] (AExprStmt (ASetVar "val" "auto" (AInteger (I8 0))))] -> True
                        _ -> False
                    _ -> False

        it "Preserves APos wrapper details" $ do
            let fileName = "apos_test.npy"
            withTempFile fileName "x = 1;" $ do
                let input = [APos 123 456 (AImport (p fileName))]
                result <- resolveImports input

                result `shouldSatisfy` \case
                    Right [APos 123 456 _] -> True
                    _ -> False

    describe "Error Message Formatting (.npy)" $ do
        
        it "Formats IO Error message correctly (includes filename)" $ do
            let fileName = "missing_data.npy"
            let input = [AImport (p fileName)]
            result <- resolveImports input
            
            result `shouldSatisfy` \case
                Left err -> ("IO Error reading import '" ++ fileName ++ "':") `isInfixOf` err
                _ -> False

        it "Formats Parse Error message correctly (includes filename)" $ do
            let fileName = "corrupted_script.npy"
            withTempFile fileName "this is not valid code" $ do
                let input = [AImport (p fileName)]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Left err -> ("Parse error in '" ++ fileName ++ "':\n") `isInfixOf` err
                    _ -> False
    
    describe "Detailed Error & Propagation Coverage" $ do
        
        it "Includes exception details (show ex) in IO Error" $ do
            let fileName = "ghost_file.npy"
            let input = [AImport (p fileName)]
            result <- resolveImports input
            
            result `shouldSatisfy` \case
                Left err -> ("IO Error reading import '" ++ fileName ++ "':") `isInfixOf` err 
                            && ("does not exist" `isInfixOf` err || "No such file" `isInfixOf` err)
                _ -> False

        it "Includes detailed parse error (errorBundlePretty)" $ do
            let fileName = "syntax_fail.npy"
            withTempFile fileName "x = ;" $ do
                let input = [AImport (p fileName)]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Left err -> "Parse error in '" `isInfixOf` err
                                && "unexpected" `isInfixOf` err
                    _ -> False

        it "Propagates errors from inside an AList (Left case)" $ do
            let fileName = "missing_inside_list.npy"
            let input = [AList [AImport (p fileName)]]
            
            result <- resolveImports input
            
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err && fileName `isInfixOf` err
                _ -> False
    
    describe "Error Propagation in Structures" $ do
        
        it "Propagates error from Function body" $ do
            let func = ADefineFunc (p "f") [] (p "void") (AImport (p "missing_in_func.npy"))
            result <- resolveImports [func]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Propagates error from Lambda body" $ do
            let lambda = ADefineLambda ["x"] (AImport (p "missing_in_lambda.npy"))
            result <- resolveImports [lambda]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Propagates error from While condition" $ do
            let loop = AWhile (AImport (p "missing_cond.npy")) AVoid
            result <- resolveImports [loop]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Propagates error from While body" $ do
            let loop = AWhile (ABool True) (AImport (p "missing_body.npy"))
            result <- resolveImports [loop]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False
    
    describe "Error Propagation in Control Structures (For & If)" $ do
        it "Propagates error from For loop initialization" $ do
            let loop = AFor (AImport (p "missing_init.npy")) 
                            (ABool True) 
                            AVoid 
                            AVoid
            result <- resolveImports [loop]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Propagates error from If condition" $ do
            let stmt = AIf (AImport (p "missing_cond.npy")) 
                           AVoid 
                           AVoid
            result <- resolveImports [stmt]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Propagates error from If 'Then' branch" $ do
            let stmt = AIf (ABool True) 
                           (AImport (p "missing_then.npy")) 
                           AVoid
            result <- resolveImports [stmt]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Propagates error from If 'Else' branch" $ do
            let stmt = AIf (ABool True) 
                           AVoid 
                           (AImport (p "missing_else.npy"))
            result <- resolveImports [stmt]
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False
    
    describe "Internal Helper Coverage" $ do

        it "Detects invalid argument count in For loop construction" $ do
            let badList = [AVoid, AVoid, AVoid]
            let result = constructForAst badList
            
            result `shouldSatisfy` \case
                Left err -> "Internal Error" `isInfixOf` err && "Invalid argument count" `isInfixOf` err
                _ -> False
    
    describe "Edge Cases in Node Processing" $ do

        it "Propagates error inside APos wrapper" $ do
            let input = [APos 1 1 (AImport (p "missing_apos.npy"))]
            result <- resolveImports input
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Unwraps APos when content expands to multiple nodes" $ do
            let fileName = "multi_stmt.npy"
            withTempFile fileName "a=1; b=2;" $ do
                let input = [APos 10 20 (AImport (p fileName))]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Right asts -> length asts == 2 
                    _ -> False

        it "Propagates error in single node context" $ do
            let input = [AIf (AImport (p "missing_single.npy")) AVoid AVoid]
            result <- resolveImports input
            result `shouldSatisfy` \case
                Left err -> "IO Error" `isInfixOf` err
                _ -> False

        it "Converts empty import to AVoid in single node context" $ do
            let fileName = "empty.npy"
            withTempFile fileName "" $ do
                let input = [AIf (AImport (p fileName)) AVoid AVoid]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Right [AIf AVoid AVoid AVoid] -> True
                    _ -> False

        it "Takes only the first node when import returns multiple in single context" $ do
            let fileName = "too_many.npy"
            withTempFile fileName "first=1; second=2;" $ do
                let input = [AIf (AImport (p fileName)) AVoid AVoid]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Right asts -> case map cleanAst asts of
                        [AIf (AExprStmt (ASetVar "first" "auto" (AInteger (I8 1)))) AVoid AVoid] -> True
                        _ -> False
                    _ -> False
    
    describe "Security & File Extension Checks" $ do

        it "Rejects imports without .npy extension" $ do
            let input = [AImport (p "script.txt")]
            result <- resolveImports input
            
            result `shouldSatisfy` \case
                Left err -> "must have .npy extension" `isInfixOf` err
                _ -> False

        it "Detects circular imports (A -> A)" $ do
            let fileA = "self_loop.npy"
            let contentA = "import \"self_loop.npy\";"
            
            withTempFile fileA contentA $ do
                let input = [AImport (p fileA)]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Left err -> "Circular or duplicate import detected" `isInfixOf` err 
                                && "self_loop.npy" `isInfixOf` err
                    _ -> False

        it "Detects indirect circular imports (A -> B -> A)" $ do
            let fileA = "cycle_A.npy"
            let fileB = "cycle_B.npy"
            
            let contentA = "import \"cycle_B.npy\";"
            let contentB = "import \"cycle_A.npy\";"
            
            withTempFile fileB contentB $ do
                withTempFile fileA contentA $ do
                    let input = [AImport (p fileA)]
                    result <- resolveImports input
                    
                    result `shouldSatisfy` \case
                        Left err -> "Circular or duplicate import detected" `isInfixOf` err
                        _ -> False
    
    it "Includes visited stack in circular import error message" $ do
            let fileName = "stack_trace_check.npy"
            let content = "import \"stack_trace_check.npy\";"
            
            withTempFile fileName content $ do
                let input = [AImport (p fileName)]
                result <- resolveImports input
                
                result `shouldSatisfy` \case
                    Left err -> 
                        " is already in the import stack " `isInfixOf` err
                        && show [fileName] `isInfixOf` err 
                    _ -> False
