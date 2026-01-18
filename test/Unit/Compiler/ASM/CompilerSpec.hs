{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Compiler.ASM.Compiler unit tests
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ASM.CompilerSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Trans.State.Lazy (runStateT)

import AST.Ast (Ast(..))
import Common.Type.Integer (IntValue(..))
import Compiler.CompilerState (CompilerState, createCompilerState)

import Compiler.ASM.CompilerMonad (CompilerMonad)
import Compiler.ASM.Compiler
  ( compileAst
  , compileIf
  , compileFor
  , compileWhile
  , compileSetVar
  , compileSetStruct
  , compileDefineFun
  , compileDefineLambda
  , compileDefineStruct
  , compileTail
  , compileLoop
  , compileAccessStruct
  , getLambdaFreeVariables
  , inferType
  )

runCompilerWithFresh :: CompilerMonad a -> Either Text (a, CompilerState)
runCompilerWithFresh act = runStateT act createCompilerState

noopCompile :: Ast -> CompilerMonad ()
noopCompile _ = return ()

i64 :: Integer -> Ast
i64 n = AInteger (I64 (fromIntegral n))

ichar :: Char -> Ast
ichar c = AInteger (IChar (fromIntegral (fromEnum c)))

spec :: Spec
spec = describe "Compiler.ASM.Compiler" $ do
  describe "getLambdaFreeVariables" $ do
    it "builtin symbol" $ do
      getLambdaFreeVariables (ASymbol "+") `shouldBe` Set.empty

    it "non-builtin symbol" $ do
      getLambdaFreeVariables (ASymbol "x") `shouldBe` Set.fromList ["x"]

    it "integer/bool" $ do
      getLambdaFreeVariables (AInteger (I64 1)) `shouldBe` Set.empty
      getLambdaFreeVariables (ABool True) `shouldBe` Set.empty

    it "lambda removes params" $ do
      let ast = ADefineLambda ["x","y"] (ACall (ASymbol "f") [ASymbol "x", ASymbol "z"])
      getLambdaFreeVariables ast `shouldBe` Set.fromList ["f","z"]

    it "setvar deletes name" $ do
      let ast = ASetVar "x" "int" (ACall (ASymbol "f") [ASymbol "x", ASymbol "y"])
      getLambdaFreeVariables ast `shouldBe` Set.fromList ["f","y"]

    it "call unions" $ do
      let ast = ACall (ASymbol "f") [ASymbol "x", ACall (ASymbol "g") [ASymbol "y"]]
      getLambdaFreeVariables ast `shouldBe` Set.fromList ["f","x","g","y"]

    it "if unions" $ do
      let ast = AIf (ASymbol "c") (ASymbol "t") (ASymbol "e")
      getLambdaFreeVariables ast `shouldBe` Set.fromList ["c","t","e"]

    it "list unions" $ do
      let ast = AList [ASymbol "a", AInteger (I64 1), ASymbol "b"]
      getLambdaFreeVariables ast `shouldBe` Set.fromList ["a","b"]

    it "default empty" $ do
      getLambdaFreeVariables AVoid `shouldBe` Set.empty
      getLambdaFreeVariables ABreak `shouldBe` Set.empty

  describe "compileTail" $ do
    it "builtin call" $ do
      let ast = ACall (ASymbol "+") [i64 1, i64 2]
      runCompilerWithFresh (compileTail noopCompile ast)
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "named non-builtin call" $ do
      let ast = ACall (ASymbol "myfun") [i64 1]
      runCompilerWithFresh (compileTail noopCompile ast)
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "indirect call" $ do
      let ast = ACall (AList []) [i64 1]
      runCompilerWithFresh (compileTail noopCompile ast)
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "if tail" $ do
      let ast = AIf (ABool True) (ACall (ASymbol "myfun") [i64 1]) (i64 0)
      runCompilerWithFresh (compileTail noopCompile ast)
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "empty list tail" $ do
      runCompilerWithFresh (compileTail noopCompile (AList []))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "non-empty list tail" $ do
      runCompilerWithFresh (compileTail noopCompile (AList [i64 1, i64 2]))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "other tail" $ do
      runCompilerWithFresh (compileTail noopCompile (ABool True))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

  describe "control flow helpers" $ do
    it "compileIf" $ do
      runCompilerWithFresh (compileIf noopCompile (ABool True) (i64 1) (i64 0))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "compileLoop" $ do
      runCompilerWithFresh (compileLoop noopCompile (ABool True) (i64 1) "end_label")
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "compileWhile" $ do
      runCompilerWithFresh (compileWhile noopCompile (ABool True) (i64 1))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "compileFor" $ do
      runCompilerWithFresh (compileFor noopCompile (i64 0) (ABool True) (i64 1) (i64 2))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

  describe "compileSetVar" $ do
    it "isStatement False" $ do
      runCompilerWithFresh (compileSetVar noopCompile "x" "int" (i64 1) False)
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "isStatement True" $ do
      runCompilerWithFresh (compileSetVar noopCompile "y" "int" (i64 2) True)
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

  describe "compileDefineStruct / compileSetStruct" $ do
    it "define struct" $ do
      runCompilerWithFresh (compileDefineStruct "Player" [("hp","int"), ("name","[char]")])
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "set struct ok" $ do
      let act = do
            compileDefineStruct "Player" [("hp","int"), ("name","[char]")]
            compileSetStruct noopCompile "Player"
              [ ("hp", i64 42)
              , ("name", AList [ichar 'A', ichar 'n', ichar 'n', ichar 'e'])
              ]
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "set struct missing field" $ do
      let act = do
            compileDefineStruct "Player" [("hp","int"), ("name","[char]")]
            compileSetStruct noopCompile "Player"
              [ ("hp", i64 42) ]
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of
          Left msg -> "Missing field" `T.isInfixOf` msg
          Right _  -> False)

  describe "compileDefineFun / compileDefineLambda" $ do
    it "define fun" $ do
      runCompilerWithFresh (compileDefineFun noopCompile "f" [("a","int"), ("b","int")] (i64 0))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "define lambda" $ do
      let act = do
            compileSetVar noopCompile "x" "int" (i64 1) True
            compileDefineLambda noopCompile ["p"] (ACall (ASymbol "+") [ASymbol "x", ASymbol "p"])
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

  describe "inferType" $ do
    it "pos unwrap" $ do
      let act = inferType (APos 0 0 (AInteger (I64 1)))
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right (t, _) -> t == "int"; _ -> False)

    it "integer/bool" $ do
      runCompilerWithFresh (inferType (AInteger (I64 1)))
        `shouldSatisfy` (\r -> case r of Right (t, _) -> t == "int"; _ -> False)
      runCompilerWithFresh (inferType (ABool True))
        `shouldSatisfy` (\r -> case r of Right (t, _) -> t == "bool"; _ -> False)

    it "symbol type lookup" $ do
      let act = do
            compileSetVar noopCompile "x" "int" (i64 1) True
            inferType (ASymbol "x")
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right (t, _) -> t == "int"; _ -> False)

    it "builtin call type" $ do
      runCompilerWithFresh (inferType (ACall (ASymbol "+") [i64 1, i64 2]))
        `shouldSatisfy` (\r -> case r of Right (t, _) -> t == "int"; _ -> False)

    it "non-builtin call fallback to symbol type" $ do
      let act = do
            compileSetVar noopCompile "f" "function" AVoid True
            inferType (ACall (ASymbol "f") [i64 1])
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right (t, _) -> t == "function"; _ -> False)

    it "access struct type" $ do
      let act = do
            compileDefineStruct "Player" [("hp","int")]
            compileSetVar noopCompile "p" "Player" AVoid True
            inferType (AAccessStruct (ASymbol "p") "hp")
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right (t, _) -> t == "int"; _ -> False)

    it "cannot infer complex" $ do
      runCompilerWithFresh (inferType (AIf (ABool True) (i64 1) (i64 0)))
        `shouldSatisfy` (\r -> case r of
          Left msg -> "Cannot infer type" `T.isInfixOf` msg
          Right _  -> False)

  describe "compileAccessStruct" $ do
    it "success" $ do
      let act = do
            compileDefineStruct "Player" [("hp","int")]
            compileSetVar noopCompile "p" "Player" AVoid True
            compileAccessStruct noopCompile (ASymbol "p") "hp"
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

  describe "compileAst" $ do
    it "basic nodes" $ do
      let act = do
            compileSetVar noopCompile "x" "int" (i64 1) True
            compileAst (APos 0 0 (ABlock
              [ AInteger (I64 1)
              , ABool True
              , ASymbol "x"
              , AVoid
              , AImport "whatever"
              , AExprStmt (AInteger (I64 2))
              ]))
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "break outside loop" $ do
      runCompilerWithFresh (compileAst ABreak)
        `shouldSatisfy` (\r -> case r of
          Left msg -> "break" `T.isInfixOf` msg
          Right _  -> False)

    it "break inside loop" $ do
      runCompilerWithFresh (compileAst (AWhile (ABool True) ABreak))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "return" $ do
      runCompilerWithFresh (compileAst (AReturn (AInteger (I64 0))))
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "attr_update ok with Symbol field" $ do
      let act = do
            compileDefineStruct "Player" [("hp","int")]
            compileSetVar noopCompile "p" "Player" AVoid True
            compileAst (ACall (ASymbol "attr_update") [ASymbol "p", ASymbol "hp", i64 99])
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of Right _ -> True; Left _ -> False)

    it "attr_update bad second arg type" $ do
      let act = do
            compileDefineStruct "Player" [("hp","int")]
            compileSetVar noopCompile "p" "Player" AVoid True
            compileAst (ACall (ASymbol "attr_update") [ASymbol "p", ABool True, i64 99])
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of
          Left msg -> "Second argument" `T.isInfixOf` msg
          Right _  -> False)

    it "attr_update bad string element" $ do
      let badString = AList [AInteger (I64 65)]
      let act = do
            compileDefineStruct "Player" [("hp","int")]
            compileSetVar noopCompile "p" "Player" AVoid True
            compileAst (ACall (ASymbol "attr_update") [ASymbol "p", badString, i64 99])
      runCompilerWithFresh act
        `shouldSatisfy` (\r -> case r of
          Left msg -> "Field name must be a string" `T.isInfixOf` msg
          Right _  -> False)
