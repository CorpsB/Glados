{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerSpec
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.ASM.CompilerSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate, SomeException)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T

import AST.Ast (Ast(..))
import qualified Z_old.Src.Type.Integer as Legacy

import Compiler.ASM.Compiler
  ( compileIf
  , compileSetVar
  , compileDefineFun
  , compileDefineLambda
  , getLambdaFreeVariables
  )

import Compiler.ASM.AstToAsm
  ( astIntToAsm
  , astBoolToAsm
  , astSymbolToAsm
  , astListToAsm
  , astCallToAsm
  )

import Compiler.ASM.CompilerMonad (CompilerMonad, emitInstruction)
import Compiler.CompilerState (CompilerState(..), ScopeType(..), createCompilerState)
import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.PsInstruction (PsInstruction(..))
import qualified Common.Type.Integer as Common

expectRight :: Either e a -> a
expectRight (Right x) = x
expectRight (Left _)  = error "Expected Right, got Left"

expectLeft :: Either a b -> a
expectLeft (Left e)  = e
expectLeft (Right _) = error "Expected Left, got Right"

runCM :: CompilerMonad a -> CompilerState -> Either T.Text (a, CompilerState)
runCM action st = runStateT action st

legacyToInt :: Legacy.IntValue -> Int
legacyToInt (Legacy.I8 n)    = fromIntegral n
legacyToInt (Legacy.I16 n)   = fromIntegral n
legacyToInt (Legacy.I32 n)   = fromIntegral n
legacyToInt (Legacy.I64 n)   = fromIntegral n
legacyToInt (Legacy.U8 n)    = fromIntegral n
legacyToInt (Legacy.U16 n)   = fromIntegral n
legacyToInt (Legacy.U32 n)   = fromIntegral n
legacyToInt (Legacy.U64 n)   = fromIntegral n
legacyToInt (Legacy.IChar c) = fromEnum c

compileStub :: Ast -> CompilerMonad ()
compileStub (AInteger n)         = astIntToAsm (legacyToInt n)
compileStub (ABool b)            = astBoolToAsm b
compileStub (ASymbol s)          = astSymbolToAsm s
compileStub (AList xs)           = astListToAsm compileStub xs
compileStub (ACall f args)       = astCallToAsm compileStub f args
compileStub (AIf c t e)          = compileIf compileStub c t e
compileStub (ADefineLambda p b)  = compileDefineLambda compileStub p b
compileStub other                = lift (Left ("compileStub unsupported: " <> T.pack (show other)))

spec :: Spec
spec = describe "Compiler.ASM.Compiler (max coverage)" $ do

  describe "Helpers coverage" $ do
    it "expectRight throws on Left" $ do
      evaluate (expectRight (Left ("boom" :: T.Text) :: Either T.Text Int))
        `shouldThrow` (\(_ :: SomeException) -> True)

    it "expectLeft throws on Right" $ do
      evaluate (expectLeft (Right (123 :: Int) :: Either T.Text Int))
        `shouldThrow` (\(_ :: SomeException) -> True)

  describe "legacyToInt covers IntValue constructors" $ do
    it "covers every constructor" $ do
      legacyToInt (Legacy.I8 1) `shouldBe` 1
      legacyToInt (Legacy.I16 1) `shouldBe` 1
      legacyToInt (Legacy.I32 1) `shouldBe` 1
      legacyToInt (Legacy.I64 1) `shouldBe` 1
      legacyToInt (Legacy.U8 1) `shouldBe` 1
      legacyToInt (Legacy.U16 1) `shouldBe` 1
      legacyToInt (Legacy.U32 1) `shouldBe` 1
      legacyToInt (Legacy.U64 1) `shouldBe` 1
      legacyToInt (Legacy.IChar 'A') `shouldBe` 65

  describe "compileStub (covers all its pattern branches)" $ do
    it "AInteger delegates to astIntToAsm" $ do
      let (_, st) = expectRight (runCM (compileStub (AInteger (Legacy.I32 7))) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmInt (Common.I64 7))))

    it "ABool delegates to astBoolToAsm" $ do
      let (_, st) = expectRight (runCM (compileStub (ABool True)) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmBool True)))

    it "ASymbol delegates to astSymbolToAsm (global)" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeGlobal, 2) }
      let (_, st) = expectRight (runCM (compileStub (ASymbol "x")) st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadGlobal 2))

    it "AList delegates to astListToAsm" $ do
      let (_, st) = expectRight (runCM (compileStub (AList [ABool True, ABool False])) createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , Real (Push (ImmBool False))
          , CallLabel "list"
          ]

    it "ACall delegates to astCallToAsm (builtin)" $ do
      let ast = ACall (ASymbol "+") [AInteger (Legacy.I32 1), AInteger (Legacy.I32 2)]
      let (_, st) = expectRight (runCM (compileStub ast) createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmInt (Common.I64 1)))
          , Real (Push (ImmInt (Common.I64 2)))
          , Real Add
          ]

    it "ACall delegates to astCallToAsm (non-builtin -> CallLabel)" $ do
      let ast = ACall (ASymbol "myFunc") [ABool True]
      let (_, st) = expectRight (runCM (compileStub ast) createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , CallLabel "myFunc"
          ]

    it "AIf delegates to compileIf (via compileStub branch)" $ do
      let ast = AIf (ABool True) (ABool False) (ABool True)
      let (_, st) = expectRight (runCM (compileStub ast) createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , JumpIfFalseLabel "else_0"
          , Real (Push (ImmBool False))
          , JumpLabel "endif_1"
          , LabelDef "else_0"
          , Real (Push (ImmBool True))
          , LabelDef "endif_1"
          ]
      csLabelCnt st `shouldBe` 2

    it "ADefineLambda delegates to compileDefineLambda (via compileStub branch)" $ do
      let ast = ADefineLambda ["x"] (ASymbol "x")
      let (_, st) = expectRight (runCM (compileStub ast) createCompilerState)
      csCode st `shouldBe` Seq.singleton (MakeClosureLabel "lambda_0" 0)
      csFuncs st `shouldBe`
        Seq.fromList
          [ LabelDef "lambda_0"
          , Real (LoadLocal 0)
          , Real Ret
          ]
      csLabelCnt st `shouldBe` 1

    it "unsupported branch returns Left" $ do
      let err = expectLeft (runCM (compileStub AVoid) createCompilerState)
      T.unpack err `shouldContain` "compileStub unsupported"

  describe "getLambdaFreeVariables (all branches)" $ do
    it "covers ASymbol/AInteger/ABool/ADefineLambda/ASetVar/ACall/AIf/AList/default" $ do
      getLambdaFreeVariables (ASymbol "x") `shouldBe` Set.fromList ["x"]
      getLambdaFreeVariables (AInteger (Legacy.I32 132)) `shouldBe` Set.empty
      getLambdaFreeVariables (ABool True) `shouldBe` Set.empty

      let lam = ADefineLambda ["x"] (AList [ASymbol "x", ASymbol "y"])
      getLambdaFreeVariables lam `shouldBe` Set.fromList ["y"]

      let astVar = ASetVar "x" "Int" (AList [ASymbol "x", ASymbol "y"])
      getLambdaFreeVariables astVar `shouldBe` Set.fromList ["y"]

      let callAst = ACall (ASymbol "+") [ASymbol "x", AInteger (Legacy.I32 2)]
      getLambdaFreeVariables callAst `shouldBe` Set.fromList ["x"]

      let ifAst = AIf (ASymbol "c") (ASymbol "t") (AList [ASymbol "e"])
      getLambdaFreeVariables ifAst `shouldBe` Set.fromList ["c","t","e"]

      let lst = AList [ASymbol "a", AList [ASymbol "b"], ABool False]
      getLambdaFreeVariables lst `shouldBe` Set.fromList ["a","b"]

      getLambdaFreeVariables (AImport "Std") `shouldBe` Set.empty

  describe "compileIf" $ do
    it "emits expected control-flow sequence and labels" $ do
      let ast = compileIf compileStub (ABool True) (ABool False) (ABool True)
      let (_, st) = expectRight (runCM ast createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , JumpIfFalseLabel "else_0"
          , Real (Push (ImmBool False))
          , JumpLabel "endif_1"
          , LabelDef "else_0"
          , Real (Push (ImmBool True))
          , LabelDef "endif_1"
          ]
      csLabelCnt st `shouldBe` 2

    it "propagates compileFn errors (condition)" $ do
      let badCompile _ = lift (Left "boom")
      let err = expectLeft (runCM (compileIf badCompile (ABool True) (ABool True) (ABool False)) createCompilerState)
      err `shouldBe` "boom"

  describe "compileSetVar" $ do
    it "stores value and registers global symbol" $ do
      let (_, st) = expectRight (runCM (compileSetVar compileStub "x" (AInteger (Legacy.I32 3))) createCompilerState)
      Map.lookup "x" (csSymbols st) `shouldBe` Just (ScopeGlobal, 0)
      csNextIndex st `shouldBe` 1
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmInt (Common.I64 3)))
          , Real (StoreGlobal 0)
          ]

    it "fails on redefinition" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeGlobal, 7), csNextIndex = 8 }
      let err = expectLeft (runCM (compileSetVar compileStub "x" (ABool True)) st0)
      err `shouldBe` "Symbol already defined: x"

    it "propagates compileFn error" $ do
      let badCompile _ = lift (Left "Manual Error") :: CompilerMonad ()
      let err = expectLeft (runCM (compileSetVar badCompile "x" (ABool True)) createCompilerState)
      err `shouldBe` "Manual Error"

  describe "compileDefineFun" $ do
    it "compiles isolated function; code goes to csFuncs, outer csCode preserved" $ do
      let action = do
            emitInstruction Nop
            compileDefineFun compileStub "foo" ["x","y"] (ASymbol "x")
            emitInstruction Halt
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe` Seq.fromList [Real Nop, Real Halt]
      csFuncs st `shouldBe`
        Seq.fromList
          [ LabelDef "fun_foo_0"
          , Real (LoadLocal 0)
          , Real Ret
          ]
      csLabelCnt st `shouldBe` 1

    it "nested lambda inside function also ends up in csFuncs (merged)" $ do
      let action = compileDefineFun compileStub "outer" ["x"] (ADefineLambda ["y"] (ASymbol "x"))
      let (_, st) = expectRight (runCM action createCompilerState)

      csCode st `shouldBe` Seq.empty

      csLabelCnt st `shouldBe` 2

      let fs = csFuncs st
      Seq.length fs `shouldBe` 7
      Seq.index fs 0 `shouldBe` LabelDef "fun_outer_0"
      Seq.index fs 1 `shouldBe` Real (LoadLocal 0)
      Seq.index fs 2 `shouldBe` MakeClosureLabel "lambda_1" 1
      Seq.index fs 3 `shouldBe` Real Ret
      Seq.index fs 4 `shouldBe` LabelDef "lambda_1"
      Seq.index fs 5 `shouldBe` Real (LoadCapture 0)
      Seq.index fs 6 `shouldBe` Real Ret

  describe "compileDefineLambda" $ do
    it "multi-capture (global/global) + list body: loads captures, builds closure, lambda uses LoadCapture" $ do
      let st0 =
            createCompilerState
              { csSymbols =
                  Map.fromList
                    [ ("a", (ScopeGlobal, 2))
                    , ("b", (ScopeGlobal, 7))
                    ]
              }
      let body = AList [ASymbol "a", ASymbol "b"]
      let (_, st) = expectRight (runCM (compileDefineLambda compileStub ["x"] body) st0)

      csCode st `shouldBe`
        Seq.fromList
          [ Real (LoadGlobal 2)
          , Real (LoadGlobal 7)
          , MakeClosureLabel "lambda_0" 2
          ]

      csFuncs st `shouldBe`
        Seq.fromList
          [ LabelDef "lambda_0"
          , Real (LoadCapture 0)
          , Real (LoadCapture 1)
          , CallLabel "list"
          , Real Ret
          ]

      csLabelCnt st `shouldBe` 1

    it "no captures: only MakeClosureLabel in outer, body uses LoadLocal in function" $ do
      let (_, st) = expectRight (runCM (compileDefineLambda compileStub ["x"] (ASymbol "x")) createCompilerState)
      csCode st `shouldBe` Seq.singleton (MakeClosureLabel "lambda_0" 0)
      csFuncs st `shouldBe`
        Seq.fromList
          [ LabelDef "lambda_0"
          , Real (LoadLocal 0)
          , Real Ret
          ]

    it "fails if a capture is undefined" $ do
      let err = expectLeft (runCM (compileDefineLambda compileStub ["x"] (ASymbol "y")) createCompilerState)
      T.unpack err `shouldContain` "Undefined symbol: y"
