{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- AstToAsmSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ASM.AstToAsmSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text, unpack)
import qualified Data.Text as T

import AST.Ast (Ast(..))
import qualified Common.Type.Integer as Common

import Compiler.ASM.AstToAsm
import Compiler.ASM.CompilerMonad (CompilerMonad)
import Compiler.CompilerState (CompilerState(..), ScopeType(..), createCompilerState)
import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.PsInstruction (PsInstruction(..))

expectRight :: Either e a -> a
expectRight (Right x) = x
expectRight (Left _)  = error "Expected Right, got Left"

expectLeft :: Either e a -> e
expectLeft (Left e)  = e
expectLeft (Right _) = error "Expected Left, got Right"

runCM :: CompilerMonad a -> CompilerState -> Either Text (a, CompilerState)
runCM action st = runStateT action st

mockCompile :: Ast -> CompilerMonad ()
mockCompile (AInteger n)   = astIntToAsm n
mockCompile (ABool b)      = astBoolToAsm b
mockCompile (ASymbol s)    = astSymbolToAsm s
mockCompile (AList l)      = astListToAsm mockCompile l
mockCompile (ACall f args) = astCallToAsm mockCompile f args
mockCompile other          = lift (Left ("mockCompile unsupported: " <> T.pack (show other)))

spec :: Spec
spec = describe "Compiler.ASM.AstToAsm (max coverage)" $ do
  describe "helpers" $ do
    it "expectRight / expectLeft ok" $ do
      expectRight (Right (42 :: Int) :: Either Text Int) `shouldBe` 42
      expectLeft (Left ("err" :: Text) :: Either Text Int) `shouldBe` "err"

    it "expectRight throws on Left" $ do
      evaluate (expectRight (Left ("boom" :: Text) :: Either Text Int)) `shouldThrow` anyErrorCall

    it "expectLeft throws on Right" $ do
      evaluate (expectLeft (Right (123 :: Int) :: Either Text Int)) `shouldThrow` anyErrorCall

  describe "builtinMap" $ do
    it "contains expected builtin operators" $ do
      Map.lookup "+" builtinMap `shouldBe` Just Add
      Map.lookup "-" builtinMap `shouldBe` Just Sub
      Map.lookup "*" builtinMap `shouldBe` Just Mul
      Map.lookup "div" builtinMap `shouldBe` Just Div
      Map.lookup "mod" builtinMap `shouldBe` Just Mod
      Map.lookup "eq?" builtinMap `shouldBe` Just Eq
      Map.lookup "<" builtinMap `shouldBe` Just Lt
      Map.lookup "<=" builtinMap `shouldBe` Just Le
      Map.lookup "print" builtinMap `shouldBe` Just Print
      Map.lookup "exit" builtinMap `shouldBe` Just Exit
      Map.lookup "cons" builtinMap `shouldBe` Just Cons
      Map.lookup "head" builtinMap `shouldBe` Just Head
      Map.lookup "tail" builtinMap `shouldBe` Just Tail
      Map.lookup "nth" builtinMap `shouldBe` Just Nth
      Map.lookup "int8" builtinMap `shouldBe` Just (Cast 0x01)
      Map.lookup "uint8" builtinMap `shouldBe` Just (Cast 0x02)
      Map.lookup "int16" builtinMap `shouldBe` Just (Cast 0x03)
      Map.lookup "uint16" builtinMap `shouldBe` Just (Cast 0x04)
      Map.lookup "int32" builtinMap `shouldBe` Just (Cast 0x05)
      Map.lookup "uint32" builtinMap `shouldBe` Just (Cast 0x06)
      Map.lookup "int64" builtinMap `shouldBe` Just (Cast 0x07)
      Map.lookup "uint64" builtinMap `shouldBe` Just (Cast 0x08)
      Map.lookup "char" builtinMap `shouldBe` Just (Cast 0x09)
      Map.lookup "uchar" builtinMap `shouldBe` Just (Cast 0x10)
      Map.lookup "unknown" builtinMap `shouldBe` Nothing

  describe "astIntToAsm" $ do
    it "emits Push ImmInt" $ do
      let (_, st) = expectRight (runCM (astIntToAsm (Common.I64 42)) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmInt (Common.I64 42))))

  describe "astBoolToAsm" $ do
    it "emits Push ImmBool" $ do
      let (_, st) = expectRight (runCM (astBoolToAsm True) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmBool True)))

  describe "astSymbolToAsm" $ do
    it "ScopeGlobal -> LoadGlobal" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeGlobal, 5, "int") }
      let (_, st) = expectRight (runCM (astSymbolToAsm "x") st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadGlobal 5))

    it "ScopeLocal -> LoadLocal" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeLocal, 1, "int") }
      let (_, st) = expectRight (runCM (astSymbolToAsm "x") st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadLocal 1))

    it "ScopeCapture -> LoadCapture" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeCapture, 2, "int") }
      let (_, st) = expectRight (runCM (astSymbolToAsm "x") st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadCapture 2))

    it "Unknown symbol -> Left error" $ do
      let err = expectLeft (runCM (astSymbolToAsm "y") createCompilerState)
      unpack err `shouldBe` "Undefined symbol: y"

  describe "astListToAsm" $ do
    it "non-empty: compiles elements then CallLabel \"list\"" $ do
      let elems = [ABool True, AInteger (Common.I32 1)]
      let (_, st) = expectRight (runCM (astListToAsm mockCompile elems) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 3
      Seq.index code 0 `shouldBe` Real (Push (ImmBool True))
      Seq.index code 1 `shouldBe` Real (Push (ImmInt (Common.I32 1)))
      Seq.index code 2 `shouldBe` Real (BuildList 2)

    it "empty: only CallLabel \"list\"" $ do
      let (_, st) = expectRight (runCM (astListToAsm mockCompile []) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (BuildList 0))

    it "propagates compileFn errors from an element" $ do
      let bad _ = lift (Left "elem error")
      let err = expectLeft (runCM (astListToAsm bad [ABool True]) createCompilerState)
      unpack err `shouldBe` "elem error"

  describe "astCallToAsm" $ do
    it "builtin with args: args compiled before builtin instruction" $ do
      let args = [ABool True, ABool False]
      let (_, st) = expectRight (runCM (astCallToAsm mockCompile (ASymbol "+") args) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 3
      Seq.index code 0 `shouldBe` Real (Push (ImmBool True))
      Seq.index code 1 `shouldBe` Real (Push (ImmBool False))
      Seq.index code 2 `shouldBe` Real Add

    it "builtin with empty args: emits only the instruction" $ do
      let (_, st) = expectRight (runCM (astCallToAsm mockCompile (ASymbol "<=") []) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real Le)

    it "non-builtin: args compiled then CallLabel" $ do
      let args = [AInteger (Common.I32 7)]
      let (_, st) = expectRight (runCM (astCallToAsm mockCompile (ASymbol "myFunc") args) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 2
      Seq.index code 0 `shouldBe` Real (Push (ImmInt (Common.I32 7)))
      Seq.index code 1 `shouldBe` CallLabel "myFunc"

    it "callee not symbol: returns Left" $ do
      let err = expectLeft (runCM (astCallToAsm mockCompile (ABool True) []) createCompilerState)
      unpack err `shouldContain` "Higher calls are not supported yet"

    it "propagates compileFn errors from an argument" $ do
      let bad _ = lift (Left "arg error")
      let err = expectLeft (runCM (astCallToAsm bad (ASymbol "myFunc") [ABool True]) createCompilerState)
      unpack err `shouldBe` "arg error"

  describe "mockCompile (extra coverage)" $ do
    it "covers ASymbol branch" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeGlobal, 9, "int") }
      let (_, st) = expectRight (runCM (mockCompile (ASymbol "x")) st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadGlobal 9))

    it "covers AList branch" $ do
      let ast = AList [AInteger (Common.I32 1), AInteger (Common.I32 2)]
      let (_, st) = expectRight (runCM (mockCompile ast) createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmInt (Common.I32 1)))
          , Real (Push (ImmInt (Common.I32 2)))
          , Real (BuildList 2)
          ]

    it "mockCompile fallback returns Left" $ do
      let err = expectLeft (runCM (mockCompile AVoid) createCompilerState)
      unpack err `shouldContain` "mockCompile unsupported:"
