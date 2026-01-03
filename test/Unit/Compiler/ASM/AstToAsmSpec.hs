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
import Data.Maybe (listToMaybe)

import AST.Ast (Ast(..))
import qualified Common.Type.Integer as Common
import qualified Z_old.Src.Type.Integer as Legacy

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

mockCompile :: Ast -> CompilerMonad ()
mockCompile (AInteger n)   = astIntToAsm (legacyToInt n)
mockCompile (ABool b)      = astBoolToAsm b
mockCompile (ASymbol s)    = astSymbolToAsm s
mockCompile (AList l)      = astListToAsm mockCompile l
mockCompile (ACall f args) = astCallToAsm mockCompile f args
mockCompile other          = lift (Left ("mockCompile unsupported: " <> T.pack (show other)))

builtins :: [(Text, Instruction)]
builtins =
  [ ("+", Add)
  , ("-", Sub)
  , ("*", Mul)
  , ("div", Div)
  , ("mod", Mod)
  , ("==", Eq)
  , ("<", Lt)
  , ("<=", Le)
  ]

spec :: Spec
spec = describe "Compiler.ASM.AstToAsm (max coverage)" $ do

  describe "helpers (expectRight/expectLeft)" $ do
    it "expectRight ok" $
      expectRight (Right (42 :: Int) :: Either Text Int) `shouldBe` 42

    it "expectLeft ok" $
      expectLeft (Left ("err" :: Text) :: Either Text Int) `shouldBe` "err"

    it "expectRight throws on Left" $
      evaluate (expectRight (Left ("boom" :: Text) :: Either Text Int))
        `shouldThrow` anyErrorCall

    it "expectLeft throws on Right" $
      evaluate (expectLeft (Right (123 :: Int) :: Either Text Int))
        `shouldThrow` anyErrorCall

  describe "builtins (force evaluation)" $ do
    it "touches all pairs" $ do
      length builtins `shouldBe` 8
      let rendered = map (\(n, i) -> (T.unpack n, show i)) builtins
      fmap fst (listToMaybe rendered) `shouldBe` Just "+"
      fmap snd (listToMaybe (reverse rendered)) `shouldBe` Just "Le"

  describe "legacyToInt (all constructors)" $ do
    it "covers every Legacy.IntValue constructor used here" $ do
      legacyToInt (Legacy.I8 1) `shouldBe` 1
      legacyToInt (Legacy.U8 1) `shouldBe` 1
      legacyToInt (Legacy.I16 1) `shouldBe` 1
      legacyToInt (Legacy.U16 1) `shouldBe` 1
      legacyToInt (Legacy.I32 1) `shouldBe` 1
      legacyToInt (Legacy.U32 1) `shouldBe` 1
      legacyToInt (Legacy.I64 1) `shouldBe` 1
      legacyToInt (Legacy.U64 1) `shouldBe` 1
      legacyToInt (Legacy.IChar 'A') `shouldBe` 65

  describe "astIntToAsm" $ do
    it "emits Push ImmInt(I64)" $ do
      let (_, st) = expectRight (runCM (astIntToAsm 42) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmInt (Common.I64 42))))

  describe "astBoolToAsm" $ do
    it "emits Push ImmBool" $ do
      let (_, st) = expectRight (runCM (astBoolToAsm True) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmBool True)))

  describe "astSymbolToAsm (all ScopeType branches + error)" $ do
    it "ScopeGlobal -> LoadGlobal" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeGlobal, 5) }
      let (_, st) = expectRight (runCM (astSymbolToAsm "x") st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadGlobal 5))

    it "ScopeLocal -> LoadLocal" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeLocal, 1) }
      let (_, st) = expectRight (runCM (astSymbolToAsm "x") st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadLocal 1))

    it "ScopeCapture -> LoadCapture" $ do
      let st0 = createCompilerState { csSymbols = Map.singleton "x" (ScopeCapture, 2) }
      let (_, st) = expectRight (runCM (astSymbolToAsm "x") st0)
      csCode st `shouldBe` Seq.singleton (Real (LoadCapture 2))

    it "Unknown symbol -> Left error" $ do
      let err = expectLeft (runCM (astSymbolToAsm "y") createCompilerState)
      unpack err `shouldBe` "Undefined symbol: y"

  describe "astListToAsm" $ do
    it "non-empty: compiles elements then CallLabel \"list\"" $ do
      let elems = [ABool True, AInteger (Legacy.I32 1)]
      let (_, st) = expectRight (runCM (astListToAsm mockCompile elems) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 3
      Seq.index code 0 `shouldBe` Real (Push (ImmBool True))
      Seq.index code 1 `shouldBe` Real (Push (ImmInt (Common.I64 1)))
      Seq.index code 2 `shouldBe` CallLabel "list"

    it "empty: only CallLabel \"list\"" $ do
      let (_, st) = expectRight (runCM (astListToAsm mockCompile []) createCompilerState)
      csCode st `shouldBe` Seq.singleton (CallLabel "list")

  describe "astCallToAsm (builtin / non-builtin / error + arg loop)" $ do
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
      let args = [AInteger (Legacy.I32 7)]
      let (_, st) = expectRight (runCM (astCallToAsm mockCompile (ASymbol "myFunc") args) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 2
      Seq.index code 0 `shouldBe` Real (Push (ImmInt (Common.I64 7)))
      Seq.index code 1 `shouldBe` CallLabel "myFunc"

    it "callee not symbol: returns Left" $ do
      let err = expectLeft (runCM (astCallToAsm mockCompile (ABool True) []) createCompilerState)
      unpack err `shouldContain` "Higher calls are not supported yet"

    it "touches all builtin entries at least once (Map table exercised)" $ do
      mapM_ (\(name, expectedInst) -> do
        let (_, st) = expectRight (runCM (astCallToAsm mockCompile (ASymbol name) []) createCompilerState)
        csCode st `shouldBe` Seq.singleton (Real expectedInst)
        ) builtins

  describe "mockCompile (cover every branch incl. fallback)" $ do
    it "mockCompile ACall path works end-to-end" $ do
      let ast = ACall (ASymbol "+") [AInteger (Legacy.I32 1), AInteger (Legacy.I32 2)]
      let (_, st) = expectRight (runCM (mockCompile ast) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 3
      Seq.index code 0 `shouldBe` Real (Push (ImmInt (Common.I64 1)))
      Seq.index code 1 `shouldBe` Real (Push (ImmInt (Common.I64 2)))
      Seq.index code 2 `shouldBe` Real Add

    it "mockCompile fallback returns Left" $ do
      let err = expectLeft (runCM (mockCompile AVoid) createCompilerState)
      unpack err `shouldContain` "mockCompile unsupported:"
