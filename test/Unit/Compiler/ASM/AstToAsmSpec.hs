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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text, unpack)
import qualified Data.Text as T

import AST.Ast (Ast(..))
import qualified Common.Type.Integer as Common
import qualified Z_old.Src.Type.Integer as Legacy
import Compiler.ASM.AstToAsm
import Compiler.ASM.CompilerMonad (CompilerMonad)
import Compiler.CompilerState (CompilerState(..), createCompilerState)
import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.PsInstruction (PsInstruction(..))
import Data.Maybe (listToMaybe)

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
mockCompile (AInteger n) = astIntToAsm (legacyToInt n)
mockCompile (ABool b)    = astBoolToAsm b
mockCompile (ASymbol s)  = astSymbolToAsm s
mockCompile (AList l)    = astListToAsm mockCompile l

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
spec = describe "Compiler.ASM.AstToAsm (coverage maximale++)" $ do

  describe "Spec helpers coverage" $ do
    it "expectRight returns on Right" $ do
      expectRight (Right (42 :: Int) :: Either Text Int) `shouldBe` 42

    it "expectLeft returns on Left" $ do
      expectLeft (Left ("err" :: Text) :: Either Text Int) `shouldBe` "err"

    it "expectRight throws on Left" $ do
      evaluate (expectRight (Left ("boom" :: Text) :: Either Text Int))
        `shouldThrow` anyErrorCall

    it "expectLeft throws on Right" $ do
      evaluate (expectLeft (Right (123 :: Int) :: Either Text Int))
        `shouldThrow` anyErrorCall

  describe "Builtins table (force evaluation for HPC)" $ do
    it "forces evaluation of builtins pairs" $ do
      let rendered = map (\(n, i) -> (T.unpack n, show i)) builtins
      length rendered `shouldBe` 8
      fmap fst (listToMaybe rendered) `shouldBe` Just "+"
      fmap snd (listToMaybe (reverse rendered)) `shouldBe` Just "Le"

  describe "astIntToAsm" $ do
    it "convertit un Int en instruction PUSH I64" $ do
      let (_, st) = expectRight (runCM (astIntToAsm 42) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmInt (Common.I64 42))))

  describe "astBoolToAsm" $ do
    it "convertit un Bool en instruction PUSH BOOL" $ do
      let (_, st) = expectRight (runCM (astBoolToAsm True) createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real (Push (ImmBool True)))

  describe "astSymbolToAsm" $ do
    it "émet LoadGlobal quand le symbole existe" $ do
      let initial = createCompilerState { csSymbols = Map.singleton "x" 5 }
      let (_, st) = expectRight (runCM (astSymbolToAsm "x") initial)
      csCode st `shouldBe` Seq.singleton (Real (LoadGlobal 5))

    it "renvoie une erreur quand le symbole est inconnu" $ do
      let err = expectLeft (runCM (astSymbolToAsm "y") createCompilerState)
      unpack err `shouldBe` "Undefined symbol: y"

  describe "mockCompile (cover yellow branches)" $ do
    it "mockCompile (ASymbol) compile un symbole existant" $ do
      let initial = createCompilerState { csSymbols = Map.singleton "x" 9 }
      let (_, st) = expectRight (runCM (mockCompile (ASymbol "x")) initial)
      csCode st `shouldBe` Seq.singleton (Real (LoadGlobal 9))

    it "mockCompile (ASymbol) propage l'erreur si symbole inconnu" $ do
      let err = expectLeft (runCM (mockCompile (ASymbol "missing")) createCompilerState)
      unpack err `shouldBe` "Undefined symbol: missing"

    it "mockCompile (AList) délègue à astListToAsm" $ do
      let ast = AList [ABool True, AInteger (Legacy.I32 2)]
      let (_, st) = expectRight (runCM (mockCompile ast) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 3
      Seq.index code 0 `shouldBe` Real (Push (ImmBool True))
      Seq.index code 1 `shouldBe` Real (Push (ImmInt (Common.I64 2)))
      Seq.index code 2 `shouldBe` CallLabel "list"

  describe "astListToAsm" $ do
    it "compile une liste puis émet CallLabel \"list\"" $ do
      let elements = [ABool True, AInteger (Legacy.I32 1)]
      let (_, st) = expectRight (runCM (astListToAsm mockCompile elements) createCompilerState)
      let code = csCode st
      Seq.length code `shouldBe` 3
      Seq.index code 0 `shouldBe` Real (Push (ImmBool True))
      Seq.index code 1 `shouldBe` Real (Push (ImmInt (Common.I64 1)))
      Seq.index code 2 `shouldBe` CallLabel "list"

    it "sur liste vide: émet uniquement CallLabel \"list\"" $ do
      let (_, st) = expectRight (runCM (astListToAsm mockCompile []) createCompilerState)
      csCode st `shouldBe` Seq.singleton (CallLabel "list")

  describe "astCallToAsm (Exhaustivité des builtins)" $ do
    mapM_ (\(name, expectedInst) ->
      it ("builtin " ++ T.unpack name ++ " -> " ++ show expectedInst) $ do
        let (_, st) = expectRight (runCM (astCallToAsm mockCompile (ASymbol name) []) createCompilerState)
        csCode st `shouldBe` Seq.singleton (Real expectedInst)
      ) builtins

  describe "astCallToAsm (Cas non-builtin / erreurs)" $ do
    it "émet un CallLabel pour une fonction utilisateur (non-builtin)" $ do
      let (_, st) = expectRight (runCM (astCallToAsm mockCompile (ASymbol "myFunc") []) createCompilerState)
      csCode st `shouldBe` Seq.singleton (CallLabel "myFunc")

    it "échoue si le callee n'est pas un symbole" $ do
      let err = expectLeft (runCM (astCallToAsm mockCompile (ABool True) []) createCompilerState)
      unpack err `shouldContain` "Higher calls are not supported yet"

  describe "Legacy IntValue coverage" $ do
    it "couvre tous les types d'entiers Legacy" $ do
      legacyToInt (Legacy.I8 1) `shouldBe` 1
      legacyToInt (Legacy.U8 1) `shouldBe` 1
      legacyToInt (Legacy.I16 1) `shouldBe` 1
      legacyToInt (Legacy.U16 1) `shouldBe` 1
      legacyToInt (Legacy.I32 1) `shouldBe` 1
      legacyToInt (Legacy.U32 1) `shouldBe` 1
      legacyToInt (Legacy.I64 1) `shouldBe` 1
      legacyToInt (Legacy.U64 1) `shouldBe` 1
      legacyToInt (Legacy.IChar 'A') `shouldBe` 65
