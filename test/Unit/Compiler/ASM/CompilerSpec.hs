{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ASM.CompilerSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import AST.Ast (Ast(..))
import qualified Common.Type.Integer as Common
import Compiler.ASM.Compiler (compileIf, compileDefine)
import Compiler.ASM.CompilerMonad (CompilerMonad, defineSymbol, emitInstruction)
import Compiler.CompilerState (CompilerState(..), createCompilerState)
import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.PsInstruction (PsInstruction(..))
import qualified Z_old.Src.Type.Integer as Legacy

expectRight :: Either e a -> a
expectRight (Right x) = x
expectRight (Left _)  = error "Expected Right, got Left"

expectLeft :: Either e a -> e
expectLeft (Left e)  = e
expectLeft (Right _) = error "Expected Left, got Right"

execCompiler :: CompilerMonad a -> Either String CompilerState
execCompiler action =
  case runStateT action createCompilerState of
    Left err     -> Left (T.unpack err)
    Right (_, s) -> Right s

compileLiteral :: Ast -> CompilerMonad ()
compileLiteral (AInteger v) = emitInstruction (Push (ImmInt (convertInt v)))
compileLiteral (ABool b)    = emitInstruction (Push (ImmBool b))
compileLiteral _            = emitInstruction Nop

-- ✅ FIX ICI : UI8/UI16/UI32/UI64 (pas U8/U16/U32/U64)
convertInt :: Legacy.IntValue -> Common.IntValue
convertInt (Legacy.I8 n)    = Common.I8 n
convertInt (Legacy.I16 n)   = Common.I16 n
convertInt (Legacy.I32 n)   = Common.I32 n
convertInt (Legacy.I64 n)   = Common.I64 n
convertInt (Legacy.U8 n)    = Common.UI8 n
convertInt (Legacy.U16 n)   = Common.UI16 n
convertInt (Legacy.U32 n)   = Common.UI32 n
convertInt (Legacy.U64 n)   = Common.UI64 n
convertInt (Legacy.IChar c) = Common.I32 (fromIntegral (fromEnum c))

spec :: Spec
spec = describe "Compiler.ASM.Compiler (coverage maximale)" $ do

  describe "Spec helpers coverage" $ do
    it "expectRight throws on Left" $ do
      evaluate (expectRight (Left ("boom" :: String) :: Either String Int))
        `shouldThrow` anyErrorCall

    it "expectLeft throws on Right" $ do
      evaluate (expectLeft (Right (123 :: Int) :: Either String Int))
        `shouldThrow` anyErrorCall

  describe "execCompiler coverage" $ do
    it "covers Right branch" $ do
      let st = expectRight (execCompiler (return ()))
      Map.size (csSymbols st) `shouldBe` 0

    it "covers Left branch" $ do
      let err = expectLeft (execCompiler (lift (Left "Manual Error")))
      err `shouldBe` "Manual Error"

  describe "Internal Helpers Coverage" $ do
    it "convertInt couvre tous les types Legacy" $ do
      convertInt (Legacy.I8 1) `shouldBe` Common.I8 1
      convertInt (Legacy.I16 1) `shouldBe` Common.I16 1
      convertInt (Legacy.I32 1) `shouldBe` Common.I32 1
      convertInt (Legacy.I64 1) `shouldBe` Common.I64 1
      convertInt (Legacy.U8 1) `shouldBe` Common.UI8 1
      convertInt (Legacy.U16 1) `shouldBe` Common.UI16 1
      convertInt (Legacy.U32 1) `shouldBe` Common.UI32 1
      convertInt (Legacy.U64 1) `shouldBe` Common.UI64 1
      convertInt (Legacy.IChar 'A') `shouldBe` Common.I32 65

    it "compileLiteral émet Nop pour AST non supporté" $ do
      let st = expectRight (execCompiler (compileLiteral (ASymbol "unsupported")))
      csCode st `shouldBe` Seq.singleton (Real Nop)

  describe "Symbol definition" $ do
    it "defineSymbol ajoute des symboles et incrémente les indices" $ do
      let action = do
            _ <- defineSymbol "var1"
            _ <- defineSymbol "var2"
            return ()
      let st = expectRight (execCompiler action)
      Map.lookup "var1" (csSymbols st) `shouldBe` Just 0
      Map.lookup "var2" (csSymbols st) `shouldBe` Just 1
      csNextIndex st `shouldBe` 2

    it "defineSymbol échoue si déjà défini" $ do
      let action = do
            _ <- defineSymbol "var1"
            _ <- defineSymbol "var1"
            return ()
      let err = expectLeft (execCompiler action)
      err `shouldBe` "Symbol already defined: var1"

  describe "High-level compilation helpers" $ do
    it "compileDefine stocke la valeur et incrémente l'état global" $ do
      let action = do
            compileDefine compileLiteral "x" (AInteger (Legacy.I32 7))
            compileDefine compileLiteral "y" (ABool True)
      let st = expectRight (execCompiler action)

      csSymbols st `shouldBe` Map.fromList [("x", 0), ("y", 1)]
      csNextIndex st `shouldBe` 2

      csCode st `shouldBe` Seq.fromList
        [ Real (Push (ImmInt (Common.I32 7)))
        , Real (StoreGlobal 0)
        , Real (Push (ImmBool True))
        , Real (StoreGlobal 1)
        ]

    it "compileDefine échoue si on redéfinit le même symbole" $ do
      let action = do
            compileDefine compileLiteral "x" (AInteger (Legacy.I32 1))
            compileDefine compileLiteral "x" (AInteger (Legacy.I32 2))
      let err = expectLeft (execCompiler action)
      err `shouldBe` "Symbol already defined: x"

    it "compileIf génère des labels uniques sur appels multiples" $ do
      let action = do
            compileIf compileLiteral (ABool True)  (ABool True)  (ABool False)
            compileIf compileLiteral (ABool False) (ABool False) (ABool True)
      let st = expectRight (execCompiler action)

      csLabelCnt st `shouldBe` 4
      let code = csCode st
      LabelDef "else_0"  `elem` code `shouldBe` True
      LabelDef "endif_1" `elem` code `shouldBe` True
      LabelDef "else_2"  `elem` code `shouldBe` True
      LabelDef "endif_3" `elem` code `shouldBe` True

    it "compileIf produit le flux attendu pour un seul appel" $ do
      let st = expectRight (execCompiler (compileIf compileLiteral (ABool True)
                                            (AInteger (Legacy.I32 1))
                                            (AInteger (Legacy.I32 0))))
      csCode st `shouldBe` Seq.fromList
        [ Real (Push (ImmBool True))
        , JumpIfFalseLabel "else_0"
        , Real (Push (ImmInt (Common.I32 1)))
        , JumpLabel "endif_1"
        , LabelDef "else_0"
        , Real (Push (ImmInt (Common.I32 0)))
        , LabelDef "endif_1"
        ]
