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
import qualified Common.Type.Integer as Common

import Compiler.ASM.Compiler
  ( compileAst
  , compileIf
  , compileFor
  , compileWhile
  , compileSetVar
  , compileSetStruct
  , compileDefineStruct
  , compileDefineFun
  , compileDefineLambda
  , compileTail
  , compileLoop
  , getLambdaFreeVariables
  )

import Compiler.ASM.CompilerMonad (CompilerMonad, emitInstruction)
import Compiler.CompilerState (CompilerState(..), ScopeType(..), createCompilerState)
import Compiler.Instruction (Instruction(..), Immediate(..))
import Compiler.PsInstruction (PsInstruction(..))

expectRight :: Either e a -> a
expectRight (Right x) = x
expectRight (Left _)  = error "Expected Right, got Left"

expectLeft :: Either a b -> a
expectLeft (Left e)  = e
expectLeft (Right _) = error "Expected Left, got Right"

runCM :: CompilerMonad a -> CompilerState -> Either T.Text (a, CompilerState)
runCM action st = runStateT action st

spec :: Spec
spec = describe "Compiler.ASM.Compiler (max coverage)" $ do

  describe "derived instances (Show / Eq / Ord)" $ do
    it "Instruction / Immediate: Show smoke" $ do
      show (Push (ImmBool True)) `shouldContain` "Push"
      show (ImmInt (Common.I32 1)) `shouldContain` "I32"

    it "Instruction / Immediate: Eq smoke" $ do
      (Push (ImmBool True) == Push (ImmBool True)) `shouldBe` True
      (Push (ImmBool True) == Push (ImmBool False)) `shouldBe` False

    it "Instruction / Immediate: Ord smoke" $ do
      compare Add Sub `shouldBe` LT
      compare (ImmInt (Common.I32 1)) (ImmInt (Common.I32 2)) `shouldBe` LT

    it "PsInstruction: Show / Eq smoke (NO Ord instance)" $ do
      show (LabelDef "x") `shouldContain` "LabelDef"
      (LabelDef "a" == LabelDef "a") `shouldBe` True
      (LabelDef "a" == LabelDef "b") `shouldBe` False
      (Real Nop == Real Nop) `shouldBe` True

    it "Ast: Show / Eq smoke" $ do
      show (ABool True) `shouldContain` "ABool"
      (AImport "Std" == AImport "Std") `shouldBe` True
      (ABool True == ABool False) `shouldBe` False

  describe "helpers" $ do
    it "expectRight throws on Left" $ do
      evaluate (expectRight (Left ("boom" :: T.Text) :: Either T.Text Int))
        `shouldThrow` (\(_ :: SomeException) -> True)

    it "expectLeft throws on Right" $ do
      evaluate (expectLeft (Right (123 :: Int) :: Either T.Text Int))
        `shouldThrow` (\(_ :: SomeException) -> True)

  describe "getLambdaFreeVariables" $ do
    it "covers major constructors and builtin exclusion" $ do
      getLambdaFreeVariables (ASymbol "x") `shouldBe` Set.fromList ["x"]
      getLambdaFreeVariables (ASymbol "+") `shouldBe` Set.empty
      getLambdaFreeVariables (AInteger (Common.I32 132)) `shouldBe` Set.empty
      getLambdaFreeVariables (ABool True) `shouldBe` Set.empty

      let lam = ADefineLambda ["x"] (AList [ASymbol "x", ASymbol "y"])
      getLambdaFreeVariables lam `shouldBe` Set.fromList ["y"]

      let astVar = ASetVar "x" "Int" (AList [ASymbol "x", ASymbol "y"])
      getLambdaFreeVariables astVar `shouldBe` Set.fromList ["y"]

      let callAst = ACall (ASymbol "+") [ASymbol "x", AInteger (Common.I32 2)]
      getLambdaFreeVariables callAst `shouldBe` Set.fromList ["x"]

      let ifAst = AIf (ASymbol "c") (ASymbol "t") (AList [ASymbol "e"])
      getLambdaFreeVariables ifAst `shouldBe` Set.fromList ["c","t","e"]

      let lst = AList [ASymbol "a", AList [ASymbol "b"], ABool False]
      getLambdaFreeVariables lst `shouldBe` Set.fromList ["a","b"]

      getLambdaFreeVariables (AImport "Std") `shouldBe` Set.empty

  describe "compileLoop" $ do
    it "emits condition, JumpIfFalseLabel to end, then body" $ do
      let action = compileLoop compileAst (ABool True) (ABool False) "END"
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , JumpIfFalseLabel "END"
          , Real (Push (ImmBool False))
          ]

    it "propagates compileFn errors" $ do
      let badCompile _ = lift (Left "boom")
      let err = expectLeft (runCM (compileLoop badCompile (ABool True) (ABool True) "E") createCompilerState)
      err `shouldBe` "boom"

  describe "compileWhile" $ do
    it "emits start label, loop body, jump back, end label" $ do
      let action = compileWhile compileAst (ABool True) (ABool False)
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ LabelDef "while_start_0"
          , Real (Push (ImmBool True))
          , JumpIfFalseLabel "while_end_1"
          , Real (Push (ImmBool False))
          , JumpLabel "while_start_0"
          , LabelDef "while_end_1"
          ]
      csLabelCnt st `shouldBe` 2

  describe "compileFor" $ do
    it "emits init, loop, update, jump back, end label" $ do
      let action = compileFor compileAst (ABool True) (ABool False) (ABool False) (ABool True)
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , LabelDef "for_start_0"
          , Real (Push (ImmBool False))
          , JumpIfFalseLabel "for_end_1"
          , Real (Push (ImmBool True))
          , Real (Push (ImmBool False))
          , JumpLabel "for_start_0"
          , LabelDef "for_end_1"
          ]
      csLabelCnt st `shouldBe` 2

  describe "compileIf" $ do
    it "emits expected control-flow sequence and labels" $ do
      let action = compileIf compileAst (ABool True) (ABool False) (ABool True)
      let (_, st) = expectRight (runCM action createCompilerState)
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
      let (_, st) = expectRight (runCM (compileSetVar compileAst "x" "int" (AInteger (Common.I32 3))) createCompilerState)
      Map.lookup "x" (csSymbols st) `shouldBe` Just (ScopeGlobal, 0, "int")
      csNextIndex st `shouldBe` 1
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmInt (Common.I32 3)))
          , Real (StoreGlobal 0)
          ]

  describe "compileDefineStruct / compileSetStruct" $ do
    it "compileDefineStruct registers struct definition only" $ do
      let action = compileDefineStruct "Point" [("x", "int"), ("y", "int")]
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe` Seq.empty
      Map.lookup "Point" (csStructs st) `shouldBe` Just [("x", "int"), ("y", "int")]

    it "compileSetStruct emits fields in struct order then BuildStruct" $ do
      let st0 = createCompilerState { csStructs = Map.singleton "Point" [("x", "int"), ("y", "int")] }
      let action = compileSetStruct compileAst "Point"
            [ ("y", AInteger (Common.I32 2))
            , ("x", AInteger (Common.I32 1))
            ]
      let (_, st) = expectRight (runCM action st0)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmInt (Common.I32 1)))
          , Real (Push (ImmInt (Common.I32 2)))
          , Real (BuildStruct 2)
          ]

    it "compileSetStruct fails when a required field is missing" $ do
      let st0 = createCompilerState { csStructs = Map.singleton "Point" [("x", "int"), ("y", "int")] }
      let err = expectLeft (runCM (compileSetStruct compileAst "Point" [("x", ABool True)]) st0)
      T.unpack err `shouldContain` "Missing field"

    it "compileSetStruct fails when struct is undefined" $ do
      let err = expectLeft (runCM (compileSetStruct compileAst "Nope" [("x", ABool True)]) createCompilerState)
      T.unpack err `shouldContain` "Undefined struct"

  describe "compileDefineFun" $ do
    it "compiles isolated function; code goes to csFuncs, outer csCode preserved" $ do
      let action = do
            emitInstruction Nop
            compileDefineFun compileAst "foo" [("x", "int"), ("y", "int")] (ASymbol "x")
            emitInstruction Halt
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe` Seq.fromList [Real Nop, Real Halt]
      csFuncs st `shouldBe`
        Seq.fromList
          [ LabelDef "fun_foo"
          , Real (LoadLocal (-2))
          , Real Ret
          ]
      csLabelCnt st `shouldBe` 0

  describe "compileDefineLambda" $ do
    it "multi-capture: loads captures, builds closure, lambda loads captures then Ret" $ do
      let st0 =
            createCompilerState
              { csSymbols =
                  Map.fromList
                    [ ("a", (ScopeGlobal, 2, "int"))
                    , ("b", (ScopeGlobal, 7, "int"))
                    ]
              }
      let body = AList [ASymbol "a", ASymbol "b"]
      let (_, st) = expectRight (runCM (compileDefineLambda compileAst ["x"] body) st0)
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
          , Real Ret
          ]
      csLabelCnt st `shouldBe` 1

    it "no-capture lambda: builds closure with 0 captures and uses LoadLocal for args" $ do
      let (_, st) = expectRight (runCM (compileDefineLambda compileAst ["x"] (ASymbol "x")) createCompilerState)
      csCode st `shouldBe` Seq.fromList [MakeClosureLabel "lambda_0" 0]
      csFuncs st `shouldBe` Seq.fromList [LabelDef "lambda_0", Real (LoadLocal (-1)), Real Ret]
      csLabelCnt st `shouldBe` 1

    it "fails if a capture is undefined" $ do
      let err = expectLeft (runCM (compileDefineLambda compileAst ["x"] (ASymbol "y")) createCompilerState)
      T.unpack err `shouldContain` "Undefined symbol: \"y\""

  describe "compileTail" $ do
    it "builtin call in tail position emits builtin instruction then Ret" $ do
      let action = compileTail compileAst (ACall (ASymbol "+") [AInteger (Common.I32 1), AInteger (Common.I32 2)])
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmInt (Common.I32 1)))
          , Real (Push (ImmInt (Common.I32 2)))
          , Real Add
          , Real Ret
          ]

    it "non-builtin call in tail position emits TailCallLabel" $ do
      let action = compileTail compileAst (ACall (ASymbol "foo") [ABool True])
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , TailCallLabel "foo"
          ]

    it "indirect call in tail position emits CallIndirect then Ret" $ do
      let action = compileTail compileAst (ACall (ABool True) [ABool False])
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , Real (Push (ImmBool False))
          , Real CallIndirect
          , Real Ret
          ]

    it "tail If uses compileTail recursively on branches" $ do
      let action = compileTail compileAst (AIf (ABool True) (ABool True) (ABool False))
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , JumpIfFalseLabel "else_0"
          , Real (Push (ImmBool True))
          , Real Ret
          , JumpLabel "endif_1"
          , LabelDef "else_0"
          , Real (Push (ImmBool False))
          , Real Ret
          , LabelDef "endif_1"
          ]
      csLabelCnt st `shouldBe` 2

    it "tail List compiles init expressions then tail-compiles last" $ do
      let action = compileTail compileAst (AList [ABool True, ABool False])
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmBool True))
          , Real (Push (ImmBool False))
          , Real Ret
          ]

    it "tail empty List emits Ret" $ do
      let action = compileTail compileAst (AList [])
      let (_, st) = expectRight (runCM action createCompilerState)
      csCode st `shouldBe` Seq.singleton (Real Ret)

  describe "compileAst" $ do
    it "AReturn compiles expr then Ret" $ do
      let (_, st) = expectRight (runCM (compileAst (AReturn (ABool True))) createCompilerState)
      csCode st `shouldBe` Seq.fromList [Real (Push (ImmBool True)), Real Ret]

    it "AVoid produces no code" $ do
      let (_, st) = expectRight (runCM (compileAst AVoid) createCompilerState)
      csCode st `shouldBe` Seq.empty

    it "AImport produces no code" $ do
      let (_, st) = expectRight (runCM (compileAst (AImport "Std")) createCompilerState)
      csCode st `shouldBe` Seq.empty

    it "ACall delegates to astCallToAsm (hits compileAst branch)" $ do
      let ast = ACall (ASymbol "+") [AInteger (Common.I32 1), AInteger (Common.I32 2)]
      let (_, st) = expectRight (runCM (compileAst ast) createCompilerState)
      csCode st `shouldBe`
        Seq.fromList
          [ Real (Push (ImmInt (Common.I32 1)))
          , Real (Push (ImmInt (Common.I32 2)))
          , Real Add
          ]
