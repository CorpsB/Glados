{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CompilerStateSpec
-}

module Compiler.CompilerStateSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.List (isInfixOf)
import Control.Monad (forM_)

import Compiler.CompilerState (CompilerState(..), createCompilerState)
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..), Immediate(..))

spec :: Spec
spec = describe "Compiler.CompilerState (source module coverage + deep Show/Eq tests)" $ do
    let empty = createCompilerState

        withNext :: CompilerState
        withNext = createCompilerState { csNextIndex = 1 }
        withLabelCnt :: CompilerState
        withLabelCnt = createCompilerState { csLabelCnt = 5 }

        symA = Map.fromList [(T.pack "x", 0), (T.pack "y", 1)]
        symB = Map.fromList [(T.pack "y", 1), (T.pack "x", 0)]

        withSymbolsA :: CompilerState
        withSymbolsA = createCompilerState { csSymbols = symA }

        withSymbolsB :: CompilerState
        withSymbolsB = createCompilerState { csSymbols = symB }

        codeSeq1 = Seq.fromList [ LabelDef (T.pack "L1")
                                , Real Halt
                                , Real (Push (ImmBool True))
                                ]
        codeSeq2 = Seq.fromList [ LabelDef (T.pack "L1")
                                , Real Halt
                                , Real (Push (ImmBool False))
                                ]

        withCode1 :: CompilerState
        withCode1 = createCompilerState { csCode = codeSeq1 }
        withCode2 :: CompilerState
        withCode2 = createCompilerState { csCode = codeSeq2 }

    it "createCompilerState initializes empty fields (basic smoke tests)" $ do
        csCode createCompilerState `shouldBe` Seq.empty
        csSymbols createCompilerState `shouldBe` Map.empty
        csNextIndex createCompilerState `shouldBe` 0
        csLabelCnt createCompilerState `shouldBe` 0
    it "Show instance produces a readable non-empty string" $
        show createCompilerState `shouldSatisfy` (not . null)

    describe "Eq instance basics" $ do
        it "Eq compares fields (changing one field makes states different)" $ do
            (empty == withNext) `shouldBe` False
            (empty == withLabelCnt) `shouldBe` False
        it "Two identical states are equal (field-by-field)" $ do
            let a = createCompilerState
                b = createCompilerState
            (a == b) `shouldBe` True
        it "Reflexivity / Symmetry / Transitivity examples" $ do
            withNext `shouldBe` withNext
            let p = createCompilerState { csNextIndex = 2 }
                q = createCompilerState { csNextIndex = 2 }
                r = createCompilerState { csNextIndex = 2 }
            p `shouldBe` q
            q `shouldBe` p
            p `shouldBe` q
            q `shouldBe` r
            p `shouldBe` r

    describe "Show instance (deep checks)" $ do
        it "Show output contains the textual label and constructor names for csCode" $ do
            let s = withCode1
            let sh = show s
            sh `shouldSatisfy` ("LabelDef" `isInfixOf`)
            sh `shouldSatisfy` ("L1" `isInfixOf`)
            sh `shouldSatisfy` ("Halt" `isInfixOf`)

        it "Show output reflects csSymbols contents" $ do
            let s = withSymbolsA
            let sh = show s
            sh `shouldSatisfy` ("x" `isInfixOf`)
            sh `shouldSatisfy` ("0" `isInfixOf`)

        it "Show output contains numeric csLabelCnt when non-zero" $ do
            let s = withLabelCnt
            show s `shouldSatisfy` ("5" `isInfixOf`)

    describe "Eq instance (deep checks)" $ do
        it "Two states with same symbol mapping but different insertion order are equal" $ do
            withSymbolsA `shouldBe` withSymbolsB
            withSymbolsB `shouldBe` withSymbolsA
        it "States differing only in csCode are not equal (deep compare of sequences)" $ do
            withCode1 `shouldNotBe` withCode2
        it "States with identical csCode sequences are equal even if constructed separately" $ do
            let a = createCompilerState { csCode = codeSeq1 }
                b = createCompilerState { csCode = Seq.fromList [ LabelDef (T.pack "L1")
                                                                , Real Halt
                                                                , Real (Push (ImmBool True))
                                                                ] }
            a `shouldBe` b
        it "Pairwise inequality for a variety of distinct states" $ do
            let variants = [ empty
                            , withNext
                            , withLabelCnt
                            , withSymbolsA
                            , withCode1
                            ]
                pairs = [ (variants !! i, variants !! j) | i <- [0..length variants - 1], j <- [0..length variants - 1], i < j ]
            forM_ pairs $ \(x,y) -> x `shouldNotBe` y

    describe "Record update invariants" $ do
        it "Record update affects only targeted field (invariant test)" $ do
            let s = createCompilerState { csLabelCnt = 10 }
            csLabelCnt s `shouldBe` 10
            csNextIndex s `shouldBe` 0
            csCode s `shouldBe` Seq.empty
            csSymbols s `shouldBe` Map.empty
        it "Updating csSymbols preserves other fields" $ do
            let s0 = createCompilerState
                s1 = s0 { csSymbols = Map.fromList [(T.pack "z", 42)] }
            csSymbols s1 `shouldBe` Map.fromList [(T.pack "z", 42)]
            csNextIndex s1 `shouldBe` csNextIndex s0
            csLabelCnt s1 `shouldBe` csLabelCnt s0

    describe "Sanity checks for Show/Eq interplay" $ do
        it "if show differs then Eq may differ — simple sanity" $ do
            let sA = createCompilerState { csLabelCnt = 7 }
                sB = createCompilerState { csLabelCnt = 8 }
            show sA `shouldNotBe` show sB
            sA `shouldNotBe` sB
