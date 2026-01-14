{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ResolveLabelsSpec.hs
-}

module Compiler.ResolveLabels.ResolveLabelsSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.ResolveLabels (resolveLabels)

-- Rappel des tailles pour comprendre les calculs :
-- Jump/Call/JIf = 5 octets
-- MakeClosure   = 9 octets (1 opcode + 4 addr + 4 count)
-- Nop           = 1 octet

spec :: Spec
spec = describe "resolveLabels Coverage" $ do

    it "resolves [LabelDef L, JumpLabel L] -> Jump -5 (Relative)" $ do
        let pseudos = [LabelDef (T.pack "L"), JumpLabel (T.pack "L")]
        resolveLabels pseudos `shouldBe` Right [Jump (-5)]

    it "resolves [JumpLabel L, LabelDef L, Real Nop] -> Jump 0" $ do
        let pseudos = [JumpLabel (T.pack "L"), LabelDef (T.pack "L"), Real Nop]
        resolveLabels pseudos `shouldBe` Right [Jump 0, Nop]

    it "resolves [LabelDef L, JumpIfFalseLabel L]" $ do
        let pseudos = [LabelDef (T.pack "L"), JumpIfFalseLabel (T.pack "L")]
        resolveLabels pseudos `shouldBe` Right [JumpIfFalse (-5)]

    it "resolves [LabelDef L, JumpIfTrueLabel L]" $ do
        let pseudos = [LabelDef (T.pack "L"), JumpIfTrueLabel (T.pack "L")]
        resolveLabels pseudos `shouldBe` Right [JumpIfTrue (-5)]

    it "resolves [LabelDef F, CallLabel F]" $ do
        let pseudos = [LabelDef (T.pack "F"), CallLabel (T.pack "F")]
        resolveLabels pseudos `shouldBe` Right [Call (-5)]

    it "resolves [LabelDef F, TailCallLabel F]" $ do
        let pseudos = [LabelDef (T.pack "F"), TailCallLabel (T.pack "F")]
        resolveLabels pseudos `shouldBe` Right [TailCall (-5)]

    it "resolves MakeClosureLabel (Relative Offset)" $ do
        let pseudos = [Real Nop, LabelDef (T.pack "C"), MakeClosureLabel (T.pack "C") 2]
        resolveLabels pseudos `shouldBe` Right [Nop, MakeClosure (-9) 2]

    it "resolves GetFuncAddrLabel" $ do
        let pseudos = [LabelDef (T.pack "F"), GetFuncAddrLabel (T.pack "F")]
        resolveLabels pseudos `shouldBe` Right [GetFuncAddr (-5)]

    it "resolves mixed Real instructions and Labels" $ do
        let pseudos = [Real Nop, LabelDef (T.pack "A"), Real Nop, JumpLabel (T.pack "A")]
        resolveLabels pseudos `shouldBe` Right [Nop, Nop, Jump (-6)]

    it "returns error for duplicate labels" $ do
        let pseudos = [LabelDef (T.pack "A"), LabelDef (T.pack "A")]
        resolveLabels pseudos `shouldBe` Left (T.pack "Duplicate label: A")

    it "returns error for unknown label" $ do
        let pseudos = [JumpLabel (T.pack "Z")]
        resolveLabels pseudos `shouldSatisfy` (\x -> case x of Left _ -> True; _ -> False)
    
    it "forces evaluation of all pseudo sizes by placing them before a target label" $ do
        let pseudos =
                [ JumpIfFalseLabel (T.pack "End")
                , JumpIfTrueLabel (T.pack "End")
                , CallLabel (T.pack "End")
                , TailCallLabel (T.pack "End")
                , MakeClosureLabel (T.pack "End") 0
                , GetFuncAddrLabel (T.pack "End")
                , LabelDef (T.pack "End")
                , JumpLabel (T.pack "End")
                ]
        resolveLabels pseudos `shouldBe` Right
                [ JumpIfFalse 29
                , JumpIfTrue 24
                , Call 19
                , TailCall 14
                , MakeClosure 5 0
                , GetFuncAddr 0
                , Jump (-5)
                ]
