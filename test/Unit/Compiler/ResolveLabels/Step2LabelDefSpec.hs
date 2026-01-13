{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2LabelDefSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ResolveLabels.Step2LabelDefSpec (spec) where

import Test.Hspec
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.Step2LabelDef (step2LabelDef)

spec :: Spec
spec = describe "Step2LabelDef" $ do

    it "acts as identity: preserves instructions and index unchanged" $ do
        -- Scénario : On a déjà accumulé des instructions [Nop] et on est à l'index 10
        let currentOut = [Nop, Halt]
        let currentIdx = 10
        
        -- LabelDef ne doit RIEN changer (pas d'ajout d'instruction, pas d'incrément d'index)
        step2LabelDef currentOut currentIdx
            `shouldBe` Right (currentOut, currentIdx)

    it "works with initial empty state" $ do
        step2LabelDef [] 0 `shouldBe` Right ([], 0)
