{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ResolveLabelsSpec
-}

module Compiler.ResolveLabelsSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..))
import Compiler.PsInstruction (PsInstruction(..))
import Compiler.ResolveLabels (resolveLabels)

spec :: Spec
spec = do
  describe "resolveLabels" $ do
    it "resolves a simple sequence with a label and jump" $ do
      let pseudos = [LabelDef (T.pack "L"), JumpLabel (T.pack "L")]
      let res = resolveLabels pseudos
      res `shouldSatisfy` (\x -> case x of Right _ -> True; _ -> False)
    it "returns error for unknown label" $ do
      let pseudos = [JumpLabel (T.pack "X")]
      let res = resolveLabels pseudos
      res `shouldSatisfy` (\x -> case x of Left _ -> True; _ -> False)