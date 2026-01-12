{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- CallSnapshotSpec
-}

module VM.CallSnapshotSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)
import qualified Data.Vector as V
import Control.Exception (evaluate)

import VM.CallSnapshot (CallSnapshot(..))
import VM.VMValue (VMValue(..))
import qualified Common.Type.Integer as Common

-- helper: build a VM integer without depending on I64/I32 constructors
i :: Int -> VMValue
i n = VInt (Common.fromInt64 (fromIntegral n))

spec :: Spec
spec = describe "VM.CallSnapshot" $ do

  describe "record fields" $ do
    it "stores and exposes callbackIndex, vStackIndex and vEnv" $ do
      let env0 = V.fromList [i 1, VBool True]
      let snap = CallSnapshot { callbackIndex = 123, vStackIndex = 456, vEnv = env0 }

      callbackIndex snap `shouldBe` 123
      vStackIndex snap `shouldBe` 456
      vEnv snap `shouldBe` env0

  describe "deriving (Eq)" $ do
    it "considers identical snapshots equal" $ do
      let env0 = V.fromList [i 1, VBool True]
      CallSnapshot 1 2 env0 `shouldBe` CallSnapshot 1 2 env0

    it "considers different snapshots not equal (different callbackIndex)" $ do
      let env0 = V.fromList [i 1]
      CallSnapshot 1 2 env0 `shouldNotBe` CallSnapshot 9 2 env0

    it "considers different snapshots not equal (different vStackIndex)" $ do
      let env0 = V.fromList [i 1]
      CallSnapshot 1 2 env0 `shouldNotBe` CallSnapshot 1 99 env0

    it "considers different snapshots not equal (different vEnv)" $ do
      CallSnapshot 1 2 (V.fromList [i 1]) `shouldNotBe`
        CallSnapshot 1 2 (V.fromList [i 2])

  describe "deriving (Show)" $ do
    it "forces the Show instance and contains key fields" $ do
      let snap = CallSnapshot
            { callbackIndex = 123
            , vStackIndex = 456
            , vEnv = V.fromList [i 1, VBool True]
            }

      let s = show snap

      -- IMPORTANT: force the WHOLE string (no deepseq needed)
      _ <- evaluate (length s)

      s `shouldSatisfy` isInfixOf "CallSnapshot"
      s `shouldSatisfy` isInfixOf "callbackIndex = 123"
      s `shouldSatisfy` isInfixOf "vStackIndex = 456"
      s `shouldSatisfy` isInfixOf "vEnv ="
      s `shouldSatisfy` isInfixOf "VBool True"
