{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Struct instructions unit tests (compilable)
-}

{-# LANGUAGE OverloadedStrings #-}

module VM.Instruction.StructSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, get)
import Control.Exception (ErrorCall(..))
import Data.List (isInfixOf)
import qualified Data.Vector as V
import qualified Data.ByteString as BS

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import VM.VMStack (stackPush, stackPop)
import Common.Type.Integer (IntValue(..))

import VM.Instruction.Struct
  ( instAttrUpdate
  , pushStruct
  )

initState :: VMState
initState = createVMState BS.empty False

i32 :: Int -> VMValue
i32 n = VInt (I32 (fromIntegral n))

listV :: [VMValue] -> V.Vector VMValue
listV = V.fromList

runVM :: VirtualMachine a -> IO (a, VMState)
runVM act = runStateT act initState

shouldThrowMsg :: IO a -> String -> Expectation
shouldThrowMsg io expected =
  io `shouldThrow` \e ->
    case e of
      ErrorCallWithLocation msg _ -> expected `isInfixOf` msg
      ErrorCall msg               -> expected `isInfixOf` msg

spec :: Spec
spec = describe "VM.Instruction.Struct (compilable tests)" $ do

  describe "pushStruct" $ do
    it "wraps last N stack values into VStruct and removes them from stack" $ do
      (out, st) <- runVM $ do
        stackPush (i32 1)
        stackPush (i32 2)
        stackPush (i32 3)
        vm <- get
        pushStruct vm 2
        stackPop

      out `shouldBe` VStruct (listV [i32 2, i32 3])
      V.length (vStack st) `shouldBe` 1
      (vStack st V.! 0) `shouldBe` i32 1

  describe "instAttrUpdate" $ do
    it "updates a struct field at valid index (success)" $ do
      (out, _st) <- runVM $ do
        stackPush (VStruct (listV [i32 1, i32 2, i32 3]))
        stackPush (i32 1)
        stackPush (i32 99)
        instAttrUpdate
        stackPop

      out `shouldBe` VStruct (listV [i32 1, i32 99, i32 3])

    it "throws on out-of-bounds update index" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (VStruct (listV [i32 1])) >> stackPush (i32 5) >> stackPush (i32 9) >> instAttrUpdate))
        "attr_update OOB"

    it "throws if struct is not a VStruct (message includes show other)" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (i32 123) >> stackPush (i32 0) >> stackPush (i32 9) >> instAttrUpdate))
        "attr_update not struct"

    it "throws if idx is not an integer (falls into not struct branch)" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (VStruct (listV [i32 1])) >> stackPush (VStruct V.empty) >> stackPush (i32 9) >> instAttrUpdate))
        "attr_update not struct"
