{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- List instructions unit tests (compilable)
-}

{-# LANGUAGE OverloadedStrings #-}

module VM.Instruction.ListSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT)
import Control.Exception (ErrorCall(..))
import Data.List (isInfixOf)
import qualified Data.Vector as V
import qualified Data.ByteString as BS

import VM.VMState (VirtualMachine, VMState(..), createVMState)
import VM.VMValue (VMValue(..))
import VM.VMStack (stackPop, stackPush)
import Common.Type.Integer (IntValue(..))

import VM.Instruction.List
  ( instCons
  , instHead
  , instTail
  , instNth
  , instNthUpdate
  )

initState :: VMState
initState = createVMState BS.empty False

i32 :: Int -> VMValue
i32 n = VInt (I32 (fromIntegral n))

listOf :: [VMValue] -> VMValue
listOf xs = VList (V.fromList xs)

runVM :: VirtualMachine a -> IO (a, VMState)
runVM act = runStateT act initState

shouldThrowMsg :: IO a -> String -> Expectation
shouldThrowMsg io expected =
  io `shouldThrow` \e ->
    case e of
      ErrorCallWithLocation msg _ -> expected `isInfixOf` msg
      ErrorCall msg               -> expected `isInfixOf` msg

spec :: Spec
spec = describe "VM.Instruction.List (compilable tests)" $ do

  describe "instCons" $ do
    it "prepends element to list (nominal)" $ do
      (out, _st) <- runVM $ do
        stackPush (i32 42)
        stackPush (listOf [i32 1, i32 2])
        instCons
        stackPop
      out `shouldBe` listOf [i32 42, i32 1, i32 2]

    it "throws if second argument is not a list" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (i32 10) >> stackPush (i32 20) >> instCons))
        "CONS expects a List"

  describe "instHead" $ do
    it "returns first element (nominal)" $ do
      (out, _st) <- runVM $ do
        stackPush (listOf [i32 7, i32 8])
        instHead
        stackPop
      out `shouldBe` i32 7

    it "throws on empty list" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (listOf []) >> instHead))
        "HEAD called on empty list"

    it "throws if argument is not a list" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (i32 1) >> instHead))
        "HEAD expects a List"

  describe "instTail" $ do
    it "returns tail (nominal)" $ do
      (out, _st) <- runVM $ do
        stackPush (listOf [i32 7, i32 8, i32 9])
        instTail
        stackPop
      out `shouldBe` listOf [i32 8, i32 9]

    it "throws on empty list" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (listOf []) >> instTail))
        "TAIL called on empty list"

    it "throws if argument is not a list" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (i32 1) >> instTail))
        "TAIL expects a List"

  describe "instNth" $ do
    it "returns element at index (nominal)" $ do
      (out, _st) <- runVM $ do
        stackPush (listOf [i32 10, i32 11, i32 12])
        stackPush (i32 1)
        instNth
        stackPop
      out `shouldBe` i32 11

    it "throws on negative index" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (listOf [i32 1]) >> stackPush (i32 (-1)) >> instNth))
        "Nth OOB"

    it "throws on index >= length" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (listOf [i32 1]) >> stackPush (i32 5) >> instNth))
        "Nth OOB"

    it "throws if list is not a list" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (i32 123) >> stackPush (i32 0) >> instNth))
        "Nth expects a List"

  describe "instNthUpdate" $ do
    it "updates value at index (nominal)" $ do
      (out, _st) <- runVM $ do
        stackPush (listOf [i32 1, i32 2, i32 3])
        stackPush (i32 1)
        stackPush (i32 99)
        instNthUpdate
        stackPop
      out `shouldBe` listOf [i32 1, i32 99, i32 3]

    it "throws on negative index" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (listOf [i32 1]) >> stackPush (i32 (-1)) >> stackPush (i32 9) >> instNthUpdate))
        "NthUpdate OOB"

    it "throws on index >= length" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (listOf [i32 1]) >> stackPush (i32 3) >> stackPush (i32 9) >> instNthUpdate))
        "NthUpdate OOB"

    it "throws if idx is not an integer" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (listOf [i32 1]) >> stackPush (listOf []) >> stackPush (i32 9) >> instNthUpdate))
        "NthUpdate expects List and Integer idx"

    it "throws if list is not a list" $ do
      shouldThrowMsg
        (fst <$> runVM (stackPush (i32 123) >> stackPush (i32 0) >> stackPush (i32 9) >> instNthUpdate))
        "NthUpdate expects List and Integer idx"
