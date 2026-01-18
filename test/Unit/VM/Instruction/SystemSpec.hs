{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- System instructions unit tests
-}

{-# LANGUAGE OverloadedStrings #-}

module VM.Instruction.SystemSpec (spec) where

import Test.Hspec
import Control.Exception (ErrorCall(..))
import Control.Monad.State.Strict (runStateT)
import Data.Bits ((.&.), shiftR)
import Data.List (isInfixOf)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import System.Exit (ExitCode(..))

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))

import VM.Instruction.System (instCheckStack, instExit, instHalt)

encodeInt32BE :: Int -> [Word8]
encodeInt32BE n =
  let x  = n
      b3 = fromIntegral ((x `shiftR` 24) .&. 0xFF)
      b2 = fromIntegral ((x `shiftR` 16) .&. 0xFF)
      b1 = fromIntegral ((x `shiftR` 8)  .&. 0xFF)
      b0 = fromIntegral ( x              .&. 0xFF)
  in [b3, b2, b1, b0]

mkState :: [Word8] -> [VMValue] -> VMState
mkState bytes stackVals =
  let st0 = createVMState (BS.pack bytes) False
  in st0 { vStack = V.fromList stackVals }

i32 :: Int -> VMValue
i32 n = VInt (I32 (fromIntegral n))

runVM :: VMState -> VirtualMachine a -> IO (a, VMState)
runVM st act = runStateT act st

shouldThrowMsg :: IO a -> String -> Expectation
shouldThrowMsg io expected =
  io `shouldThrow` \(ErrorCall msg) ->
    expected `isInfixOf` msg

spec :: Spec
spec = describe "VM.Instruction.System" $ do

  describe "instHalt" $ do
    it "sets isRunning to False" $ do
      let st = mkState [] []
      (_a, st') <- runVM st instHalt
      isRunning st' `shouldBe` False

  describe "instCheckStack" $ do
    it "succeeds when stack length >= required (True branch)" $ do
      let st = mkState (encodeInt32BE 2) [i32 1, i32 2]
      (_a, st') <- runVM st instCheckStack
      bytecodeIndex st' `shouldBe` 4

    it "throws when stack length < required (False branch)" $ do
      let st = mkState (encodeInt32BE 3) [i32 1, i32 2]
      shouldThrowMsg
        (fst <$> runVM st instCheckStack)
        "VM Error: Stack Check Failed"

    it "error message contains Required and Actual values" $ do
      let st = mkState (encodeInt32BE 5) [i32 9]
      (fst <$> runVM st instCheckStack)
        `shouldThrow` \(ErrorCall msg) ->
          "Required: 5" `isInfixOf` msg && "Actual: 1" `isInfixOf` msg

  describe "instExit" $ do
    it "exits with ExitSuccess when code is 0 (case 0 branch)" $ do
      let st = mkState [] [i32 0]
      (fst <$> runVM st instExit)
        `shouldThrow` (\e -> e == ExitSuccess)

    it "exits with ExitFailure n when code is non-zero (default branch)" $ do
      let st = mkState [] [i32 84]
      (fst <$> runVM st instExit)
        `shouldThrow` (\e -> e == ExitFailure 84)

    it "pops the status code from the stack (stackPop path is executed)" $ do
      let st = mkState [] [i32 12]
      (fst <$> runVM st instExit)
        `shouldThrow` (\e -> e == ExitFailure 12)
