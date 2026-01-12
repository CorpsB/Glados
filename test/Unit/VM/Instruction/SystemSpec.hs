{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- SystemSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.Instruction.SystemSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Control.Exception (finally)
import System.IO (stdout, hFlush, openTempFile, hClose)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))
import VM.Instruction.System (instPrint, instHalt)

runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

mkVM :: [VMValue] -> Bool -> VMState
mkVM stk running =
  (createVMState BS.empty)
    { vStack = V.fromList stk
    , isRunning = running
    }

captureStdout :: IO a -> IO (a, String)
captureStdout action = do
  (fp, h) <- openTempFile "/tmp" "glados_stdout.txt"
  old <- hDuplicate stdout
  hDuplicateTo h stdout

  result <- action `finally` (hFlush stdout >> hDuplicateTo old stdout)

  hFlush stdout
  hClose h
  hClose old

  out <- readFile fp
  pure (result, out)

spec :: Spec
spec = describe "VM.Instruction.System" $ do

  describe "instHalt" $ do
    it "sets isRunning to False" $ do
      let vm0 = mkVM [] True
      (_, vm1) <- runVM instHalt vm0
      isRunning vm1 `shouldBe` False

    it "does not modify the stack" $ do
      let vm0 = mkVM [VInt (I64 1), VBool True] True
      (_, vm1) <- runVM instHalt vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1), VBool True]

  describe "instPrint" $ do
    it "pops the top value and prints it (newline included)" $ do
      let vm0 = mkVM [VInt (I64 42)] True
      (((), vm1), out) <- captureStdout (runVM instPrint vm0)
      out `shouldBe` "42\n"
      vStack vm1 `shouldBe` V.empty

    it "throws Stack Underflow on empty stack" $ do
      let vm0 = mkVM [] True
      evalVM instPrint vm0 `shouldThrow` errorCall "VM Error: Stack Underflow"
