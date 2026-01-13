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
import Control.Exception (ErrorCall(..))
import Data.List (isInfixOf)
import Data.Bits (shiftR)
import Data.Int (Int32)
import Data.Word (Word8, Word32)

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))
import VM.Instruction.System (instPrint, instHalt, instCheckStack)

runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

mkVM :: BS.ByteString -> [VMValue] -> Bool -> VMState
mkVM bc stk running =
  (createVMState bc)
    { vStack = V.fromList stk
    , isRunning = running
    , bytecodeIndex = 0
    }

beI32 :: Int -> [Word8]
beI32 n =
  let w :: Word32
      w = fromIntegral (fromIntegral n :: Int32)
  in [ fromIntegral (w `shiftR` 24)
     , fromIntegral (w `shiftR` 16)
     , fromIntegral (w `shiftR` 8)
     , fromIntegral w
     ]

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
      let vm0 = mkVM BS.empty [] True
      (_, vm1) <- runVM instHalt vm0
      isRunning vm1 `shouldBe` False

    it "does not modify the stack" $ do
      let vm0 = mkVM BS.empty [VInt (I64 1), VBool True] True
      (_, vm1) <- runVM instHalt vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1), VBool True]

  describe "instPrint" $ do
    it "pops the top value and prints it (newline included)" $ do
      let vm0 = mkVM BS.empty [VInt (I64 42)] True
      (((), vm1), out) <- captureStdout (runVM instPrint vm0)
      out `shouldBe` "42\n"
      vStack vm1 `shouldBe` V.empty

    it "throws Stack Underflow on empty stack" $ do
      let vm0 = mkVM BS.empty [] True
      evalVM instPrint vm0 `shouldThrow` \(ErrorCall msg) ->
        "VM Error: Stack Underflow" `isInfixOf` msg

  describe "instCheckStack" $ do
    it "succeeds when stack has enough values" $ do
      let bc = BS.pack (beI32 2)
      let vm0 = mkVM bc [VBool True, VInt (I8 1)] True
      (_, vm1) <- runVM instCheckStack vm0
      bytecodeIndex vm1 `shouldBe` 4

    it "fails when stack is too small and message includes sizes (forces show)" $ do
      let bc = BS.pack (beI32 3)
      let vm0 = mkVM bc [VBool True] True
      evalVM instCheckStack vm0 `shouldThrow` \(ErrorCall msg) ->
        "VM Error: Stack Check Failed (Required: 3, Actual: 1)" `isInfixOf` msg
