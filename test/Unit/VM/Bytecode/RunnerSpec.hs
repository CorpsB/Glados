{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM.Bytecode.Runner unit tests
-}

{-# LANGUAGE OverloadedStrings #-}

module VM.Bytecode.RunnerSpec (spec) where

import Test.Hspec
import Control.Exception (try, SomeException, displayException)
import Control.Monad.State.Strict (runStateT)
import Data.List (isInfixOf)
import Data.Word (Word8)
import qualified Data.ByteString as BS

import VM.VMState (VMState(..), VirtualMachine, createVMState)
import VM.Bytecode.Runner (executeInstruction, runBytecode)

runVM :: BS.ByteString -> Bool -> VirtualMachine a -> IO (Either SomeException (a, VMState))
runVM bc debugFlag action = try (runStateT action (createVMState bc debugFlag))

hasUnknownOpcodeMsg :: SomeException -> Bool
hasUnknownOpcodeMsg e =
  "Unknown Opcode" `isInfixOf` displayException e

spec :: Spec
spec = describe "VM.Bytecode.Runner" $ do

  describe "executeInstruction" $ do
    it "throws on unknown opcode" $ do
      res <- runVM BS.empty False (executeInstruction 0xAB)
      case res of
        Left e  -> hasUnknownOpcodeMsg e `shouldBe` True
        Right _ -> expectationFailure "Expected unknown opcode to throw, but it succeeded"

    it "NOP (0xFF) succeeds (does nothing)" $ do
      res <- runVM BS.empty False (executeInstruction 0xFF)
      case res of
        Left e  -> expectationFailure ("NOP should not throw, but got: " ++ displayException e)
        Right _ -> True `shouldBe` True

    it "covers every mapped opcode branch (no 'Unknown Opcode' for known opcodes)" $ do
      let knownOpcodes :: [Word8]
          knownOpcodes =
            [ 0x01,0x02,0x03,0x04
            , 0x10,0x11,0x12,0x13,0x14
            , 0x20,0x21,0x22,0x23,0x24,0x25,0x26
            , 0x30,0x31,0x32
            , 0x40,0x41,0x42,0x43
            , 0x50,0x51,0x52,0x53,0x54,0x55
            , 0x60,0x61,0x62,0x63,0x64
            , 0x70,0x71,0x72,0x80,0x81
            , 0x90,0x91,0x92,0x93,0x94,0x95
            , 0xA0,0xA1,0xA2,0xA3,0xA4
            , 0xFE,0xFF
            ]

      results <- mapM (\op -> runVM BS.empty False (executeInstruction op)) knownOpcodes

      let bad = [ op | (op, Left e) <- zip knownOpcodes results, hasUnknownOpcodeMsg e ]
      bad `shouldSatisfy` null

  describe "runBytecode" $ do
    it "stops when HALT is executed (NOP then HALT)" $ do
      let bc = BS.pack [0xFF, 0x71]
      res <- runVM bc False runBytecode
      case res of
        Left e -> expectationFailure ("runBytecode should not throw, got: " ++ displayException e)
        Right (_a, st) -> do
          isRunning st `shouldBe` False
          bytecodeIndex st `shouldBe` 2

    it "takes the debugTrace path when debug is enabled (should not crash)" $ do
      let bc = BS.pack [0x71]
      res <- runVM bc True runBytecode
      case res of
        Left e -> expectationFailure ("runBytecode (debug) should not throw, got: " ++ displayException e)
        Right (_a, st) -> do
          isRunning st `shouldBe` False
          bytecodeIndex st `shouldBe` 1
