{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VMStateSpec
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VM.VMStateSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT, evalStateT)
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import VM.VMState (VMState(..), VirtualMachine, createVMState, createSnapshot, doSnapshot)
import VM.CallSnapshot (CallSnapshot(..))
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))

-- Helpers
runVM :: VirtualMachine a -> VMState -> IO (a, VMState)
runVM = runStateT

evalVM :: VirtualMachine a -> VMState -> IO a
evalVM = evalStateT

spec :: Spec
spec = describe "VM.VMState" $ do

  describe "createVMState" $ do
    it "initializes fields correctly (bytecode, indices, stacks, envs, isRunning)" $ do
      let code = BS.pack [0x01, 0x02, 0x03]
      let vm = createVMState code

      bytecode vm `shouldBe` code
      bytecodeIndex vm `shouldBe` 0
      vStack vm `shouldBe` V.empty
      baseVStackIndex vm `shouldBe` 0
      snapshotStack vm `shouldBe` []
      env vm `shouldBe` V.empty
      V.length (globalEnv vm) `shouldBe` 1024
      isRunning vm `shouldBe` True

    it "does not force evaluation of globalEnv (it contains undefined cells)" $ do
      let vm = createVMState (BS.pack [])
      -- This must not crash: only checking length, not elements
      V.length (globalEnv vm) `shouldBe` 1024

  describe "createSnapshot" $ do
    it "captures callbackIndex, vStackIndex and vEnv from the VM state" $ do
      let code = BS.pack [0xAA]
      let vm0 = (createVMState code)
                { bytecodeIndex = 42
                , baseVStackIndex = 7
                , env = V.fromList [VInt (I8 1), VBool True]
                }

      let snap = createSnapshot vm0
      callbackIndex snap `shouldBe` 42
      vStackIndex snap `shouldBe` 7
      vEnv snap `shouldBe` V.fromList [VInt (I8 1), VBool True]

  describe "doSnapshot" $ do
    it "pushes a snapshot and updates baseVStackIndex, bytecodeIndex and env" $ do
      let code = BS.pack [0x00, 0x00]
      let vm0 = (createVMState code)
                { bytecodeIndex = 10
                , baseVStackIndex = 3
                , vStack = V.fromList [VInt (I8 11), VBool False]  -- length = 2
                , env = V.fromList [VInt (I8 1)]
                }

      let newEnv = V.fromList [VBool True, VVoid]

      (_, vm1) <- runVM (doSnapshot 99 newEnv) vm0

      -- Snapshot pushed
      length (snapshotStack vm1) `shouldBe` 1
      let topSnap = head (snapshotStack vm1)
      callbackIndex topSnap `shouldBe` 10
      vStackIndex topSnap `shouldBe` 3
      vEnv topSnap `shouldBe` V.fromList [VInt (I8 1)]

      -- Updated registers/env
      baseVStackIndex vm1 `shouldBe` 2   -- length of old vStack
      bytecodeIndex vm1 `shouldBe` 99
      env vm1 `shouldBe` newEnv

      -- Unchanged parts
      bytecode vm1 `shouldBe` code
      vStack vm1 `shouldBe` V.fromList [VInt (I8 11), VBool False]
      isRunning vm1 `shouldBe` True
      V.length (globalEnv vm1) `shouldBe` 1024

    it "stacks snapshots (LIFO) when called multiple times" $ do
      let vm0 = (createVMState (BS.pack [0x00]))
                { bytecodeIndex = 1
                , baseVStackIndex = 0
                , vStack = V.fromList [VInt (I8 9)]  -- length=1
                , env = V.fromList [VBool False]
                }

      let env1 = V.fromList [VInt (I8 1)]
      let env2 = V.fromList [VInt (I8 2), VVoid]

      (_, vm1) <- runVM (doSnapshot 100 env1) vm0
      (_, vm2) <- runVM (doSnapshot 200 env2) vm1

      length (snapshotStack vm2) `shouldBe` 2

      -- Latest snapshot on top should reflect state at time of second call (vm1)
      let sTop = head (snapshotStack vm2)
      callbackIndex sTop `shouldBe` 100
      vStackIndex sTop `shouldBe` 1
      vEnv sTop `shouldBe` env1

      -- Next snapshot should be original state (vm0)
      let sNext = (snapshotStack vm2) !! 1
      callbackIndex sNext `shouldBe` 1
      vStackIndex sNext `shouldBe` 0
      vEnv sNext `shouldBe` V.fromList [VBool False]

      -- After second doSnapshot, baseVStackIndex becomes length of vStack in vm1 (still 1)
      baseVStackIndex vm2 `shouldBe` 1
      bytecodeIndex vm2 `shouldBe` 200
      env vm2 `shouldBe` env2
