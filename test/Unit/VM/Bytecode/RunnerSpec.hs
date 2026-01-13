{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- RunnerSpec
-}

module VM.Bytecode.RunnerSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT)
import Control.Exception (ErrorCall(..))
import Data.List (isInfixOf)

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Bits (shiftR)
import Data.Int (Int32)
import Data.Word (Word8, Word32)

import VM.Bytecode.Runner (executeInstruction, runBytecode)
import VM.VMState (VMState(..), createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..))

-- Encode Int as signed Int32 big endian (compatible with Reader.readInt32)
i32be :: Int -> [Word8]
i32be n =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]
  where
    w :: Word32
    w = fromIntegral (fromIntegral n :: Int32)

spec :: Spec
spec = describe "VM.Bytecode.Runner (dispatch + loop coverage)" $ do

  describe "executeInstruction (dispatch coverage)" $ do
    it "Opcode 0x04 dispatches to instSwap" $ do
      let vm0 =
            (createVMState (BS.pack []))
              { vStack = V.fromList [VInt (I64 1), VBool True]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x04) vm0
      vStack vm1 `shouldBe` V.fromList [VBool True, VInt (I64 1)]

    it "Opcode 0x52 dispatches to instLoadGlobal" $ do
      let vm0 =
            (createVMState (BS.pack (i32be 1)))
              { vStack = V.empty
              , globalEnv = V.fromList [VInt (I64 10), VInt (I64 99)]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x52) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 99)]

    it "Opcode 0x53 dispatches to instStoreGlobal" $ do
      let vm0 =
            (createVMState (BS.pack (i32be 0)))
              { vStack = V.fromList [VInt (I64 777)]
              , globalEnv = V.fromList [VVoid]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x53) vm0
      vStack vm1 `shouldBe` V.empty
      (globalEnv vm1 V.! 0) `shouldBe` VInt (I64 777)

    it "Opcode 0x61 dispatches to instGetFuncAddr" $ do
      let vm0 =
            (createVMState (BS.pack (i32be 123)))
              { vStack = V.empty
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x61) vm0
      vStack vm1 `shouldBe` V.fromList [VFuncPtr 123]

    it "Opcode 0x62 dispatches to instBuildStruct" $ do
      let vm0 =
            (createVMState (BS.pack (i32be 2)))
              { vStack = V.fromList [VInt (I64 1), VBool False, VInt (I64 2)]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x62) vm0
      vStack vm1 `shouldBe`
        V.fromList
          [ VInt (I64 1)
          , VStruct (V.fromList [VBool False, VInt (I64 2)])
          ]

    it "Opcode 0x63 dispatches to instGetStructField" $ do
      let s = VStruct (V.fromList [VInt (I64 5), VBool True, VInt (I64 9)])
      let vm0 =
            (createVMState (BS.pack (i32be 1)))
              { vStack = V.fromList [s]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x63) vm0
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "Opcode 0x80 dispatches to instCast" $ do
      let vm0 =
            (createVMState (BS.pack [0x00]))
              { vStack = V.fromList [VInt (I64 0)]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x80) vm0
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "Opcode 0x90 dispatches to instCons (SUCCESS)" $ do
      -- IMPORTANT: CONS pops (list) then (element).
      -- So stack must be [..., element, list] with list on TOP.
      let xs = VList (V.fromList [VInt (I64 1), VInt (I64 2)])
      let vm0 =
            (createVMState (BS.pack []))
              { vStack = V.fromList [VInt (I64 10), xs]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x90) vm0
      vStack vm1 `shouldBe`
        V.fromList
          [ VList (V.fromList [VInt (I64 10), VInt (I64 1), VInt (I64 2)]) ]

    it "Opcode 0x91 dispatches to instHead" $ do
      let vm0 =
            (createVMState (BS.pack []))
              { vStack = V.fromList [VList (V.fromList [VInt (I64 7), VInt (I64 8)])]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x91) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 7)]

    it "Opcode 0x92 dispatches to instTail" $ do
      let vm0 =
            (createVMState (BS.pack []))
              { vStack = V.fromList [VList (V.fromList [VInt (I64 7), VInt (I64 8), VBool True])]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0x92) vm0
      vStack vm1 `shouldBe`
        V.fromList
          [ VList (V.fromList [VInt (I64 8), VBool True]) ]

    it "Opcode 0xFE dispatches to instCheckStack (success path)" $ do
      let vm0 =
            (createVMState (BS.pack (i32be 2)))
              { vStack = V.fromList [VInt (I64 1), VInt (I64 2)]
              , isRunning = True
              }
      (_, vm1) <- runStateT (executeInstruction 0xFE) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1), VInt (I64 2)]

    it "Opcode 0xFE dispatches to instCheckStack (failure path)" $ do
      let vm0 =
            (createVMState (BS.pack (i32be 3)))
              { vStack = V.fromList [VInt (I64 1), VInt (I64 2)]
              , isRunning = True
              }
      runStateT (executeInstruction 0xFE) vm0
        `shouldThrow` \(ErrorCall msg) ->
          ("Stack Check Failed" `isInfixOf` msg)
            && ("Required: 3" `isInfixOf` msg)
            && ("Actual: 2" `isInfixOf` msg)

    it "Unknown opcode throws" $ do
      let vm0 = (createVMState (BS.pack [])) { isRunning = True }
      runStateT (executeInstruction 0xAB) vm0
        `shouldThrow` \(ErrorCall msg) ->
          "Unknown Opcode" `isInfixOf` msg

  describe "runBytecode (loop coverage)" $ do
    it "stops immediately if isRunning is False" $ do
      let vm0 =
            (createVMState (BS.pack [0xFF, 0x71]))
              { isRunning = False
              , vStack = V.fromList [VInt (I64 1)]
              }
      (_, vm1) <- runStateT runBytecode vm0
      isRunning vm1 `shouldBe` False
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1)]
      bytecodeIndex vm1 `shouldBe` bytecodeIndex vm0

    it "executes NOP then HALT and stops" $ do
      let vm0 =
            (createVMState (BS.pack [0xFF, 0x71]))
              { isRunning = True
              }
      (_, vm1) <- runStateT runBytecode vm0
      isRunning vm1 `shouldBe` False

    it "throws if bytecode contains an unknown opcode while running" $ do
      let vm0 =
            (createVMState (BS.pack [0xAB]))
              { isRunning = True
              }
      runStateT runBytecode vm0
        `shouldThrow` \(ErrorCall msg) ->
          "Unknown Opcode" `isInfixOf` msg
