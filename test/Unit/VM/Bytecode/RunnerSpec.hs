{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- RunnerSpec
-}

module VM.Bytecode.RunnerSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT)
import Control.Exception (evaluate, ErrorCall(..))
import Data.List (isInfixOf)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import VM.Bytecode.Runner (runBytecode, executeInstruction)
import VM.VMState (VMState(..), createVMState)
import VM.CallSnapshot (CallSnapshot(..))
import VM.VMValue (VMValue(..))
import qualified Common.Type.Integer as Common

-- Small helper to build ints without depending on a specific constructor like I64
i :: Int -> VMValue
i n = VInt (Common.fromInt64 (fromIntegral n))

mkVM :: BS.ByteString -> VMState
mkVM bc =
  (createVMState bc)
    { bytecodeIndex = 0
    , isRunning = True
    }

forceDecode :: Word8 -> IO ()
forceDecode op = do
  -- This forces the pattern match inside executeInstruction,
  -- which is what we want for HPC coverage of VM.Bytecode.Runner.
  _ <- evaluate (executeInstruction op)
  pure ()

spec :: Spec
spec = describe "VM.Bytecode.Runner" $ do

  describe "executeInstruction" $ do
    it "covers all known opcode mappings (forces decode for each pattern)" $ do
      let ops :: [Word8]
          ops =
            [ 0x01, 0x02, 0x03
            , 0x10, 0x11, 0x12, 0x13, 0x14
            , 0x20, 0x21, 0x22, 0x23, 0x24, 0x25
            , 0x30, 0x31, 0x32
            , 0x40, 0x41, 0x42, 0x43
            , 0x50, 0x51, 0x54, 0x55
            , 0x60
            , 0x70, 0x71
            , 0xFF
            ]
      mapM_ forceDecode ops

    it "0xFF is NOP (covers executeInstruction 0xFF = return ()) without requiring Show VMState" $ do
      let vm0 =
            (mkVM BS.empty)
              { bytecodeIndex = 123
              , baseVStackIndex = 7
              , isRunning = True
              , vStack = V.fromList [VBool True, i 42]
              , env = V.fromList [VBool False]
              , snapshotStack =
                  [ CallSnapshot
                      { callbackIndex = 9
                      , vStackIndex = 3
                      , vEnv = V.fromList [i 1]
                      }
                  ]
              }

      (_, vm1) <- runStateT (executeInstruction 0xFF) vm0

      -- NOP must not change anything (we assert key fields only; no Show instance needed)
      bytecodeIndex vm1 `shouldBe` bytecodeIndex vm0
      baseVStackIndex vm1 `shouldBe` baseVStackIndex vm0
      isRunning vm1 `shouldBe` isRunning vm0
      vStack vm1 `shouldBe` vStack vm0
      env vm1 `shouldBe` env vm0
      snapshotStack vm1 `shouldBe` snapshotStack vm0

    it "unknown opcode throws expected message" $ do
      evaluate (executeInstruction 0x99) `shouldThrow` \(ErrorCall msg) ->
        ("VM Error: Unknown Opcode 0x" `isInfixOf` msg)

  describe "runBytecode" $ do
    it "does nothing when isRunning is False (covers False -> return ())" $ do
      let vm0 =
            (mkVM BS.empty)
              { isRunning = False
              , bytecodeIndex = 42
              , baseVStackIndex = 2
              , vStack = V.fromList [i 1, VBool True]
              , env = V.fromList [VBool False]
              , snapshotStack =
                  [ CallSnapshot
                      { callbackIndex = 77
                      , vStackIndex = 10
                      , vEnv = V.fromList [i 99]
                      }
                  ]
              }

      (_, vm1) <- runStateT runBytecode vm0

      isRunning vm1 `shouldBe` False
      bytecodeIndex vm1 `shouldBe` 42
      baseVStackIndex vm1 `shouldBe` baseVStackIndex vm0
      vStack vm1 `shouldBe` vStack vm0
      env vm1 `shouldBe` env vm0
      snapshotStack vm1 `shouldBe` snapshotStack vm0

    it "executes a HALT instruction then stops (covers True branch + loop)" $ do
      -- bytecode contains only HALT (0x71)
      let bc = BS.pack [0x71]
      let vm0 =
            (mkVM bc)
              { isRunning = True
              , bytecodeIndex = 0
              , vStack = V.fromList [i 1]
              }

      (_, vm1) <- runStateT runBytecode vm0

      -- readByte consumes 1 byte
      bytecodeIndex vm1 `shouldBe` 1
      -- HALT sets isRunning to False
      isRunning vm1 `shouldBe` False
      -- HALT does not modify the stack
      vStack vm1 `shouldBe` vStack vm0

    it "handles multiple instructions (NOP then HALT) and stops cleanly" $ do
      let bc = BS.pack [0xFF, 0x71] -- NOP then HALT
      let vm0 =
            (mkVM bc)
              { isRunning = True
              , bytecodeIndex = 0
              , vStack = V.fromList [i 10, i 20]
              }

      (_, vm1) <- runStateT runBytecode vm0

      -- read two bytes
      bytecodeIndex vm1 `shouldBe` 2
      isRunning vm1 `shouldBe` False
      vStack vm1 `shouldBe` vStack vm0
