{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Function Instructions Spec
-}

{-# LANGUAGE LambdaCase #-}

module VM.Instruction.FunctionSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Bits (shiftR)
import Data.Int (Int32)
import Data.Word (Word32)
import Data.List (isInfixOf)
import Control.Exception (ErrorCall(..))

import VM.VMState (VMState(..), createVMState)
import VM.VMValue (VMValue(..))
import VM.CallSnapshot (CallSnapshot(..))
import VM.Instruction.Function
import qualified Common.Type.Integer as Common

-- Small helpers ---------------------------------------------------------------

i :: Int -> VMValue
i n = VInt (Common.fromInt64 (fromIntegral n))

beI32 :: Int -> BS.ByteString
beI32 n =
  let w :: Word32
      w = fromIntegral (fromIntegral n :: Int32)
  in BS.pack
      [ fromIntegral (w `shiftR` 24)
      , fromIntegral (w `shiftR` 16)
      , fromIntegral (w `shiftR`  8)
      , fromIntegral w
      ]

mkVM :: BS.ByteString -> VMState
mkVM bc =
  (createVMState bc False)
    { bytecodeIndex = 0
    , isRunning = True
    }

expectTopSnap :: VMState -> (CallSnapshot -> Expectation) -> Expectation
expectTopSnap vm k =
  case snapshotStack vm of
    (s:_) -> k s
    []    -> expectationFailure "Expected snapshotStack to contain at least one CallSnapshot"

-- Spec ------------------------------------------------------------------------

spec :: Spec
spec = describe "VM.Instruction.Function" $ do

  describe "instCall (CALL 0x40)" $ do
    it "reads offset, pushes a snapshot (saving old env), jumps to (post-read IP)+offset, clears env" $ do
      let offset = 10
      let bc = beI32 offset

      let vm0 =
            (mkVM bc)
              { bytecodeIndex = 0
              , baseVStackIndex = 0
              , env = V.fromList [VBool True]        -- should be SAVED into snapshot
              , vStack = V.fromList [i 1, i 2]       -- FP becomes stack length
              }

      (_, vm1) <- runStateT instCall vm0

      -- After reading Int32, IP is 4. Target = 4 + offset
      bytecodeIndex vm1 `shouldBe` (4 + offset)

      -- New frame pointer becomes current stack length
      baseVStackIndex vm1 `shouldBe` V.length (vStack vm0)

      -- Env is cleared for static calls
      env vm1 `shouldBe` V.empty

      -- Snapshot pushed, stores return address (= post-read IP) and previous env
      expectTopSnap vm1 $ \s -> do
        callbackIndex s `shouldBe` 4
        vStackIndex s `shouldBe` baseVStackIndex vm0
        vEnv s `shouldBe` env vm0

  describe "instTailCall (TAIL_CALL 0x41)" $ do
    it "reads offset and replaces current frame (no snapshot pushed), clears env" $ do
      let offset = 7
      let bc = beI32 offset

      let snap0 = CallSnapshot { callbackIndex = 999, vStackIndex = 123, vEnv = V.fromList [i 42] }

      let vm0 =
            (mkVM bc)
              { bytecodeIndex = 0
              , baseVStackIndex = 0
              , env = V.fromList [VBool True]
              , vStack = V.fromList [i 1, i 2, i 3]
              , snapshotStack = [snap0]
              }

      (_, vm1) <- runStateT instTailCall vm0

      -- post-read IP is 4, target is 4 + offset
      bytecodeIndex vm1 `shouldBe` (4 + offset)

      -- FP becomes current stack length
      baseVStackIndex vm1 `shouldBe` V.length (vStack vm0)

      -- env cleared
      env vm1 `shouldBe` V.empty

      -- snapshotStack preserved (no push)
      snapshotStack vm1 `shouldBe` snapshotStack vm0

  describe "instCallIndirect (CALL_INDIRECT 0x42)" $ do
    it "calls a closure (uses its address + captured env)" $ do
      let caps = V.fromList [i 9, VBool False]
      let vm0 =
            (mkVM BS.empty)
              { bytecodeIndex = 10
              , baseVStackIndex = 0
              , env = V.fromList [VBool True] -- should be saved into snapshot
              , vStack = V.fromList [i 1, VClosure 77 caps]
              }

      (_, vm1) <- runStateT instCallIndirect vm0

      -- Jump to closure addr
      bytecodeIndex vm1 `shouldBe` 77

      -- Env becomes captured env
      env vm1 `shouldBe` caps

      -- Callee popped from stack
      vStack vm1 `shouldBe` V.fromList [i 1]

      -- Snapshot pushed, return address is current IP (no read here)
      expectTopSnap vm1 $ \s -> do
        callbackIndex s `shouldBe` 10
        vStackIndex s `shouldBe` baseVStackIndex vm0
        vEnv s `shouldBe` env vm0

    it "calls a function pointer (uses address + empty env)" $ do
      let vm0 =
            (mkVM BS.empty)
              { bytecodeIndex = 33
              , baseVStackIndex = 0
              , env = V.fromList [VBool False] -- saved into snapshot
              , vStack = V.fromList [i 1, VFuncPtr 55]
              }

      (_, vm1) <- runStateT instCallIndirect vm0

      bytecodeIndex vm1 `shouldBe` 55
      env vm1 `shouldBe` V.empty
      vStack vm1 `shouldBe` V.fromList [i 1]

      expectTopSnap vm1 $ \s -> do
        callbackIndex s `shouldBe` 33
        vEnv s `shouldBe` env vm0

    it "throws when popped value is not callable and message contains show value (covers show x)" $ do
      let vm0 =
            (mkVM BS.empty)
              { vStack = V.fromList [VBool True]
              }

      runStateT instCallIndirect vm0 `shouldThrow` \case
        ErrorCall msg ->
          ("VM Error: Not callable:" `isInfixOf` msg) &&
          ("VBool True" `isInfixOf` msg)

  describe "instMakeClosure (MAKE_CLOSURE 0x60)" $ do
    it "captures last N values and pushes VClosure" $ do
      let bc = beI32 77 <> beI32 2

      let vm0 =
            (mkVM bc)
              { vStack = V.fromList [i 1, i 2, i 3]
              }

      (_, vm1) <- runStateT instMakeClosure vm0

      vStack vm1 `shouldBe`
        V.fromList
          [ i 1
          , VClosure 85 (V.fromList [i 2, i 3])
          ]

    it "throws MAKE_CLOSURE Stack Underflow when not enough values" $ do
      let bc = beI32 77 <> beI32 5

      let vm0 =
            (mkVM bc)
              { vStack = V.fromList [i 1, i 2]
              }

      runStateT instMakeClosure vm0 `shouldThrow` \case
        ErrorCall msg -> "VM Error: MAKE_CLOSURE Stack Underflow" `isInfixOf` msg

  describe "instRet (RET 0x43)" $ do
    it "restores IP/FP/Env, pops snapshot, cleans stack, then pushes return value" $ do
      let snap = CallSnapshot { callbackIndex = 123, vStackIndex = 0, vEnv = V.fromList [i 42] }
      let vm0 =
            (mkVM (beI32 0)) 
              { baseVStackIndex = 2
              , bytecodeIndex = 0
              , env = V.fromList [VBool False]
              , snapshotStack = [snap]
              , vStack = V.fromList [i 10, i 20, VBool False, i 999]
              }

      (_, vm1) <- runStateT instRet vm0
      bytecodeIndex vm1 `shouldBe` 123
      baseVStackIndex vm1 `shouldBe` 0
      vStack vm1 `shouldBe` V.fromList [i 10, i 20, i 999]

    it "throws when call stack is empty" $ do
      let vm0 =
            (mkVM (beI32 0))
              { snapshotStack = []
              , vStack = V.fromList [i 1]
              }
      runStateT instRet vm0 `shouldThrow` \case
        ErrorCall msg -> "VM Error: Return called with empty call stack" `isInfixOf` msg
