{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VariableSpec
-}

module VM.Instruction.VariableSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (execStateT)
import Control.Exception (try, SomeException, displayException)
import Data.Bits (shiftR)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Word (Word8, Word32)
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import VM.VMState (VMState(..), createVMState)
import VM.VMValue (VMValue(..))
import Common.Type.Integer (IntValue(..)) -- ✅ FIX I64

import VM.Instruction.Variable
  ( instLoadLocal
  , instStoreLocal
  , instLoadCapture
  , instStoreCapture
  )

beI32 :: Int -> [Word8]
beI32 n =
  let w :: Word32
      w = fromIntegral (fromIntegral n :: Int32)
  in [ fromIntegral (w `shiftR` 24)
     , fromIntegral (w `shiftR` 16)
     , fromIntegral (w `shiftR` 8)
     , fromIntegral w
     ]

mkVM :: [Word8] -> VMState
mkVM bytes = createVMState (BS.pack bytes) False

expectVMErrorContains :: IO a -> String -> Expectation
expectVMErrorContains action needle = do
  r <- try (action >> pure ()) :: IO (Either SomeException ())
  case r of
    Left e  -> displayException e `shouldSatisfy` isInfixOf needle
    Right _ -> expectationFailure "Expected VM error, but action succeeded."

spec :: Spec
spec = describe "VM.Instruction.Variable" $ do

  describe "instLoadLocal" $ do
    it "pushes the value at FP + offset onto the stack (positive offset)" $ do
      let vm0 = (mkVM (beI32 1))
                { baseVStackIndex = 0
                , vStack = V.fromList [VInt (I64 10), VInt (I64 20), VBool False]
                }
      vm1 <- execStateT instLoadLocal vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 10), VInt (I64 20), VBool False, VInt (I64 20)]

    it "pushes the value at FP + offset (negative offset)" $ do
      let vm0 = (mkVM (beI32 (-2)))
                { baseVStackIndex = 2
                , vStack = V.fromList [VInt (I64 10), VInt (I64 20), VBool False]
                }
      vm1 <- execStateT instLoadLocal vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 10), VInt (I64 20), VBool False, VInt (I64 10)]

    it "throws out of bounds and message includes Index and Size (covers line 53)" $ do
      let vm0 = (mkVM (beI32 5))
                { baseVStackIndex = 0
                , vStack = V.fromList [VInt (I64 1), VInt (I64 2)]
                }
      expectVMErrorContains (execStateT instLoadLocal vm0) "LOAD_LOCAL out of bounds (Index: 5, Size: 2)"

  describe "instStoreLocal" $ do
    it "pops a value and stores it at FP + offset (positive offset)" $ do
      let vm0 = (mkVM (beI32 1))
                { baseVStackIndex = 0
                , vStack = V.fromList [VInt (I64 10), VInt (I64 20), VBool False, VInt (I64 30)]
                }
      vm1 <- execStateT instStoreLocal vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 10), VInt (I64 30), VBool False]

    it "pops a value and stores it at FP + offset (negative offset)" $ do
      let vm0 = (mkVM (beI32 (-2)))
                { baseVStackIndex = 2
                , vStack = V.fromList [VInt (I64 10), VInt (I64 20), VBool False, VInt (I64 99)]
                }
      vm1 <- execStateT instStoreLocal vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 99), VInt (I64 20), VBool False]

    it "throws out of bounds and message includes Index and Size (covers line 79)" $ do
      let vm0 = (mkVM (beI32 0))
                { baseVStackIndex = 0
                , vStack = V.fromList [VInt (I64 123)]
                }
      expectVMErrorContains (execStateT instStoreLocal vm0) "STORE_LOCAL out of bounds (Index: 0, Size: 0)"

  describe "instLoadCapture" $ do
    it "pushes env[idx] onto the stack" $ do
      let vm0 = (mkVM (beI32 1))
                { env = V.fromList [VInt (I64 7), VBool True]
                , vStack = V.fromList [VInt (I64 1)]
                }
      vm1 <- execStateT instLoadCapture vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1), VBool True]

    it "throws out of bounds and message includes idx (covers line 96)" $ do
      let vm0 = (mkVM (beI32 3))
                { env = V.fromList [VInt (I64 7)]
                }
      expectVMErrorContains (execStateT instLoadCapture vm0) "LOAD_CAPTURE out of bounds (3)"

  describe "instStoreCapture" $ do
    it "pops a value and stores it into env[idx]" $ do
      let vm0 = (mkVM (beI32 0))
                { env = V.fromList [VInt (I64 1)]
                , vStack = V.fromList [VInt (I64 99)]
                }
      vm1 <- execStateT instStoreCapture vm0
      env vm1 `shouldBe` V.fromList [VInt (I64 99)]
      vStack vm1 `shouldBe` V.empty

    it "throws out of bounds and message includes idx (covers line 115)" $ do
      let vm0 = (mkVM (beI32 0))
                { env = V.empty
                , vStack = V.fromList [VInt (I64 5)]
                }
      expectVMErrorContains (execStateT instStoreCapture vm0) "STORE_CAPTURE out of bounds (0)"
