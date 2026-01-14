{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- RunnerSpec (Complete Instruction Set Coverage)
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
import VM.CallSnapshot (CallSnapshot(..))
import Common.Type.Integer (IntValue(..))

-- Helper: Encode Int as signed Int32 big endian
i32be :: Int -> [Word8]
i32be n =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]
  where w = fromIntegral (fromIntegral n :: Int32) :: Word32

-- Helper: Create a VM state with specific stack
vmWithStack :: [VMValue] -> VMState
vmWithStack vals = (createVMState BS.empty) { vStack = V.fromList vals, isRunning = True }

-- Helper: Create a VM state with code
vmWithCode :: [Word8] -> VMState
vmWithCode bytes = (createVMState (BS.pack bytes)) { isRunning = True }

spec :: Spec
spec = describe "VM Instruction Set Coverage" $ do

  -- ==========================================
  -- 1. STACK OPERATIONS
  -- ==========================================
  describe "Stack Operations" $ do
    it "0x01 PUSH (Int8)" $ do
      -- Opcode PUSH (0x01) + Type I8 (0x01) + Value (42)
      let vm0 = vmWithCode [0x01, 0x42]
      (_, vm1) <- runStateT (executeInstruction 0x01) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I8 66)] -- 0x42 = 66

    it "0x01 PUSH (Bool True)" $ do
      -- Opcode PUSH (0x01) + Type Bool (0x00) + Value (1)
      let vm0 = vmWithCode [0x00, 0x01]
      (_, vm1) <- runStateT (executeInstruction 0x01) vm0
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "0x02 POP" $ do
      let vm0 = vmWithStack [VInt (I64 1), VInt (I64 2)]
      (_, vm1) <- runStateT (executeInstruction 0x02) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1)]

    it "0x03 DUP" $ do
      let vm0 = vmWithStack [VInt (I64 42)]
      (_, vm1) <- runStateT (executeInstruction 0x03) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 42), VInt (I64 42)]

    it "0x04 SWAP" $ do
      let vm0 = vmWithStack [VInt (I64 1), VInt (I64 2)]
      (_, vm1) <- runStateT (executeInstruction 0x04) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 2), VInt (I64 1)]

  -- ==========================================
  -- 2. ARITHMETIC (Binary operations)
  -- ==========================================
  describe "Arithmetic Operations" $ do
    let stackInts = [VInt (I64 10), VInt (I64 2)] 

    it "0x10 ADD (10 + 2)" $ do
      (_, vm1) <- runStateT (executeInstruction 0x10) (vmWithStack stackInts)
      vStack vm1 `shouldBe` V.fromList [VInt (I64 12)]

    it "0x11 SUB (10 - 2)" $ do
      (_, vm1) <- runStateT (executeInstruction 0x11) (vmWithStack stackInts)
      vStack vm1 `shouldBe` V.fromList [VInt (I64 8)]

    it "0x12 MUL (10 * 2)" $ do
      (_, vm1) <- runStateT (executeInstruction 0x12) (vmWithStack stackInts)
      vStack vm1 `shouldBe` V.fromList [VInt (I64 20)]

    it "0x13 DIV (10 / 2)" $ do
      (_, vm1) <- runStateT (executeInstruction 0x13) (vmWithStack stackInts)
      vStack vm1 `shouldBe` V.fromList [VInt (I64 5)]

    it "0x14 MOD (10 % 3)" $ do
      let vm0 = vmWithStack [VInt (I64 10), VInt (I64 3)]
      (_, vm1) <- runStateT (executeInstruction 0x14) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 1)]

  -- ==========================================
  -- 3. LOGIC & COMPARISON
  -- ==========================================
  describe "Logic & Comparison" $ do
    it "0x20 EQ (Equal)" $ do
      let vm0 = vmWithStack [VInt (I64 5), VInt (I64 5)]
      (_, vm1) <- runStateT (executeInstruction 0x20) vm0
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "0x21 LT (Less Than)" $ do
      let vm0 = vmWithStack [VInt (I64 5), VInt (I64 10)] -- 5 < 10
      (_, vm1) <- runStateT (executeInstruction 0x21) vm0
      vStack vm1 `shouldBe` V.fromList [VBool True]

    it "0x22 NOT" $ do
      let vm0 = vmWithStack [VBool True]
      (_, vm1) <- runStateT (executeInstruction 0x22) vm0
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "0x23 AND" $ do
      let vm0 = vmWithStack [VBool True, VBool False]
      (_, vm1) <- runStateT (executeInstruction 0x23) vm0
      vStack vm1 `shouldBe` V.fromList [VBool False]

    it "0x24 OR" $ do
      let vm0 = vmWithStack [VBool True, VBool False]
      (_, vm1) <- runStateT (executeInstruction 0x24) vm0
      vStack vm1 `shouldBe` V.fromList [VBool True]
    
    it "0x25 LE (Less or Equal)" $ do
      let vm0 = vmWithStack [VInt (I64 10), VInt (I64 10)]
      (_, vm1) <- runStateT (executeInstruction 0x25) vm0
      vStack vm1 `shouldBe` V.fromList [VBool True]

  -- ==========================================
  -- 4. FLOW CONTROL (Jumps)
  -- ==========================================
  describe "Flow Control" $ do
    it "0x30 JUMP (Unconditional)" $ do
      let vm0 = vmWithCode (i32be 10)
      (_, vm1) <- runStateT (executeInstruction 0x30) vm0
      bytecodeIndex vm1 `shouldBe` 14

    it "0x31 JUMP_IF_FALSE (False condition)" $ do
      let vm0 = (vmWithCode (i32be 20)) { vStack = V.fromList [VBool False] }
      (_, vm1) <- runStateT (executeInstruction 0x31) vm0
      bytecodeIndex vm1 `shouldBe` 24

    it "0x31 JUMP_IF_FALSE (True condition - No Jump)" $ do
      let vm0 = (vmWithCode (i32be 20)) { vStack = V.fromList [VBool True] }
      (_, vm1) <- runStateT (executeInstruction 0x31) vm0
      bytecodeIndex vm1 `shouldBe` 4

    it "0x32 JUMP_IF_TRUE (True condition)" $ do
      let vm0 = (vmWithCode (i32be 30)) { vStack = V.fromList [VBool True] }
      (_, vm1) <- runStateT (executeInstruction 0x32) vm0
      bytecodeIndex vm1 `shouldBe` 34

  -- ==========================================
  -- 5. FUNCTIONS & CALLS
  -- ==========================================
  describe "Functions" $ do
    it "0x40 CALL (Push Snapshot)" $ do
      let vm0 = (vmWithCode (i32be 50))
      (_, vm1) <- runStateT (executeInstruction 0x40) vm0
      bytecodeIndex vm1 `shouldBe` 54
      length (snapshotStack vm1) `shouldBe` 1
      callbackIndex (head (snapshotStack vm1)) `shouldBe` 4 

    it "0x43 RET (Restore Snapshot)" $ do
      let snapshot = CallSnapshot { callbackIndex = 100, vStackIndex = 0, vEnv = V.empty }
      let vm0 = (vmWithStack [VInt (I64 99)]) 
                  { snapshotStack = [snapshot]
                  , bytecodeIndex = 500 
                  }
      (_, vm1) <- runStateT (executeInstruction 0x43) vm0
      bytecodeIndex vm1 `shouldBe` 100
      vStack vm1 `shouldBe` V.fromList [VInt (I64 99)]
      length (snapshotStack vm1) `shouldBe` 0

    it "0x41 TAIL_CALL (No new snapshot)" $ do
      let vm0 = (vmWithCode (i32be 100)) { snapshotStack = [] }
      (_, vm1) <- runStateT (executeInstruction 0x41) vm0
      bytecodeIndex vm1 `shouldBe` 104
      length (snapshotStack vm1) `shouldBe` 0

    it "0x42 CALL_INDIRECT (Call from stack value)" $ do
      let vm0 = (vmWithCode []) 
                  { vStack = V.fromList [VClosure 50 V.empty] 
                  , bytecodeIndex = 10
                  }
      (_, vm1) <- runStateT (executeInstruction 0x42) vm0
      bytecodeIndex vm1 `shouldBe` 50
      length (snapshotStack vm1) `shouldBe` 1
      callbackIndex (head (snapshotStack vm1)) `shouldBe` 10

    it "0x61 GET_FUNC_ADDR" $ do
      let vm0 = vmWithCode (i32be 999)
      (_, vm1) <- runStateT (executeInstruction 0x61) vm0
      V.last (vStack vm1) `shouldBe` VFuncPtr 999

  -- ==========================================
  -- 6. MEMORY & VARIABLES
  -- ==========================================
  describe "Memory & Variables" $ do
    it "0x50 LOAD_LOCAL" $ do
      let vm0 = (vmWithCode (i32be 0)) 
                  { vStack = V.fromList [VInt (I64 10), VInt (I64 20), VInt (I64 30)]
                  , baseVStackIndex = 1
                  }
      (_, vm1) <- runStateT (executeInstruction 0x50) vm0
      V.last (vStack vm1) `shouldBe` VInt (I64 20)

    it "0x51 STORE_LOCAL" $ do
      let vm0 = (vmWithCode (i32be 0))
                  { vStack = V.fromList [VInt (I64 10), VInt (I64 20), VInt (I64 99)]
                  , baseVStackIndex = 1
                  }
      (_, vm1) <- runStateT (executeInstruction 0x51) vm0
      vStack vm1 `shouldBe` V.fromList [VInt (I64 10), VInt (I64 99)]

    it "0x54 LOAD_CAPTURE" $ do
      let vm0 = (vmWithCode (i32be 0))
                  { env = V.fromList [VInt (I64 123)] }
      (_, vm1) <- runStateT (executeInstruction 0x54) vm0
      V.last (vStack vm1) `shouldBe` VInt (I64 123)

    it "0x55 STORE_CAPTURE" $ do
      let vm0 = (vmWithCode (i32be 0))
                  { env = V.fromList [VInt (I64 0)]
                  , vStack = V.fromList [VInt (I64 456)]
                  }
      (_, vm1) <- runStateT (executeInstruction 0x55) vm0
      env vm1 `shouldBe` V.fromList [VInt (I64 456)]

    it "0x52 LOAD_GLOBAL" $ do
      let globals = V.fromList [VInt (I64 100), VInt (I64 200)]
      let vm0 = (vmWithCode (i32be 1)) { globalEnv = globals }
      (_, vm1) <- runStateT (executeInstruction 0x52) vm0
      V.last (vStack vm1) `shouldBe` VInt (I64 200)

    it "0x53 STORE_GLOBAL" $ do
      let globals = V.fromList [VInt (I64 0), VInt (I64 0)]
      let vm0 = (vmWithCode (i32be 0))
                  { globalEnv = globals
                  , vStack = V.fromList [VInt (I64 42)] 
                  }
      (_, vm1) <- runStateT (executeInstruction 0x53) vm0
      globalEnv vm1 `shouldBe` V.fromList [VInt (I64 42), VInt (I64 0)]

  -- ==========================================
  -- 7. CLOSURES & STRUCTS
  -- ==========================================
  describe "Closures & Structs" $ do
    it "0x60 MAKE_CLOSURE" $ do
      let bytes = i32be 100 ++ i32be 2
      let vm0 = (vmWithCode bytes) { vStack = V.fromList [VInt (I64 1), VInt (I64 2)] }
      (_, vm1) <- runStateT (executeInstruction 0x60) vm0
      let closure = V.last (vStack vm1)
      case closure of
        VClosure addr caps -> do
            addr `shouldBe` 100
            caps `shouldBe` V.fromList [VInt (I64 1), VInt (I64 2)]
        _ -> expectationFailure "Expected VClosure"

    it "0x62 BUILD_STRUCT" $ do
      let vm0 = (vmWithCode (i32be 3)) 
                  { vStack = V.fromList [VInt (I64 1), VInt (I64 2), VInt (I64 3)] }
      (_, vm1) <- runStateT (executeInstruction 0x62) vm0
      case V.last (vStack vm1) of
          VStruct fields -> fields `shouldBe` V.fromList [VInt (I64 1), VInt (I64 2), VInt (I64 3)]
          _ -> expectationFailure "Expected VStruct"

    it "0x63 GET_STRUCT_FIELD" $ do
      let myStruct = VStruct (V.fromList [VInt (I64 10), VInt (I64 20)])
      let vm0 = (vmWithCode (i32be 1)) { vStack = V.fromList [myStruct] }
      (_, vm1) <- runStateT (executeInstruction 0x63) vm0
      V.last (vStack vm1) `shouldBe` VInt (I64 20)

  -- ==========================================
  -- 8. LIST OPERATIONS
  -- ==========================================
  describe "List Operations" $ do
    it "0x90 CONS" $ do
       let vm0 = vmWithStack [VInt (I64 1), VList V.empty]
       (_, vm1) <- runStateT (executeInstruction 0x90) vm0
       vStack vm1 `shouldBe` V.fromList [VList (V.fromList [VInt (I64 1)])]

    it "0x91 HEAD" $ do
       let list = VList (V.fromList [VInt (I64 1), VInt (I64 2)])
       let vm0 = vmWithStack [list]
       (_, vm1) <- runStateT (executeInstruction 0x91) vm0
       vStack vm1 `shouldBe` V.fromList [VInt (I64 1)]

    it "0x92 TAIL" $ do
       let list = VList (V.fromList [VInt (I64 1), VInt (I64 2)])
       let vm0 = vmWithStack [list]
       (_, vm1) <- runStateT (executeInstruction 0x92) vm0
       case V.last (vStack vm1) of
           VList v -> v `shouldBe` V.fromList [VInt (I64 2)]
           _ -> expectationFailure "Expected List"

  -- ==========================================
  -- 9. TYPES & CASTING
  -- ==========================================
  describe "Type Casting" $ do
    it "0x80 CAST (Int -> Bool)" $ do
      let vm0 = (vmWithCode [0x00]) { vStack = V.fromList [VInt (I64 1)] }
      (_, vm1) <- runStateT (executeInstruction 0x80) vm0
      V.last (vStack vm1) `shouldBe` VBool True

    it "0x80 CAST (Int -> Int8)" $ do
      let vm0 = (vmWithCode [0x01]) { vStack = V.fromList [VInt (I64 255)] }
      (_, vm1) <- runStateT (executeInstruction 0x80) vm0
      V.last (vStack vm1) `shouldBe` VInt (I8 (-1))

  -- ==========================================
  -- 10. SYSTEM
  -- ==========================================
  describe "System & Debug" $ do
    it "0x70 PRINT (IO check)" $ do
       let vm0 = vmWithStack [VInt (I64 42)]
       (_, vm1) <- runStateT (executeInstruction 0x70) vm0
       vStack vm1 `shouldBe` V.empty

    it "0x71 HALT" $ do
       let vm0 = (createVMState BS.empty) { isRunning = True }
       (_, vm1) <- runStateT (executeInstruction 0x71) vm0
       isRunning vm1 `shouldBe` False

    it "0xFF NOP" $ do
       let vm0 = vmWithStack []
       (_, vm1) <- runStateT (executeInstruction 0xFF) vm0
       vStack vm1 `shouldBe` V.empty

    it "0xFE CHECK_STACK (Failure)" $ do
      let vm0 = (vmWithCode (i32be 3)) { vStack = V.fromList [VInt (I64 1)] }
      runStateT (executeInstruction 0xFE) vm0 `shouldThrow` anyErrorCall

    it "Unknown Opcode throws error" $ do
      let vm0 = vmWithCode [0xEE] -- 0xEE is undefined
      runStateT (executeInstruction 0xEE) vm0 
        `shouldThrow` (\(ErrorCall msg) -> "Unknown Opcode 0x238" `isInfixOf` msg)

  -- ==========================================
  -- RUNNER LOOP (runBytecode)
  -- ==========================================
  describe "VM Execution Loop (runBytecode)" $ do
    it "Stops when isRunning is False" $ do
      let vm0 = (createVMState BS.empty) { isRunning = False }
      (_, vm1) <- runStateT runBytecode vm0
      bytecodeIndex vm1 `shouldBe` 0

    it "Executes instructions sequentially until HALT" $ do
      let vm0 = vmWithCode [0xFF, 0xFF, 0x71]
      (_, vm1) <- runStateT runBytecode vm0
      bytecodeIndex vm1 `shouldBe` 3
      isRunning vm1 `shouldBe` False

    it "Propagates runtime errors" $ do
      let vm0 = vmWithCode [0x02] 
      runStateT runBytecode vm0 
        `shouldThrow` (\(ErrorCall msg) -> "Stack Underflow" `isInfixOf` msg)
