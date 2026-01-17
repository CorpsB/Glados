{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Function Instructions
-}

{-|
Module      : VM.Instruction.Function
Description : Implementation of function call and return operations.
Stability   : stable

Handles the creation and destruction of stack frames (CallSnapshots).
Ensures proper scope management and control flow between functions.
-}
module VM.Instruction.Function
    ( instCall
    , instTailCall
    , instCallIndirect
    , instRet
    , instMakeClosure
    , instGetFuncAddr
    ) where

import Control.Monad.State.Strict (get, put)
import qualified Data.Vector as V

import VM.VMState (VirtualMachine, VMState(..), doSnapshot)
import VM.VMValue (VMValue(..))
import VM.CallSnapshot (CallSnapshot(..))
import VM.Bytecode.Reader (readInt32)
import VM.VMStack (stackPop, stackPush)

-- | Implements CALL (Opcode 0x40).
--
-- @details
--   Static call. Reads relative offset, calculates target, and delegates frame setup.
--
instCall :: VirtualMachine ()
instCall = do
    offset <- readInt32
    vm <- get
    doSnapshot (bytecodeIndex vm + offset) V.empty

-- | Implements TAIL_CALL (Opcode 0x41).
--
-- @details
--   Optimized call. Replaces current frame (no snapshot pushed).
--
instTailCall :: VirtualMachine ()
instTailCall = do
    offset <- readInt32
    vm <- get
    put $ vm { baseVStackIndex = V.length (vStack vm),
        bytecodeIndex = bytecodeIndex vm + offset, env = V.empty }

-- | Helper to dispatch CALL_INDIRECT based on the callable type.
--
-- @args
--   - callee: The value popped from stack (must be VClosure or VFuncPtr).
--
-- @details
--   Extracts the address and environment from the callable and calls 'doSnapshot'.
--
-- @throws
--   Error if 'callee' is not a callable type.
--
dispatchIndirectCall :: VMValue -> VirtualMachine ()
dispatchIndirectCall (VClosure addr caps) = doSnapshot addr caps
dispatchIndirectCall (VFuncPtr addr) = doSnapshot addr V.empty
dispatchIndirectCall x = error $ "VM Error: Not callable: " ++ show x

-- | Implements CALL_INDIRECT (Opcode 0x42).
--
-- @details
--   Pops the top value and delegates to 'dispatchIndirectCall'.
--
instCallIndirect :: VirtualMachine ()
instCallIndirect = do
    callee <- stackPop
    dispatchIndirectCall callee

-- | Helper to finalize the creation of a closure.
--
-- @args
--   - vm: The current VM state.
--   - addr: The function address.
--   - count: The number of variables to capture.
--
-- @details
--   Extracts the last 'count' elements from the stack, truncates the stack,
--   and pushes the new VClosure.
--
pushClosure :: VMState -> Int -> Int -> VirtualMachine ()
pushClosure vm addr count =
    put (vm { vStack = V.take start (vStack vm) }) >>
    stackPush (VClosure addr (V.slice start count (vStack vm)))
    where start = (V.length (vStack vm)) - count

-- | Implements MAKE_CLOSURE (Opcode 0x60).
--
-- @details
--   Reads arguments then checks if the stack has enough elements.
--   If yes, calls 'pushClosure'.
--
instMakeClosure :: VirtualMachine ()
instMakeClosure = do
    addr <- readInt32
    count <- readInt32
    vm <- get
    case V.length (vStack vm) >= count of
        False -> error "VM Error: MAKE_CLOSURE Stack Underflow"
        True -> pushClosure vm addr count

-- | Helper to perform the state restoration for RET.
--
-- @args
--   - vm: Current state.
--   - snap: The snapshot to restore.
--   - rest: The remaining snapshot stack.
--   - v: The return value to push.
--
-- @details
--   Restores IP, FP, Env, and cleans the stack before pushing the return value.
--
execReturn :: VMState -> CallSnapshot -> [CallSnapshot] -> VMValue -> Int ->
    VirtualMachine ()
execReturn vm snap rest v nArgs = put (vm { bytecodeIndex = callbackIndex snap,
    baseVStackIndex = vStackIndex snap, env = vEnv snap, snapshotStack = rest,
    vStack = V.take (baseVStackIndex vm - nArgs) (vStack vm) }) >> stackPush v

-- | Implements RET (Opcode 0x43).
--
-- @details
--   1. Pops the return value from the top of the stack.
--   2. Retrieves and removes the top 'CallSnapshot'.
--   3. "Cleans" the stack by truncating it to the start of the current frame.
--      This effectively discards all local variables.
--   4. Restores the Caller's context (IP and FP).
--   5. Pushes the return value back onto the restored stack.
--
-- @throws
--   Error "VM Error: Return called with empty call stack" if executed in global scope.
--
instRet :: VirtualMachine ()
instRet = do
    nArgs <- readInt32
    retVal <- stackPop
    vm <- get
    case snapshotStack vm of
        [] -> error "VM Error: Return called with empty call stack"
        (snap:rest) -> execReturn vm snap rest retVal nArgs

-- | Implements GET_FUNC_ADDR (Opcode 0x61).
--
-- @details
--   Reads an absolute address (Int) and pushes it as a VFuncPtr.
--   Used to pass functions as values before potentially wrapping them in closures.
--
instGetFuncAddr :: VirtualMachine ()
instGetFuncAddr = do
    addr <- readInt32
    stackPush (VFuncPtr addr)
