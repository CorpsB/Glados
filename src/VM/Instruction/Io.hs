{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Io Instructions
-}

{-|
Module      : VM.Instruction.Io
Description : System Input/Output primitives.
Stability   : experimental

This module implements low-level file system interactions (Open, Read, Write, Close).
It acts as a bridge between the VM's stack-based memory and the Host OS.
-}
module VM.Instruction.Io
    ( instOpen
    , instRead
    , instWrite
    , instClose
    , instInput
    ) where

import Control.Exception (try, IOException)
import Control.Monad.State.Strict (get, put, liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import System.IO

import VM.VMState (VirtualMachine, VMState(..))
import VM.VMValue (VMValue(..), valueToInt, valueToString, stringToValue)
import VM.VMStack (stackPop, stackPush)
import Common.Type.Integer (IntValue(..))

-- | Wrapper to attempt opening a file safely.
--
-- @arg path: The target file path string.
-- @arg mode: The Handle mode (Read/Write/Append).
--
-- @details
--   Catches any IOException using 'try' to prevent VM crash.
--   Returns an Either type to be handled by the caller.
--
tryOpen :: FilePath -> IOMode -> VirtualMachine (Either IOException Handle)
tryOpen path mode = liftIO $ try (openFile path mode)

-- | Converts an integer to a standard IOMode.
--
-- @arg i: The integer representation (0, 1, 2).
--
-- @details
--   Mapping:
--   1 -> WriteMode
--   2 -> AppendMode
--   Default -> ReadMode
--
getIoMode :: Int -> IOMode
getIoMode 1 = WriteMode
getIoMode 2 = AppendMode
getIoMode _ = ReadMode

-- | Registers a new Handle in the VM state.
--
-- @arg res: The result of the open operation (Handle or Error).
--
-- @details
--   If Success: Assigns a new FD, stores Handle in vmFds, pushes FD.
--   If Error: Pushes -1 to the stack.
--
registerHandle :: Either IOException Handle -> VirtualMachine ()
registerHandle (Left _) = stackPush (VInt (I32 (-1)))
registerHandle (Right h) = do
    vm <- get
    put vm { vmFds = Map.insert (nextFd vm) h (vmFds vm),
        nextFd = nextFd vm + 1 }
    stackPush (VInt (I32 (fromIntegral (nextFd vm))))

-- | Implements OPEN instruction (Opcode 0xB0).
--
-- @arg StackTop: Mode (Int) - 0:Read, 1:Write, 2:Append.
-- @arg StackNext: Path (String).
--
-- @details
--   Pops path and mode, calls tryOpen, and pushes the resulting FD.
--   Safety: Returns -1 on failure instead of crashing.
--
instOpen :: VirtualMachine ()
instOpen = do
    mode <- getIoMode . valueToInt <$> stackPop
    path <- valueToString <$> stackPop
    res <- tryOpen (T.unpack path) mode
    registerHandle res

-- | Wrapper to attempt reading from a handle.
--
-- @arg h: The file handle.
-- @arg size: Number of bytes to read (-1 for all).
--
-- @details
--   Dispatches between 'hGetContents' (full read) and 'readChars'
--   (partial read). Catches IOExceptions.
--
tryRead :: Handle -> Int -> VirtualMachine (Either IOException String)
tryRead h (-1) = liftIO $ try (hGetContents h)
tryRead h size = liftIO $ try (readChars h size)

-- | Helper to read N characters safely.
--
-- @arg h: The file handle.
-- @arg n: The exact number of characters to read.
--
-- @details
--   Recursive read that handles EOF gracefully.
--   Returns partial string if EOF is reached before N chars.
--
readChars :: Handle -> Int -> IO String
readChars _ 0 = return []
readChars h n = do
    eof <- hIsEOF h
    if eof then return [] else (:) <$> hGetChar h <*> readChars h (n - 1)

-- | Handles the result of a read operation.
--
-- @arg res: The result from tryRead (String or Error).
--
-- @details
--   Pushes the content string on success.
--   Pushes an empty list/string on failure.
--
handleReadResult :: Either IOException String -> VirtualMachine ()
handleReadResult (Left _) = stackPush (VList V.empty)
handleReadResult (Right content) = stackPush (stringToValue (T.pack content))

-- | Implements READ instruction (Opcode 0xB1).
--
-- @arg StackTop: Size (Int).
-- @arg StackNext: FD (Int).
--
-- @details
--   Reads 'Size' bytes from the file descriptor 'FD'.
--   Pushes the read content string to the stack.
--
instRead :: VirtualMachine ()
instRead = do
    size <- valueToInt <$> stackPop
    fd <- valueToInt <$> stackPop
    vm <- get
    case Map.lookup fd (vmFds vm) of
        Nothing -> stackPush (VList V.empty)
        Just h -> tryRead h size >>= handleReadResult

-- | Wrapper to attempt writing to a handle.
--
-- @arg h: The file handle.
-- @arg c: The content string to write.
--
-- @details
--   Writes string and forces a flush (hFlush) to ensure
--   output is visible immediately (useful for terminals).
--
tryWrite :: Handle -> String -> VirtualMachine (Either IOException ())
tryWrite h c = liftIO $ try (hPutStr h c >> hFlush h)

-- | Implements WRITE instruction (Opcode 0xB2).
--
-- @arg StackTop: Content (String).
-- @arg StackNext: FD (Int).
--
-- @details
--   Writes content to the specified file descriptor.
--   Ignores operation if FD is invalid.
--
instWrite :: VirtualMachine ()
instWrite = do
    content <- valueToString <$> stackPop
    fd <- valueToInt <$> stackPop
    vm <- get
    case Map.lookup fd (vmFds vm) of
        Just h -> tryWrite h (T.unpack content) >> return ()
        Nothing -> return ()

-- | Implements CLOSE instruction (Opcode 0xB3).
--
-- @arg StackTop: FD (Int).
--
-- @details
--   Closes the handle and removes the FD from the VM state map.
--   Safe to call on invalid FD (no-op).
--
instClose :: VirtualMachine ()
instClose = do
    fd <- valueToInt <$> stackPop
    vm <- get
    case Map.lookup fd (vmFds vm) of
        Just h -> liftIO (hClose h) >> removeFd vm fd
        Nothing -> return ()

-- | Removes a File Descriptor from the VM state.
--
-- @arg vm: The current VM State.
-- @arg fd: The File Descriptor to remove.
--
-- @details
--   Updates the Map in the state to delete the key.
--
removeFd :: VMState -> Int -> VirtualMachine ()
removeFd vm fd = put vm { vmFds = Map.delete fd (vmFds vm) }

-- | Wrapper to attempt reading a line.
--
-- @arg h: The file handle.
--
-- @details
--   Uses hGetLine to read until newline.
--   Catches EOF or other IO errors.
--
tryInput :: Handle -> VirtualMachine (Either IOException String)
tryInput h = liftIO $ try (hGetLine h)

-- | Implements INPUT instruction (Opcode 0xB4).
--
-- @arg StackTop: FD (Int).
--
-- @details
--   Reads a single line from the FD (useful for stdin).
--   Pushes the string content (without newline usually) to stack.
--
instInput :: VirtualMachine ()
instInput = do
    fd <- valueToInt <$> stackPop
    vm <- get
    case Map.lookup fd (vmFds vm) of
        Nothing -> stackPush (VList V.empty)
        Just h -> tryInput h >>= handleReadResult
