{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Io instructions unit tests (compilable without filepath/time/temp)
-}

{-# LANGUAGE OverloadedStrings #-}

module VM.Instruction.IoSpec (spec) where

import Test.Hspec
import Control.Exception (try, SomeException, bracket)
import Control.Monad.State.Strict (runStateT)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS

import System.Directory
  ( doesFileExist
  , getTemporaryDirectory
  , createDirectory
  , removePathForcibly
  , removeFile
  )
import System.IO (openFile, hClose, IOMode(..), openTempFile)

import VM.VMState (VirtualMachine, VMState(..), createVMState)
import VM.VMValue (VMValue(..), valueToInt, valueToString, stringToValue)
import VM.VMStack (stackPop, stackPush)
import Common.Type.Integer (IntValue(..))

import VM.Instruction.Io (instOpen, instRead, instWrite, instClose, instInput)

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
  base <- getTemporaryDirectory
  (tmpPath, h) <- openTempFile base (prefix ++ ".tmp")
  hClose h
  removeFile tmpPath
  createDirectory tmpPath
  bracket (pure tmpPath) removePathForcibly action

mkPath :: FilePath -> String -> FilePath
mkPath dir name = dir ++ "/" ++ name

initState :: VMState
initState = createVMState BS.empty False

i32 :: Int -> VMValue
i32 n = VInt (I32 (fromIntegral n))

pushStr :: String -> VirtualMachine ()
pushStr s = stackPush (stringToValue (T.pack s))

popInt :: VirtualMachine Int
popInt = valueToInt <$> stackPop

runVM :: VirtualMachine a -> IO (a, VMState)
runVM act = runStateT act initState

doOpen :: FilePath -> Int -> VirtualMachine Int
doOpen path mode = do
  pushStr path
  stackPush (i32 mode)
  instOpen
  popInt

doRead :: Int -> Int -> VirtualMachine VMValue
doRead fd size = do
  stackPush (i32 fd)
  stackPush (i32 size)
  instRead
  stackPop

doWrite :: Int -> String -> VirtualMachine ()
doWrite fd s = do
  stackPush (i32 fd)
  pushStr s
  instWrite

doClose :: Int -> VirtualMachine ()
doClose fd = stackPush (i32 fd) >> instClose

doInput :: Int -> VirtualMachine VMValue
doInput fd = do
  stackPush (i32 fd)
  instInput
  stackPop

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

spec :: Spec
spec = describe "VM.Instruction.Io (compilable tests)" $ do

  describe "instOpen (covers getIoMode + registerHandle branches)" $ do
    it "mode other than 1/2 behaves like ReadMode: open non-existent file => pushes -1" $ do
      (fd, _st) <- runVM (doOpen "/this/path/should/not/exist_424242" 0)
      fd `shouldBe` (-1)

    it "mode 1 behaves like WriteMode: creates file and returns a valid fd" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "out.txt"
        (fd, st) <- runVM (doOpen fp 1)
        fd `shouldSatisfy` (>= 0)
        Map.member fd (vmFds st) `shouldBe` True
        exists <- doesFileExist fp
        exists `shouldBe` True

    it "mode 2 behaves like AppendMode: allows appending (via write)" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "append.txt"
        (_res, _st) <- runVM $ do
          fd1 <- doOpen fp 1
          doWrite fd1 "A"
          doClose fd1
          fd2 <- doOpen fp 2
          doWrite fd2 "B"
          doClose fd2
        content <- readFile fp
        content `shouldBe` "AB"

  describe "instRead (covers fd lookup + tryRead branches + handleReadResult)" $ do
    it "invalid fd => pushes empty list" $ do
      (val, _st) <- runVM (doRead 424242 10)
      val `shouldBe` VList V.empty

    it "size = -1 reads all content (do NOT close fd: hGetContents is lazy)" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "readall.txt"
        writeFile fp "hello"
        (txt, _st) <- runVM $ do
          fd <- doOpen fp 0
          v <- doRead fd (-1)
          pure (valueToString v)
        txt `shouldBe` "hello"

    it "size > 0 reads N chars" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "readn.txt"
        writeFile fp "abcdef"
        (txt, _st) <- runVM $ do
          fd <- doOpen fp 0
          v <- doRead fd 3
          doClose fd
          pure (valueToString v)
        txt `shouldBe` "abc"

    it "size > file length returns partial string (EOF early)" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "eof.txt"
        writeFile fp "hi"
        (txt, _st) <- runVM $ do
          fd <- doOpen fp 0
          v <- doRead fd 10
          doClose fd
          pure (valueToString v)
        txt `shouldBe` "hi"

    it "closed handle triggers IOException => pushes empty list" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "closedread.txt"
        writeFile fp "zzz"
        h <- openFile fp ReadMode
        hClose h

        let st0 = initState { vmFds = Map.singleton 7 h, nextFd = 8 }
        (val, _st) <- runStateT (doRead 7 (-1)) st0
        val `shouldBe` VList V.empty

  describe "instWrite (covers fd lookup + tryWrite success/failure)" $ do
    it "invalid fd => no-op (does not crash)" $ do
      res <- try (runVM (doWrite 999 "X")) :: IO (Either SomeException ((), VMState))
      isRight res `shouldBe` True

    it "valid fd writes content to file" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "write.txt"
        (_res, _st) <- runVM $ do
          fd <- doOpen fp 1
          doWrite fd "hello"
          doClose fd
        content <- readFile fp
        content `shouldBe` "hello"

    it "closed handle makes tryWrite fail but instWrite must not crash" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "closedwrite.txt"
        writeFile fp ""
        h <- openFile fp WriteMode
        hClose h
        let st0 = initState { vmFds = Map.singleton 3 h, nextFd = 4 }

        res <- try (runStateT (doWrite 3 "boom") st0) :: IO (Either SomeException ((), VMState))
        isRight res `shouldBe` True

  describe "instClose (covers Just/Nothing branches)" $ do
    it "invalid fd => no-op" $ do
      res <- try (runVM (doClose 424242)) :: IO (Either SomeException ((), VMState))
      isRight res `shouldBe` True

    it "valid fd closes and removes handle from vmFds" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "close.txt"
        (fd, st) <- runVM $ do
          fd <- doOpen fp 1
          doClose fd
          pure fd
        Map.member fd (vmFds st) `shouldBe` False

  describe "instInput (covers fd lookup + tryInput success/failure)" $ do
    it "invalid fd => pushes empty list" $ do
      (val, _st) <- runVM (doInput 12345)
      val `shouldBe` VList V.empty

    it "reads one line from fd (success)" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "input.txt"
        writeFile fp "line1\nline2\n"
        (txt, _st) <- runVM $ do
          fd <- doOpen fp 0
          v <- doInput fd
          doClose fd
          pure (valueToString v)
        txt `shouldBe` "line1"

    it "closed handle triggers IOException => pushes empty list" $
      withTempDirectory "glados-io" $ \dir -> do
        let fp = mkPath dir "closedinput.txt"
        writeFile fp "a\n"
        h <- openFile fp ReadMode
        hClose h
        let st0 = initState { vmFds = Map.singleton 9 h, nextFd = 10 }

        (val, _st) <- runStateT (doInput 9) st0
        val `shouldBe` VList V.empty
