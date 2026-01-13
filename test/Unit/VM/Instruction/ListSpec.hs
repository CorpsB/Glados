{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- ListSpec
-}

module VM.Instruction.ListSpec (spec) where

import Test.Hspec
import Control.Exception (ErrorCall(..))
import Control.Monad.State.Strict (runStateT)
import Data.List (isInfixOf)
import qualified Data.Vector as V

import qualified Common.Type.Integer as I
import VM.VMState (VMState, VirtualMachine, createVMState)
import VM.VMValue (VMValue(..))
import VM.VMStack (stackPush, stackPop, stackTop)
import VM.Instruction.List (instCons, instHead, instTail)

-- helper: int VMValue
i :: Integer -> VMValue
i n = VInt (I.fitInteger (fromInteger n))

mkEmptyVM :: VMState
mkEmptyVM = createVMState mempty

runVM :: VirtualMachine a -> IO (a, VMState)
runVM act = runStateT act mkEmptyVM

spec :: Spec
spec = describe "VM.Instruction.List" $ do

  describe "instCons (CONS 0x90)" $ do
    it "cons: pushes a new list with element at head" $ do
      let base = i 0
          x    = i 10
          xs   = VList (V.fromList [i 1, i 2])

      ((out, baseTop), _) <- runVM $ do
        -- IMPORTANT: CONS expects the 2nd arg (the list) on TOP of the stack
        -- so push element first, then the list.
        mapM_ stackPush [base, x, xs]
        instCons
        res <- stackPop
        b   <- stackTop
        pure (res, b)

      baseTop `shouldBe` base
      out `shouldBe` VList (V.fromList [x, i 1, i 2])

    it "cons: throws when second operand is not a list" $ do
      let base    = i 0
          x       = i 10
          notList = i 99

      runVM (do
        -- 2nd arg is TOP -> put a non-list on top to trigger the error
        mapM_ stackPush [base, x, notList]
        instCons
        )
        `shouldThrow` \(ErrorCall msg) ->
          "VM Error: CONS expects a List as second argument" `isInfixOf` msg

  describe "instHead (HEAD 0x91)" $ do
    it "head: pushes the first element (list is consumed)" $ do
      let base = i 0
          xs   = VList (V.fromList [i 7, i 8])

      ((h, baseTop), _) <- runVM $ do
        mapM_ stackPush [base, xs]
        instHead
        headVal <- stackPop
        b       <- stackTop
        pure (headVal, b)

      baseTop `shouldBe` base
      h `shouldBe` i 7

    it "head: throws on empty list" $ do
      let base  = i 0
          empty = VList V.empty

      runVM (do
        mapM_ stackPush [base, empty]
        instHead
        )
        `shouldThrow` \(ErrorCall msg) ->
          "VM Error: HEAD called on empty list" `isInfixOf` msg

    it "head: throws on non-list" $ do
      let base = i 0
          v    = i 5

      runVM (do
        mapM_ stackPush [base, v]
        instHead
        )
        `shouldThrow` \(ErrorCall msg) ->
          "VM Error: HEAD expects a List" `isInfixOf` msg

  describe "instTail (TAIL 0x92)" $ do
    it "tail: pushes the tail list (list is consumed)" $ do
      let base = i 0
          xs   = VList (V.fromList [i 7, i 8, i 9])

      ((t, baseTop), _) <- runVM $ do
        mapM_ stackPush [base, xs]
        instTail
        tailVal <- stackPop
        b       <- stackTop
        pure (tailVal, b)

      baseTop `shouldBe` base
      t `shouldBe` VList (V.fromList [i 8, i 9])

    it "tail: throws on empty list" $ do
      let base  = i 0
          empty = VList V.empty

      runVM (do
        mapM_ stackPush [base, empty]
        instTail
        )
        `shouldThrow` \(ErrorCall msg) ->
          "VM Error: TAIL called on empty list" `isInfixOf` msg

    it "tail: throws on non-list" $ do
      let base = i 0
          v    = i 5

      runVM (do
        mapM_ stackPush [base, v]
        instTail
        )
        `shouldThrow` \(ErrorCall msg) ->
          "VM Error: TAIL expects a List" `isInfixOf` msg
