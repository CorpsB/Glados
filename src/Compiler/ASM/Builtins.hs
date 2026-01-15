{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Builtins
-}

module Compiler.ASM.Builtins
    ( builtinMap
    ) where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import Compiler.Instruction (Instruction(..))

builtinCastList :: [(Text, Instruction)]
builtinCastList =
    [ (pack "int8", Cast 0x01), (pack "uint8", Cast 0x02)
    , (pack "int16", Cast 0x03), (pack "uint16", Cast 0x04)
    , (pack "int32", Cast 0x05), (pack "uint32", Cast 0x06)
    , (pack "int64", Cast 0x07), (pack "uint64", Cast 0x08)
    , (pack "char", Cast 0x09), (pack "uchar", Cast 0x10) 
    , (pack "char", Cast 0x09), (pack "uchar", Cast 0x10) ]

builtinArithList :: [(Text, Instruction)]
builtinArithList =
    [ (pack "+", Add), (pack "-", Sub), (pack "*", Mul)
    , (pack "div", Div), (pack "mod", Mod) ]

builtinStackList :: [(Text, Instruction)]
builtinStackList =
    [ (pack "cons", Cons), (pack "head", Head),
    (pack "tail", Tail), (pack "nth", Nth) ]

builtinLogicList :: [(Text, Instruction)]
builtinLogicList =
    [ (pack "eq?", Eq), (pack "<", Lt), (pack "<=", Le)
    , (pack "&&", And), (pack "||", Or), (pack "!", Not) ]

builtinSystemList :: [(Text, Instruction)]
builtinSystemList =
    [ (pack "print", Print), (pack "exit", Exit) ]

builtinList :: [(Text, Instruction)]
builtinList = (builtinCastList
    ++ builtinArithList
    ++ builtinStackList
    ++ builtinLogicList
    ++ builtinSystemList)

-- | Mapping of builtin operator names to their VM instructions.
--
builtinMap :: Map.Map Text Instruction
builtinMap = Map.fromList builtinList
