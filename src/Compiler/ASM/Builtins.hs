{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Builtins
-}

module Compiler.ASM.Builtins
    ( builtinMap
    , getInnerListType
    , getBuiltinReturnType
    ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Compiler.Instruction (Instruction(..))

builtinCastList :: [(Text, Instruction)]
builtinCastList =
    [ (pack "int8", Cast 0x01), (pack "uint8", Cast 0x02)
    , (pack "int16", Cast 0x03), (pack "uint16", Cast 0x04)
    , (pack "int32", Cast 0x05), (pack "uint32", Cast 0x06)
    , (pack "int64", Cast 0x07), (pack "uint64", Cast 0x08)
    , (pack "char", Cast 0x09), (pack "uchar", Cast 0x10) ]

builtinArithList :: [(Text, Instruction)]
builtinArithList =
    [ (pack "+", Add), (pack "-", Sub), (pack "*", Mul)
    , (pack "div", Div), (pack "mod", Mod) ]

builtinStackList :: [(Text, Instruction)]
builtinStackList =
    [ (pack "cons", Cons), (pack "head", Head),
    (pack "tail", Tail), (pack "nth", Nth),
    (pack "nth_update", NthUpdate), (pack "attr_update", AttrUpdate) ]

builtinLogicList :: [(Text, Instruction)]
builtinLogicList =
    [ (pack "eq?", Eq), (pack "teq?", TEq)
    , (pack "<", Lt), (pack "<=", Le)
    , (pack "&&", And), (pack "||", Or), (pack "!", Not) ]

builtinIoList :: [(Text, Instruction)]
builtinIoList =
    [ (pack "open", Open), (pack "read", Read)
    , (pack "write", Write), (pack "close", Close)
    , (pack "input", Input) ]

builtinSystemList :: [(Text, Instruction)]
builtinSystemList =
    [ (pack "typeof", TypeOf), (pack "print", Print)
    , (pack "exit", Exit) ]

builtinList :: [(Text, Instruction)]
builtinList = (builtinCastList
    ++ builtinArithList
    ++ builtinStackList
    ++ builtinLogicList
    ++ builtinIoList
    ++ builtinSystemList)

-- | Mapping of builtin operator names to their VM instructions.
--
builtinMap :: Map.Map Text Instruction
builtinMap = Map.fromList builtinList

-- | Helper to transform "[int]" to "int" or "[Point]" to "Point"
--
getInnerListType :: Text -> Either Text Text
getInnerListType t
    | T.length t > 2 && T.head t == '[' && T.last t == ']' =
        Right (T.tail (T.init t))
    | otherwise = Left (pack $ "Expected a list type [...], got: " ++ show t)

-- | Returns the return type of a builtin based on its name and argument types.
--
getBuiltinReturnType :: String -> [Text] -> Either Text Text
getBuiltinReturnType "nth" [listType, _] = getInnerListType listType
getBuiltinReturnType "head" [listType] = getInnerListType listType
getBuiltinReturnType "tail" [listType] = Right listType
getBuiltinReturnType "cons" [_, listType] = Right listType

getBuiltinReturnType "+" _ = Right (pack "int")
getBuiltinReturnType "-" _ = Right (pack "int")
getBuiltinReturnType "*" _ = Right (pack "int")
getBuiltinReturnType "/" _ = Right (pack "int")
getBuiltinReturnType "%" _ = Right (pack "int")
getBuiltinReturnType "div" _ = Right (pack "int")
getBuiltinReturnType "mod" _ = Right (pack "int")

getBuiltinReturnType "eq?" _   = Right (pack "bool")
getBuiltinReturnType "neq?" _  = Right (pack "bool")
getBuiltinReturnType "teq?" _  = Right (pack "bool")
getBuiltinReturnType "tneq?" _ = Right (pack "bool")
getBuiltinReturnType "<" _     = Right (pack "bool")
getBuiltinReturnType ">" _     = Right (pack "bool")
getBuiltinReturnType "<=" _    = Right (pack "bool")
getBuiltinReturnType ">=" _    = Right (pack "bool")
getBuiltinReturnType "!" _     = Right (pack "bool")
getBuiltinReturnType "&&" _    = Right (pack "bool")
getBuiltinReturnType "||" _    = Right (pack "bool")

getBuiltinReturnType "open" _ = Right (pack "int")
getBuiltinReturnType "close" _ = Right (pack "int")
getBuiltinReturnType "read" _ = Right (pack "[char]")
getBuiltinReturnType "write" _ = Right (pack "int")
getBuiltinReturnType "ffread" _ = Right (pack "[[char]]")
getBuiltinReturnType "ffwrite" _ = Right (pack "bool")
getBuiltinReturnType "typeof" _ = Right (pack "[char]")

getBuiltinReturnType name _ = Left (
    pack $ "Unknown builtin or invalid arguments for: " ++ show name)
