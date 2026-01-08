{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Semantic Types definition
-}

module AST.Semantics.Type (
    Type(..),
    StructDef(..),
    CheckEnv(..),
    emptyEnv,
    parseType,
    typeToString,
    areTypesCompatible,
) where

import qualified Data.Text as DT
import qualified Data.Map.Strict as Map

-- | Enumeration of all possible types in the language.
data Type
    = TyInt                -- Primitive integer type (int)
    | TyBool               -- Primitive boolean type (bool)
    | TyVoid               -- Void type (void)
    | TyList Type          -- List type (e.g., [int], [[char]])
    | TyFunc [Type] Type   -- Function type (Args -> Return)
    | TyStruct DT.Text      -- Custom structure type identified by name
    | TyAuto               -- Placeholder for type inference
    deriving (Show)

-- | Definition of a structure (Name + Field Map).
data StructDef = StructDef
    { structName :: DT.Text
    , structFields :: Map.Map DT.Text Type
    } deriving (Show)

-- | Verification Environment.
data CheckEnv = CheckEnv
    { envVars :: Map.Map DT.Text Type        -- Symbol table for variables
    , envStructs :: Map.Map DT.Text StructDef -- Symbol table for structs
    } deriving (Show)

-- | Initial empty environment.
emptyEnv :: CheckEnv
emptyEnv = CheckEnv Map.empty Map.empty

-- | Parses a raw type string (from Parser) into a Semantic Type.
parseType :: DT.Text -> Type
parseType t
    | t == DT.pack "int"  = TyInt
    | t == DT.pack "bool" = TyBool
    | t == DT.pack "void" = TyVoid
    | t == DT.pack "auto" = TyAuto
    | DT.isPrefixOf (DT.pack "[") t && DT.isSuffixOf (DT.pack "]") t =
        let inner = DT.init (DT.tail t)
        in TyList (parseType inner)
    | otherwise = TyStruct t

-- | Helper to convert a Semantic Type back to a readable String.
typeToString :: Type -> String
typeToString TyInt = "int"
typeToString TyBool = "bool"
typeToString TyVoid = "void"
typeToString TyAuto = "auto"
typeToString (TyList t) = "[" ++ typeToString t ++ "]"
typeToString (TyStruct n) = DT.unpack n
typeToString (TyFunc args ret) = 
    "(" ++ unwords (map typeToString args) ++ " -> " ++ typeToString ret ++ ")"

-- | Checks if two types are semantically compatible.
-- Replaces usage of (==) with explicit pattern matching.
areTypesCompatible :: Type -> Type -> Bool
areTypesCompatible TyInt TyInt   = True
areTypesCompatible TyBool TyBool = True
areTypesCompatible TyVoid TyVoid = True
areTypesCompatible TyAuto TyAuto = True
areTypesCompatible (TyList a) (TyList b) = areTypesCompatible a b
areTypesCompatible (TyStruct a) (TyStruct b) = a == b
areTypesCompatible (TyFunc args1 ret1) (TyFunc args2 ret2) = 
    length args1 == length args2 &&
    and (zipWith areTypesCompatible args1 args2) &&
    areTypesCompatible ret1 ret2
areTypesCompatible _ _ = False
