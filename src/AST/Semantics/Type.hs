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
) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- | Enumeration of all possible types in the language.
data Type
    = TyInt                -- Primitive integer type (int)
    | TyBool               -- Primitive boolean type (bool)
    | TyVoid               -- Void type (void)
    | TyList Type          -- List type (e.g., [int], [[char]])
    | TyFunc [Type] Type   -- Function type (Args -> Return)
    | TyStruct T.Text      -- Custom structure type identified by name
    | TyAuto               -- Placeholder for type inference
    deriving (Show)

-- | Definition of a structure (Name + Field Map).
data StructDef = StructDef
    { structName :: T.Text
    , structFields :: Map.Map T.Text Type
    } deriving (Show)

-- | Verification Environment.
data CheckEnv = CheckEnv
    { envVars :: Map.Map T.Text Type        -- Symbol table for variables
    , envStructs :: Map.Map T.Text StructDef -- Symbol table for structs
    } deriving (Show)

-- | Initial empty environment.
emptyEnv :: CheckEnv
emptyEnv = CheckEnv Map.empty Map.empty

-- | Parses a raw type string (from Parser) into a Semantic Type.
parseType :: T.Text -> Type
parseType t
    | t == T.pack "int"  = TyInt
    | t == T.pack "bool" = TyBool
    | t == T.pack "void" = TyVoid
    | t == T.pack "auto" = TyAuto
    | T.isPrefixOf (T.pack "[") t && T.isSuffixOf (T.pack "]") t =
        let inner = T.init (T.tail t)
        in TyList (parseType inner)
    | otherwise = TyStruct t

-- | Helper to convert a Semantic Type back to a readable String.
typeToString :: Type -> String
typeToString TyInt = "int"
typeToString TyBool = "bool"
typeToString TyVoid = "void"
typeToString TyAuto = "auto"
typeToString (TyList t) = "[" ++ typeToString t ++ "]"
typeToString (TyStruct n) = T.unpack n
typeToString (TyFunc args ret) = 
    "(" ++ unwords (map typeToString args) ++ " -> " ++ typeToString ret ++ ")"
