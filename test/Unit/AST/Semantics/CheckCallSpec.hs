{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Unit tests for CheckCall
-}

{-# LANGUAGE LambdaCase #-}

module AST.Semantics.CheckCallSpec (spec) where

import Test.Hspec
import AST.Semantics.CheckCall
import AST.Semantics.Type
import AST.Ast (Ast(..))
import Common.Type.Integer (IntValue(..), fitInteger)
import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import AST.Semantics.Check (checkExpr)

p :: String -> DT.Text
p = DT.pack

testEnv :: CheckEnv
testEnv = CheckEnv 
    (Map.fromList [
        (p "i", TyInt),
        (p "b", TyBool),
        (p "add", TyFunc [TyInt, TyInt] TyInt),
        (p "isZero", TyFunc [TyInt] TyBool),
        (p "notAFunc", TyInt)
    ]) 
    Map.empty

mockCheckExpr :: CheckEnv -> Ast -> Either String Type
mockCheckExpr _ (AInteger _) = Right TyInt
mockCheckExpr _ (ABool _) = Right TyBool
mockCheckExpr env (ASymbol name) = case Map.lookup name (envVars env) of
    Just t -> Right t
    Nothing -> Left $ "Undefined variable '" ++ DT.unpack name ++ "'"
mockCheckExpr _ _ = Left "Mock error: expression not supported in unit test"

spec :: Spec
spec = describe "AST.Semantics.CheckCall" $ do

    describe "Arithmetic Operators (+, -, *, div, mod)" $ do
        it "Validates int + int" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "+")) args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Validates division" $ do
            let args = [AInteger (fitInteger 10), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "div")) args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Rejects int + bool" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "+")) args `shouldSatisfy` \case
                Left err -> "expects (int, int)" `isInfixOf` err
                _ -> False

        it "Rejects wrong argument count" $ do
            let args = [AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "+")) args `shouldSatisfy` \case
                Left err -> "expects 2 arguments" `isInfixOf` err
                _ -> False

    describe "Comparison Operators (<, >, <=, >=)" $ do
        it "Validates int < int" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "<")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects bool < int" $ do
            let args = [ABool True, AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "<")) args `shouldSatisfy` \case
                Left err -> "expects (int, int)" `isInfixOf` err
                _ -> False

    describe "Equality Operator (eq?)" $ do
        it "Validates int == int" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "eq?")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Validates bool == bool" $ do
            let args = [ABool True, ABool False]
            checkCall mockCheckExpr testEnv (ASymbol (p "eq?")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects int == bool (Incompatible types)" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "eq?")) args `shouldSatisfy` \case
                Left err -> "compatible types" `isInfixOf` err
                _ -> False

    describe "Logic Operators (&&, ||)" $ do
        it "Validates bool && bool" $ do
            let args = [ABool True, ABool False]
            checkCall mockCheckExpr testEnv (ASymbol (p "&&")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects bool && int" $ do
            let args = [ABool True, AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "&&")) args `shouldSatisfy` \case
                Left err -> "expects (bool, bool)" `isInfixOf` err
                _ -> False

    describe "Unary Operator (!)" $ do
        it "Validates !bool" $ do
            let args = [ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "!")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects !int" $ do
            let args = [AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "!")) args `shouldSatisfy` \case
                Left err -> "expects bool" `isInfixOf` err
                _ -> False
        
        it "Rejects unary op with 2 arguments" $ do
            let args = [ABool True, ABool False]
            checkCall mockCheckExpr testEnv (ASymbol (p "!")) args `shouldSatisfy` \case
                Left err -> "expects 1 argument" `isInfixOf` err
                _ -> False

    describe "User Defined Functions" $ do
        it "Validates correct function call (add)" $ do
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv (ASymbol (p "add")) args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Validates correct function call (isZero)" $ do
            let args = [AInteger (fitInteger 0)]
            checkCall mockCheckExpr testEnv (ASymbol (p "isZero")) args `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "Rejects invalid argument count" $ do
            let args = [AInteger (fitInteger 1)]
            checkCall mockCheckExpr testEnv (ASymbol (p "add")) args `shouldSatisfy` \case
                Left err -> "expects 2 arguments" `isInfixOf` err
                _ -> False

        it "Rejects invalid argument types" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            checkCall mockCheckExpr testEnv (ASymbol (p "add")) args `shouldSatisfy` \case
                Left err -> "Argument type mismatch" `isInfixOf` err
                _ -> False

        it "Rejects undefined function" $ do
            let args = []
            checkCall mockCheckExpr testEnv (ASymbol (p "unknown")) args `shouldSatisfy` \case
                Left err -> "Undefined function" `isInfixOf` err
                _ -> False

        it "Rejects calling a non-function variable" $ do
            let args = []
            checkCall mockCheckExpr testEnv (ASymbol (p "notAFunc")) args `shouldSatisfy` \case
                Left err -> "is not a function" `isInfixOf` err
                _ -> False
    
    describe "Edge Cases" $ do
        it "Handles APos wrapper on function name transparently" $ do
            let funcNode = APos 1 1 (ASymbol (p "+"))
            let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkCall mockCheckExpr testEnv funcNode args `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "Rejects invalid call node (not a symbol)" $ do
            let funcNode = AInteger (fitInteger 1)
            checkCall mockCheckExpr testEnv funcNode [] `shouldSatisfy` \case
                Left err -> "Invalid function call" `isInfixOf` err
                _ -> False

    describe "CheckCall and Environment Propagation" $ do
        
        it "checkCall: passes env to checkUserFunc (resolves user function)" $ do
            let funcName = DT.pack "myFunc"
            let funcType = TyFunc [TyInt] TyInt
            let funcEnv = emptyEnv { 
                envVars = Map.singleton funcName funcType 
            }
            let call = ACall (ASymbol funcName) [AInteger (fitInteger 10)]
            
            checkExpr funcEnv call `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "checkCall: handles APos wrapper recursively" $ do
            let funcName = DT.pack "f"
            let funcType = TyFunc [] TyVoid
            let posEnv = emptyEnv { 
                envVars = Map.singleton funcName funcType 
            }
            let call = ACall (APos 1 1 (ASymbol funcName)) []
            
            checkExpr posEnv call `shouldSatisfy` \case
                Right TyVoid -> True
                _ -> False

        it "checkMathComp: passes e (env) to resolve variables in arguments" $ do
            let varName = DT.pack "x"
            let mathEnv = emptyEnv { 
                envVars = Map.singleton varName TyInt 
            }
            let call = ACall (ASymbol (DT.pack "+")) [ASymbol varName, AInteger (fitInteger 1)]
            
            checkExpr mathEnv call `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "checkMathComp: passes e (env) to comparison operators" $ do
            let varName = DT.pack "y"
            let compEnv = emptyEnv { 
                envVars = Map.singleton varName TyInt 
            }
            let call = ACall (ASymbol (DT.pack "<")) [ASymbol varName, AInteger (fitInteger 10)]
            
            checkExpr compEnv call `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

    describe "Logic and Special Operators (checkLogicSpecial)" $ do

        it "passes env to logic operators (&&) to resolve variables" $ do
            let varName = DT.pack "b"
            let logicEnv = emptyEnv { 
                envVars = Map.singleton varName TyBool 
            }
            let call = ACall (ASymbol (DT.pack "&&")) [ASymbol varName, ASymbol varName]
            
            checkExpr logicEnv call `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "passes env to equality operator (eq?)" $ do
            let varName = DT.pack "val"
            let eqEnv = emptyEnv { 
                envVars = Map.singleton varName TyInt 
            }
            let call = ACall (ASymbol (DT.pack "eq?")) [ASymbol varName, AInteger (fitInteger 42)]
            
            checkExpr eqEnv call `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "passes env to unary operator (!)" $ do
            let varName = DT.pack "flag"
            let unaryEnv = emptyEnv { 
                envVars = Map.singleton varName TyBool 
            }
            let call = ACall (ASymbol (DT.pack "!")) [ASymbol varName]
            
            checkExpr unaryEnv call `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "passes env to set_field to resolve struct definition and variable" $ do
            let sName = DT.pack "MyStruct"
            let fName = DT.pack "f"
            let varName = DT.pack "s"
            
            let sDef = StructDef sName (Map.singleton fName TyInt)
            let structEnv = emptyEnv {
                envStructs = Map.singleton sName sDef,
                envVars = Map.singleton varName (TyStruct sName)
            }
            
            let fieldNameAst = AList [AInteger (IChar 102)]
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol varName, fieldNameAst, AInteger (fitInteger 123)]

            checkExpr structEnv call `shouldSatisfy` \case
                Right (TyStruct name) | name == sName -> True
                _ -> False
    
    describe "checkSetField and checkFieldInStruct Logic" $ do
        
        let sName = DT.pack "Point"
        let sDef = StructDef sName (Map.singleton (DT.pack "x") TyInt)
        
        let fieldEnv = emptyEnv {
            envStructs = Map.singleton sName sDef,
            envVars = Map.fromList [
                (DT.pack "p", TyStruct sName),
                (DT.pack "num", TyInt)
            ]
        }

        it "checkSetField: fails when first argument is not a struct" $ do
            let fieldName = AList [AInteger (IChar 120)]
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "num"), fieldName, AInteger (fitInteger 10)]
            
            checkExpr fieldEnv call `shouldSatisfy` \case
                Left err | err == "set_field expects a structure as first argument" -> True
                _ -> False

        it "checkSetField: fails when argument count is incorrect" $ do
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "p")]
            
            checkExpr fieldEnv call `shouldSatisfy` \case
                Left err | err == "set_field expects 3 arguments" -> True
                _ -> False

        it "checkFieldInStruct: fails when field does not exist in struct" $ do
            let fieldZ = AList [AInteger (IChar 122)]
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "p"), fieldZ, AInteger (fitInteger 10)]
            
            checkExpr fieldEnv call `shouldSatisfy` \case
                Left err | err == "Field 'z' not found in struct 'Point'" -> True
                _ -> False
    
    describe "validateAssignment Logic" $ do
        
        let sName = DT.pack "Point"
        let fName = DT.pack "x"
        let sDef = StructDef sName (Map.singleton fName TyInt)
        
        let fieldEnv = emptyEnv {
            envStructs = Map.singleton sName sDef,
            envVars = Map.fromList [
                (DT.pack "p", TyStruct sName),
                (DT.pack "y", TyInt),
                (DT.pack "b", TyBool) 
            ]
        }
        let fNameAst = AList [AInteger (IChar 120)] 

        it "validateAssignment: succeeds and uses env to resolve value type" $ do
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "p"), fNameAst, ASymbol (DT.pack "y")]
            
            checkExpr fieldEnv call `shouldSatisfy` \case
                Right (TyStruct n) | n == sName -> True
                _ -> False

        it "validateAssignment: fails with specific error message on type mismatch" $ do
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "p"), fNameAst, ASymbol (DT.pack "b")]
            
            let expectedErr = "Type mismatch in field assignment 'x'. Expected int, got bool"

            checkExpr fieldEnv call `shouldSatisfy` \case
                Left err | err == expectedErr -> True
                _ -> False
    
    describe "Internal Helpers and Binary Op Validation" $ do

        it "extractStringFromAst: returns error for invalid list content (non-char)" $ do
            let badString = AList [AInteger (fitInteger 65), AInteger (fitInteger 10)] 
            let sDef = StructDef (DT.pack "S") (Map.singleton (DT.pack "f") TyInt)
            let env = emptyEnv {
                envStructs = Map.singleton (DT.pack "S") sDef,
                envVars = Map.singleton (DT.pack "s") (TyStruct (DT.pack "S"))
            }
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "s"), badString, AInteger (fitInteger 1)]
            
            checkExpr env call `shouldSatisfy` \case
                Left err | err == "Invalid string format in AST" -> True
                _ -> False

        it "extractStringFromAst: returns error for invalid node type (not AList)" $ do
            let notAList = AInteger (fitInteger 123)
            let sDef = StructDef (DT.pack "S") (Map.singleton (DT.pack "f") TyInt)
            let env = emptyEnv {
                envStructs = Map.singleton (DT.pack "S") sDef,
                envVars = Map.singleton (DT.pack "s") (TyStruct (DT.pack "S"))
            }
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "s"), notAList, AInteger (fitInteger 1)]
            
            checkExpr env call `shouldSatisfy` \case
                Left err | err == "Invalid field name format in set_field" -> True
                _ -> False

        it "getStructDef: returns error when struct definition is missing in env" $ do
            let env = emptyEnv {
                envVars = Map.singleton (DT.pack "ghost") (TyStruct (DT.pack "GhostStruct"))
            }
            let fieldName = AList [AInteger (IChar 120)]
            let call = ACall (ASymbol (DT.pack "set_field")) 
                             [ASymbol (DT.pack "ghost"), fieldName, AInteger (fitInteger 1)]
            
            checkExpr env call `shouldSatisfy` \case
                Left err | err == "Error: Undefined struct 'GhostStruct'" -> True
                _ -> False

        it "validateBinaryOp: uses env to resolve left and right operands (tLeft, tRight)" $ do
            let env = emptyEnv {
                envVars = Map.fromList [
                    (DT.pack "lhs", TyInt),
                    (DT.pack "rhs", TyInt)
                ]
            }
            let call = ACall (ASymbol (DT.pack "+")) 
                             [ASymbol (DT.pack "lhs"), ASymbol (DT.pack "rhs")]
            
            checkExpr env call `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

    
    describe "Equality and Unary Operator Validation" $ do

        let x = ASymbol (DT.pack "x")
        let b = ASymbol (DT.pack "b")
        let i = ASymbol (DT.pack "i")
        
        let env = emptyEnv {
            envVars = Map.fromList [
                (DT.pack "x", TyInt),
                (DT.pack "i", TyInt),
                (DT.pack "b", TyBool)
            ]
        }

        it "checkEqualityOp: uses env to validate compatible types (int vs int)" $ do
            let call = ACall (ASymbol (DT.pack "eq?")) [x, i]
            checkExpr env call `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "checkEqualityOp: fails with specific message for incompatible types" $ do
            let call = ACall (ASymbol (DT.pack "eq?")) [x, b]
            let expected = "Equality requires compatible types, got int and bool"
            checkExpr env call `shouldSatisfy` \case
                Left err | err == expected -> True
                _ -> False

        it "checkEqualityOp: fails on incorrect argument count" $ do
            let call = ACall (ASymbol (DT.pack "eq?")) [x]
            checkExpr env call `shouldSatisfy` \case
                Left err | err == "Equality operator expects 2 arguments" -> True
                _ -> False

        it "checkUnaryOp: uses env to validate operand (success)" $ do
            let call = ACall (ASymbol (DT.pack "!")) [b]
            checkExpr env call `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "checkUnaryOp: fails with specific message 'but got' on type mismatch" $ do
            let call = ACall (ASymbol (DT.pack "!")) [i]
            let expected = "Operator '!' expects bool but got int"
            checkExpr env call `shouldSatisfy` \case
                Left err | err == expected -> True
                _ -> False

        it "checkUnaryOp: fails on incorrect argument count" $ do
            let call = ACall (ASymbol (DT.pack "!")) [b, b]
            checkExpr env call `shouldSatisfy` \case
                Left err | err == "Operator '!' expects 1 argument" -> True
                _ -> False
    
    describe "User Function and Binary Error Formatting" $ do

        it "validateBinaryOp: formats error message with actual types (tLeft, tRight)" $ do
            let call = ACall (ASymbol (DT.pack "+")) 
                             [AInteger (fitInteger 1), ABool True]
            
            checkExpr emptyEnv call `shouldSatisfy` \case
                Left err | "(int, bool)" `isInfixOf` err -> True
                _ -> False

        it "checkUserFunc: uses env to detect symbol is not a function" $ do
            let varName = DT.pack "myVar"
            let env = emptyEnv { 
                envVars = Map.singleton varName TyInt 
            }
            let call = ACall (ASymbol varName) []
            
            checkExpr env call `shouldSatisfy` \case
                Left err | err == "'myVar' is not a function" -> True
                _ -> False

        it "checkUserFunc: uses env to detect undefined function" $ do
            let funcName = DT.pack "ghostFunc"
            let call = ACall (ASymbol funcName) []
            
            checkExpr emptyEnv call `shouldSatisfy` \case
                Left err | err == "Undefined function 'ghostFunc'" -> True
                _ -> False
    
    describe "Function Arguments Validation (Count, Types, Env)" $ do
        
        let fName = DT.pack "testFunc"
        let fType = TyFunc [TyInt] TyBool
        let varName = DT.pack "myVar"
        
        let env = emptyEnv {
            envVars = Map.fromList [
                (fName, fType),
                (varName, TyInt)
            ]
        }

        it "verifyArgCount: fails and reports actual argument count (show length args)" $ do
            let call = ACall (ASymbol fName) [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            
            checkExpr env call `shouldSatisfy` \case
                Left err | "but got 2" `isInfixOf` err -> True
                _ -> False

        it "verifyArgTypes: uses env to resolve variables passed as arguments" $ do
            let call = ACall (ASymbol fName) [ASymbol varName]
            
            checkExpr env call `shouldSatisfy` \case
                Right TyBool -> True
                _ -> False

        it "verifyArgTypes: includes function name in error message on mismatch" $ do
            let call = ACall (ASymbol fName) [ABool True]
            
            checkExpr env call `shouldSatisfy` \case
                Left err | "call to 'testFunc'" `isInfixOf` err -> True
                _ -> False
