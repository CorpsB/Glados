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

simType :: CheckEnv -> Ast -> Either String Type
simType _ (AInteger _) = Right TyInt
simType _ (ASymbol s)
    | s == p "str"     = Right (TyList TyInt)
    | s == p "listStr" = Right (TyList (TyList TyInt))
    | s == p "bad"     = Right TyBool
simType _ _ = Right TyVoid

tyString :: Type
tyString = TyList TyInt

tyListString :: Type
tyListString = TyList (TyList TyInt)

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

        it "Accepts int == bool (Runtime check)" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            let mockChecker _ (AInteger _) = Right TyInt
                mockChecker _ (ABool _) = Right TyBool
                mockChecker _ _ = Left "Error"
            checkEqualityOp mockChecker emptyEnv args `shouldSatisfy` \case
                Right TyBool -> True
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

        it "Rejects invalid call node (not a function type)" $ do
            let funcNode = AInteger (fitInteger 1)
            checkCall mockCheckExpr testEnv funcNode [] `shouldSatisfy` \case
                Left err -> "expression is not a function" `isInfixOf` err
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

        it "passes env to attr_update to resolve struct definition and variable" $ do
            let sName = DT.pack "MyStruct"
            let fName = DT.pack "f"
            let varName = DT.pack "s"
            
            let sDef = StructDef sName (Map.singleton fName TyInt)
            let structEnv = emptyEnv {
                envStructs = Map.singleton sName sDef,
                envVars = Map.singleton varName (TyStruct sName)
            }
            
            let fieldNameAst = AList [AInteger (IChar 102)]
            let call = ACall (ASymbol (DT.pack "attr_update")) 
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
            let call = ACall (ASymbol (DT.pack "attr_update")) 
                             [ASymbol (DT.pack "num"), fieldName, AInteger (fitInteger 10)]
            
            checkExpr fieldEnv call `shouldSatisfy` \case
                Left err | err == "attr_update expects a structure as first argument" -> True
                _ -> False

        it "checkSetField: fails when argument count is incorrect" $ do
            let call = ACall (ASymbol (DT.pack "attr_update")) 
                             [ASymbol (DT.pack "p")]
            
            checkExpr fieldEnv call `shouldSatisfy` \case
                Left err | err == "attr_update expects 3 arguments" -> True
                _ -> False

        it "checkFieldInStruct: fails when field does not exist in struct" $ do
            let fieldZ = AList [AInteger (IChar 122)]
            let call = ACall (ASymbol (DT.pack "attr_update")) 
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
            let call = ACall (ASymbol (DT.pack "attr_update")) 
                             [ASymbol (DT.pack "p"), fNameAst, ASymbol (DT.pack "y")]
            
            checkExpr fieldEnv call `shouldSatisfy` \case
                Right (TyStruct n) | n == sName -> True
                _ -> False

        it "validateAssignment: fails with specific error message on type mismatch" $ do
            let call = ACall (ASymbol (DT.pack "attr_update")) 
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
            let call = ACall (ASymbol (DT.pack "attr_update")) 
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
            let call = ACall (ASymbol (DT.pack "attr_update")) 
                             [ASymbol (DT.pack "s"), notAList, AInteger (fitInteger 1)]
            
            checkExpr env call `shouldSatisfy` \case
                Left err | err == "Invalid field name format in attr_update" -> True
                _ -> False

        it "getStructDef: returns error when struct definition is missing in env" $ do
            let env = emptyEnv {
                envVars = Map.singleton (DT.pack "ghost") (TyStruct (DT.pack "GhostStruct"))
            }
            let fieldName = AList [AInteger (IChar 120)]
            let call = ACall (ASymbol (DT.pack "attr_update")) 
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

        it "checkEqualityOp: Accepts incompatible types (returns Bool)" $ do
            let args = [AInteger (fitInteger 1), ABool True]
            let mockChecker _ (AInteger _) = Right TyInt
                mockChecker _ (ABool _) = Right TyBool
                mockChecker _ _ = Left "Error"
            checkEqualityOp mockChecker emptyEnv args `shouldBe` Right TyBool

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

    describe "List Operators (cons, head, tail, nth, nth_update)" $ do
        let listEnv = emptyEnv {
            envVars = Map.fromList [
                (DT.pack "lInt", TyList TyInt),
                (DT.pack "lBool", TyList TyBool),
                (DT.pack "val", TyInt),
                (DT.pack "idx", TyInt),
                (DT.pack "b", TyBool),
                (DT.pack "notAList", TyInt)
            ]
        }
        let lInt = ASymbol (DT.pack "lInt")
        let val = ASymbol (DT.pack "val")
        let idx = ASymbol (DT.pack "idx")
        let b = ASymbol (DT.pack "b")
        let notAList = ASymbol (DT.pack "notAList")

        describe "cons(elem, list)" $ do
            it "Validates adding an int to [int]" $ do
                let args = [val, lInt]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "cons")) args `shouldSatisfy` \case
                    Right (TyList TyInt) -> True
                    _ -> False

            it "Rejects adding a bool to [int] (Type mismatch)" $ do
                let args = [b, lInt]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "cons")) args `shouldSatisfy` \case
                    Left err | "cons type mismatch" `isInfixOf` err -> True
                    _ -> False

            it "Rejects if second argument is not a list" $ do
                let args = [val, val]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "cons")) args `shouldSatisfy` \case
                    Left err | "expects a list as second argument" `isInfixOf` err -> True
                    _ -> False

        describe "head(list)" $ do
            it "Returns the inner type (int) of [int]" $ do
                let args = [lInt]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "head")) args `shouldSatisfy` \case
                    Right TyInt -> True
                    _ -> False

            it "Rejects if argument is not a list" $ do
                let args = [val]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "head")) args `shouldSatisfy` \case
                    Left err | "head expects a list" `isInfixOf` err -> True
                    _ -> False

        describe "tail(list)" $ do
            it "Returns the list type ([int])" $ do
                let args = [lInt]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "tail")) args `shouldSatisfy` \case
                    Right (TyList TyInt) -> True
                    _ -> False

            it "Rejects if argument is not a list" $ do
                let args = [val]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "tail")) args `shouldSatisfy` \case
                    Left err | "tail expects a list" `isInfixOf` err -> True
                    _ -> False

        describe "nth(list, index)" $ do
            it "Returns the inner type (int) at index" $ do
                let args = [lInt, idx]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth")) args `shouldSatisfy` \case
                    Right TyInt -> True
                    _ -> False

            it "Rejects if index is not an int" $ do
                let args = [lInt, b]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth")) args `shouldSatisfy` \case
                    Left err | "index must be an integer" `isInfixOf` err -> True
                    _ -> False

            it "Rejects if first argument is not a list" $ do
                let args = [notAList, idx]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth")) args `shouldSatisfy` \case
                    Left err | "expects a list as first argument" `isInfixOf` err -> True
                    _ -> False

        describe "nth_update(list, index, value)" $ do
            it "Validates correct update on [int]" $ do
                let args = [lInt, idx, val]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth_update")) args `shouldSatisfy` \case
                    Right (TyList TyInt) -> True
                    _ -> False

            it "Rejects if index is not an int" $ do
                let args = [lInt, b, val]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth_update")) args `shouldSatisfy` \case
                    Left err | "index must be an integer" `isInfixOf` err -> True
                    _ -> False

            it "Rejects if value type matches list type mismatch (bool into [int])" $ do
                let args = [lInt, idx, b]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth_update")) args `shouldSatisfy` \case
                    Left err | "Type mismatch in list update" `isInfixOf` err -> True
                    _ -> False

            it "Rejects if first argument is not a list" $ do
                let args = [notAList, idx, val]
                checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth_update")) args `shouldSatisfy` \case
                    Left err | "expects a list as first argument" `isInfixOf` err -> True
                    _ -> False
            
            it "Rejects incorrect argument count" $ do
                 let args = [lInt, idx]
                 checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth_update")) args `shouldSatisfy` \case
                    Left err | "expects 3 arguments" `isInfixOf` err -> True
                    _ -> False

        describe "Built-in Functions (print) and Strict Operators (===, !==)" $ do

            describe "print(arg)" $ do
                it "Validates print with an Integer" $ do
                    let args = [AInteger (fitInteger 42)]
                    checkCall mockCheckExpr testEnv (ASymbol (DT.pack "print")) args `shouldSatisfy` \case
                        Right TyVoid -> True
                        _ -> False

                it "Validates print with a Boolean" $ do
                    let args = [ABool True]
                    checkCall mockCheckExpr testEnv (ASymbol (DT.pack "print")) args `shouldSatisfy` \case
                        Right TyVoid -> True
                        _ -> False

                it "Rejects print with no arguments" $ do
                    let args = []
                    checkCall mockCheckExpr testEnv (ASymbol (DT.pack "print")) args `shouldSatisfy` \case
                        Left err | "expects exactly 1 argument" `isInfixOf` err -> True
                        _ -> False

                it "Rejects print with too many arguments" $ do
                    let args = [AInteger (fitInteger 1), AInteger (fitInteger 2)]
                    checkCall mockCheckExpr testEnv (ASymbol (DT.pack "print")) args `shouldSatisfy` \case
                        Left err | "expects exactly 1 argument" `isInfixOf` err -> True
                        _ -> False

            describe "Strict Equality (===) [teq?]" $ do
                let teq = ASymbol (DT.pack "teq?")

                it "Validates strict equality between same types (int === int)" $ do
                    let args = [AInteger (fitInteger 1), AInteger (fitInteger 1)]
                    checkCall mockCheckExpr testEnv teq args `shouldSatisfy` \case
                        Right TyBool -> True
                        _ -> False

                it "Validates strict equality between different types (int === bool)" $ do
                    let args = [AInteger (fitInteger 1), ABool True]
                    checkCall mockCheckExpr testEnv teq args `shouldSatisfy` \case
                        Right TyBool -> True
                        _ -> False

                it "Rejects incorrect argument count" $ do
                    let args = [AInteger (fitInteger 1)]
                    checkCall mockCheckExpr testEnv teq args `shouldSatisfy` \case
                        Left err | "Strict comparison operator expects 2 arguments" `isInfixOf` err -> True
                        _ -> False

            describe "Strict Inequality (!==) [tneq?]" $ do
                let tneq = ASymbol (DT.pack "tneq?")

                it "Validates strict inequality (int !== bool)" $ do
                    let args = [AInteger (fitInteger 1), ABool False]
                    checkCall mockCheckExpr testEnv tneq args `shouldSatisfy` \case
                        Right TyBool -> True
                        _ -> False
        
        describe "System Function (exit) and List Operator Argument Errors" $ do

            describe "exit(code)" $ do
                it "Validates exit with an Integer code" $ do
                    let args = [AInteger (fitInteger 0)]
                    checkCall mockCheckExpr testEnv (ASymbol (DT.pack "exit")) args `shouldSatisfy` \case
                        Right TyVoid -> True
                        _ -> False

                it "Rejects exit with a Boolean (expects int)" $ do
                    let args = [ABool True]
                    checkCall mockCheckExpr testEnv (ASymbol (DT.pack "exit")) args `shouldSatisfy` \case
                        Left err | "exit expects an integer code" `isInfixOf` err -> True
                        _ -> False

                it "Rejects exit with incorrect argument count (0 args)" $ do
                    let args = []
                    checkCall mockCheckExpr testEnv (ASymbol (DT.pack "exit")) args `shouldSatisfy` \case
                        Left err | "exit expects 1 argument" `isInfixOf` err -> True
                        _ -> False

            describe "List Operators Argument Count Checks" $ do
                
                let listInt = ASymbol (DT.pack "lInt")

                it "head: Rejects incorrect argument count (0 args)" $ do
                    let args = []
                    checkCall mockCheckExpr listEnv (ASymbol (DT.pack "head")) args `shouldSatisfy` \case
                        Left err | "head expects 1 argument" `isInfixOf` err -> True
                        _ -> False

                it "head: Rejects incorrect argument count (2 args)" $ do
                    let args = [listInt, listInt]
                    checkCall mockCheckExpr listEnv (ASymbol (DT.pack "head")) args `shouldSatisfy` \case
                        Left err | "head expects 1 argument" `isInfixOf` err -> True
                        _ -> False

                it "tail: Rejects incorrect argument count (0 args)" $ do
                    let args = []
                    checkCall mockCheckExpr listEnv (ASymbol (DT.pack "tail")) args `shouldSatisfy` \case
                        Left err | "tail expects 1 argument" `isInfixOf` err -> True
                        _ -> False

                it "nth: Rejects incorrect argument count (1 arg)" $ do
                    let args = [listInt]
                    checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth")) args `shouldSatisfy` \case
                        Left err | "nth expects 2 arguments" `isInfixOf` err -> True
                        _ -> False

                it "nth: Rejects incorrect argument count (3 args)" $ do
                    let args = [listInt, AInteger (fitInteger 0), AInteger (fitInteger 0)]
                    checkCall mockCheckExpr listEnv (ASymbol (DT.pack "nth")) args `shouldSatisfy` \case
                        Left err | "nth expects 2 arguments" `isInfixOf` err -> True
                        _ -> False

            describe "checkTypeof" $ do
                it "Returns string type for any valid argument" $ do
                    let args = [AInteger (I8 42)]
                    checkTypeof simType testEnv args `shouldBe` Right tyString

                it "Fails if argument count is wrong" $ do
                    checkTypeof simType testEnv [] `shouldBe` Left "typeof expects 1 argument"

            describe "checkFFRead" $ do
                it "Accepts a String path and returns [String]" $ do
                    let args = [ASymbol (p "str")]
                    checkFFRead simType testEnv args `shouldBe` Right tyListString

                it "Rejects non-string path" $ do
                    let args = [AInteger (I8 1)]
                    checkFFRead simType testEnv args `shouldBe` Left "ffread expects a string (path) as argument"

            describe "checkFFWrite" $ do
                it "Accepts (String, [String]) and returns Bool" $ do
                    let args = [ASymbol (p "str"), ASymbol (p "listStr")]
                    checkFFWrite simType testEnv args `shouldBe` Right TyBool

                it "Rejects if second arg is not [String]" $ do
                    let args = [ASymbol (p "str"), AInteger (I8 1)]
                    checkFFWrite simType testEnv args `shouldBe` Left "ffwrite expects (path: string, content: [string])"

            describe "checkOpen" $ do
                it "Accepts (String, Int) and returns Int (FD)" $ do
                    let args = [ASymbol (p "str"), AInteger (I8 0)]
                    checkOpen simType testEnv args `shouldBe` Right TyInt

                it "Rejects if path is not String" $ do
                    let args = [AInteger (I8 0), AInteger (I8 0)]
                    checkOpen simType testEnv args `shouldBe` Left "open expects (path: string, mode: int)"

            describe "checkClose" $ do
                it "Accepts Int (FD) and returns Int" $ do
                    let args = [AInteger (I8 3)]
                    checkClose simType testEnv args `shouldBe` Right TyInt

                it "Rejects non-integer FD" $ do
                    let args = [ASymbol (p "str")]
                    checkClose simType testEnv args `shouldBe` Left "close expects an integer file descriptor"

            describe "checkRead" $ do
                it "Accepts (Int, Int) and returns String" $ do
                    let args = [AInteger (I8 3), AInteger (I8 100)]
                    checkRead simType testEnv args `shouldBe` Right tyString

                it "Rejects if size is not Int" $ do
                    let args = [AInteger (I8 3), ASymbol (p "str")]
                    checkRead simType testEnv args `shouldBe` Left "read expects (fd: int, size: int)"

            describe "checkInput" $ do
                it "Accepts Int (FD) and returns String" $ do
                    let args = [AInteger (I8 0)]
                    checkInput simType testEnv args `shouldBe` Right tyString

                it "Rejects if FD is not Int" $ do
                    let args = [ASymbol (p "str")]
                    checkInput simType testEnv args `shouldBe` Left "input expects an integer file descriptor"

            describe "checkSystemOps" $ do
        
                it "checkPrint accepts any argument and returns Void" $ do
                    let call = ASymbol (p "print")
                    let args = [AInteger (I8 42)]
                    checkCall simType testEnv call args `shouldBe` Right TyVoid

                it "checkExit accepts Int and returns Void" $ do
                    let call = ASymbol (p "exit")
                    let args = [AInteger (I8 0)]
                    checkCall simType testEnv call args `shouldBe` Right TyVoid

                it "checkExit rejects non-Int argument" $ do
                    let call = ASymbol (p "exit")
                    let args = [ASymbol (p "str")]
                    checkCall simType testEnv call args `shouldBe` Left "exit expects an integer code"

            describe "checkDataFuncs" $ do

                it "checkCast (int8) accepts Int and returns Int" $ do
                    let call = ASymbol (p "int8")
                    let args = [AInteger (I8 100)]
                    checkCall simType testEnv call args `shouldBe` Right TyInt

                it "checkCons accepts (Int, [Int])" $ do
                    let call = ASymbol (p "cons")
                    let args = [AInteger (I8 1), ASymbol (p "str")] 
                    checkCall simType testEnv call args `shouldBe` Right tyString

                it "checkHead accepts [Int] and returns Int" $ do
                    let call = ASymbol (p "head")
                    let args = [ASymbol (p "str")]
                    checkCall simType testEnv call args `shouldBe` Right TyInt

                it "checkTail accepts [Int] and returns [Int]" $ do
                    let call = ASymbol (p "tail")
                    let args = [ASymbol (p "str")]
                    checkCall simType testEnv call args `shouldBe` Right tyString

                it "checkNth accepts ([Int], Int) and returns Int" $ do
                    let call = ASymbol (p "nth")
                    let args = [ASymbol (p "str"), AInteger (I8 0)]
                    checkCall simType testEnv call args `shouldBe` Right TyInt

                it "checkNth rejects non-Int index" $ do
                    let call = ASymbol (p "nth")
                    let args = [ASymbol (p "str"), ASymbol (p "str")]
                    checkCall simType testEnv call args `shouldBe` Left "nth index must be an integer"

                it "checkUpdate accepts ([Int], Int, Int) and returns [Int]" $ do
                    let call = ASymbol (p "nth_update")
                    let args = [ASymbol (p "str"), AInteger (I8 0), AInteger (I8 99)]
                    checkCall simType testEnv call args `shouldBe` Right tyString
            
            describe "checkIOFuncs Dispatch (via checkCall)" $ do
        
                it "Dispatches 'ffread' correctly" $ do
                    let call = ASymbol (p "ffread")
                    let args = [ASymbol (p "str")]
                    checkCall simType testEnv call args `shouldBe` Right tyListString

                it "Dispatches 'ffwrite' correctly" $ do
                    let call = ASymbol (p "ffwrite")
                    let args = [ASymbol (p "str"), ASymbol (p "listStr")]
                    checkCall simType testEnv call args `shouldBe` Right TyBool

                it "Dispatches 'open' correctly" $ do
                    let call = ASymbol (p "open")
                    let args = [ASymbol (p "str"), AInteger (I8 0)]
                    checkCall simType testEnv call args `shouldBe` Right TyInt

                it "Dispatches 'close' correctly" $ do
                    let call = ASymbol (p "close")
                    let args = [AInteger (I8 3)]
                    checkCall simType testEnv call args `shouldBe` Right TyInt

                it "Dispatches 'read' correctly" $ do
                    let call = ASymbol (p "read")
                    let args = [AInteger (I8 3), AInteger (I8 100)]
                    checkCall simType testEnv call args `shouldBe` Right tyString

                it "Dispatches 'input' correctly" $ do
                    let call = ASymbol (p "input")
                    let args = [AInteger (I8 0)]
                    checkCall simType testEnv call args `shouldBe` Right tyString
