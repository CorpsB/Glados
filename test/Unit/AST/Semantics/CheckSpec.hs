{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Semantics.CheckSpec (spec) where

import Test.Hspec
import AST.Semantics.Check
import AST.Semantics.Type
import AST.Ast (Ast(..))
import Common.Type.Integer (fitInteger)
import qualified Data.Text as DT
import qualified Data.Map.Strict as Map
import qualified Data.List

p :: String -> DT.Text
p = DT.pack

isInfixOfStr :: String -> String -> Bool
isInfixOfStr needle haystack = needle `Data.List.isInfixOf` haystack

spec :: Spec
spec = describe "Semantic Checker Coverage" $ do

    let env = CheckEnv 
            (Map.fromList [ 
                (p "i", TyInt), 
                (p "b", TyBool),
                (p "v", TyVoid),
                (p "auto_var", TyAuto),
                (p "l1", TyList TyInt),
                (p "l2", TyList TyBool),
                (p "s1", TyStruct (p "A")),
                (p "s2", TyStruct (p "B")),
                (p "f1", TyFunc [TyInt] TyVoid),
                (p "f2", TyFunc [TyInt, TyInt] TyVoid),
                (p "f3", TyFunc [TyBool] TyVoid),
                (p "f4", TyFunc [TyInt] TyInt)
            ]) Map.empty

    describe "checkExpr Basic" $ do
        it "Literals" $ do
            checkExpr env (AInteger (fitInteger 1)) `shouldSatisfy` \case Right TyInt -> True; _ -> False
            checkExpr env (ABool True) `shouldSatisfy` \case Right TyBool -> True; _ -> False
            checkExpr env AVoid `shouldSatisfy` \case Right TyVoid -> True; _ -> False

        it "Variables" $ do
            checkExpr env (ASymbol (p "i")) `shouldSatisfy` \case Right TyInt -> True; _ -> False
            checkExpr env (ASymbol (p "undef")) `shouldSatisfy` \case Left _ -> True; _ -> False

        it "Unsupported Expression (Default case)" $ do
            checkExpr env (AImport (p "lib")) `shouldSatisfy` \case Left msg -> "supported" `isInfixOfStr` msg; _ -> False

    describe "areTypesCompatible" $ do
        it "Primitives: TyInt == TyInt" $ do
            let expr = AIf (ABool True) (ASymbol (p "i")) (ASymbol (p "i"))
            checkExpr env expr `shouldSatisfy` \case Right TyInt -> True; _ -> False

        it "Primitives: TyBool == TyBool" $ do
            let expr = AIf (ABool True) (ASymbol (p "b")) (ASymbol (p "b"))
            checkExpr env expr `shouldSatisfy` \case Right TyBool -> True; _ -> False

        it "Primitives: TyVoid == TyVoid" $ do
            let expr = AIf (ABool True) (ASymbol (p "v")) (ASymbol (p "v"))
            checkExpr env expr `shouldSatisfy` \case Right TyVoid -> True; _ -> False
        
        it "Primitives: TyAuto == TyAuto" $ do
            let expr = AIf (ABool True) (ASymbol (p "auto_var")) (ASymbol (p "auto_var"))
            checkExpr env expr `shouldSatisfy` \case Right TyAuto -> True; _ -> False

        it "Lists: Recursion Success" $ do
            let expr = AIf (ABool True) (ASymbol (p "l1")) (ASymbol (p "l1"))
            checkExpr env expr `shouldSatisfy` \case Right (TyList TyInt) -> True; _ -> False

        it "Lists: Recursion Failure" $ do
            let expr = AIf (ABool True) (ASymbol (p "l1")) (ASymbol (p "l2"))
            checkExpr env expr `shouldSatisfy` \case Left _ -> True; _ -> False

        it "Structs: Success" $ do
            let expr = AIf (ABool True) (ASymbol (p "s1")) (ASymbol (p "s1"))
            checkExpr env expr `shouldSatisfy` \case Right (TyStruct _) -> True; _ -> False

        it "Structs: Failure" $ do
            let expr = AIf (ABool True) (ASymbol (p "s1")) (ASymbol (p "s2"))
            checkExpr env expr `shouldSatisfy` \case Left _ -> True; _ -> False

        it "Funcs: Success" $ do
            let expr = AIf (ABool True) (ASymbol (p "f1")) (ASymbol (p "f1"))
            checkExpr env expr `shouldSatisfy` \case Right _ -> True; _ -> False

        it "Funcs: Failure (Length)" $ do
            let expr = AIf (ABool True) (ASymbol (p "f1")) (ASymbol (p "f2"))
            checkExpr env expr `shouldSatisfy` \case Left _ -> True; _ -> False

        it "Funcs: Failure (Args Types)" $ do
            let expr = AIf (ABool True) (ASymbol (p "f1")) (ASymbol (p "f3"))
            checkExpr env expr `shouldSatisfy` \case Left _ -> True; _ -> False

        it "Funcs: Failure (Return Type)" $ do
            let expr = AIf (ABool True) (ASymbol (p "f1")) (ASymbol (p "f4"))
            checkExpr env expr `shouldSatisfy` \case Left _ -> True; _ -> False

    describe "checkStmt Coverage" $ do
        it "ASetVar: Auto Type (Inference Check)" $ do
            let stmt = ASetVar (p "x") (p "auto") (AInteger (fitInteger 1))
            checkStmt emptyEnv stmt `shouldSatisfy` \case 
                -- On vérifie que l'AST retourné a bien "int" au lieu de "auto"
                Right (ASetVar "x" "int" _, _) -> True
                _ -> False

        it "ASetVar: Explicit Compatible" $ do
            let stmt = ASetVar (p "x") (p "int") (AInteger (fitInteger 1))
            checkStmt emptyEnv stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "ASetVar: Explicit Incompatible" $ do
            let stmt = ASetVar (p "x") (p "bool") (AInteger (fitInteger 1))
            checkStmt emptyEnv stmt `shouldSatisfy` \case Left msg -> "declared as" `isInfixOfStr` msg; _ -> False

        it "ASetVar: Expression Error propagation" $ do
            let stmt = ASetVar (p "x") (p "int") (ASymbol (p "unknown_var"))
            checkStmt emptyEnv stmt `shouldSatisfy` \case Left msg -> "Undefined" `isInfixOfStr` msg; _ -> False

        it "Unknown Statement (Fallback to checkExpr with valid expression)" $ do
            -- CHANGED: Utilisation de AVoid au lieu de AImport, car AImport n'est pas une expression
            checkStmt emptyEnv AVoid `shouldSatisfy` \case Right (_, _) -> True; _ -> False

    describe "checkAst" $ do
        it "Sequence Success (Empty)" $ do
            checkAst [] `shouldSatisfy` \case Right [] -> True; _ -> False

        it "Sequence Immediate Failure (Type Mismatch)" $ do
            let ast = [ ASetVar (p "x") (p "bool") (AInteger (fitInteger 1)) ]
            checkAst ast `shouldSatisfy` \case 
                Left err -> "assigned int" `isInfixOfStr` err 
                _ -> False

        it "Sequence Failure due to empty environment (Undefined Var)" $ do
            let ast = [ ASetVar (p "x") (p "int") (ASymbol (p "unknown_var")) ]
            checkAst ast `shouldSatisfy` \case
                Left err -> "Undefined variable" `isInfixOfStr` err
                _ -> False

        it "Sequence Failure Mid-stream" $ do
            let ast = [ ASetVar (p "x") (p "int") (AInteger (fitInteger 1))
                      , ASetVar (p "y") (p "bool") (AInteger (fitInteger 1))
                      ]
            checkAst ast `shouldSatisfy` \case 
                Left err -> "assigned int" `isInfixOfStr` err
                _ -> False
        
        it "Sequence Full Success" $ do
            let ast = [ ASetVar (p "x") (p "int") (AInteger (fitInteger 1))
                      , ASetVar (p "y") (p "int") (AInteger (fitInteger 2))
                      ]
            checkAst ast `shouldSatisfy` \case Right _ -> True; _ -> False
    
    describe "Detailed Error Messages" $ do
        it "If branches type mismatch message" $ do
            let expr = AIf (ABool True) (AInteger (fitInteger 1)) (ABool False)
            checkExpr emptyEnv expr `shouldSatisfy`
                \case Left msg -> "mismatch" `isInfixOfStr` msg && "int" `isInfixOfStr` msg && "bool" `isInfixOfStr` msg
                      _ -> False

        it "Variable assignment error message" $ do
            let stmt = ASetVar (p "myVar") (p "bool") (AInteger (fitInteger 1))
            checkStmt emptyEnv stmt `shouldSatisfy`
                \case Left msg -> "myVar" `isInfixOfStr` msg && "declared as bool" `isInfixOfStr` msg && "assigned int" `isInfixOfStr` msg
                      _ -> False
    
    describe "Extra checkExpr Scenarios" $ do
        it "Defined symbol lookup" $ do
            let envLocal = emptyEnv { envVars = Map.fromList [(p "x", TyInt)] }
            checkExpr envLocal (ASymbol (p "x")) `shouldSatisfy` \case Right TyInt -> True; _ -> False

        it "If condition not boolean" $ do
            let expr = AIf AVoid
                           (AInteger (fitInteger 2))
                           (AInteger (fitInteger 3))
            checkExpr emptyEnv expr `shouldSatisfy`
                \case Left msg -> "must be boolean" `isInfixOfStr` msg
                      _        -> False

        it "If branches incompatible" $ do
            let expr = AIf (ABool True)
                           (AInteger (fitInteger 1))
                           (ABool False)
            checkExpr emptyEnv expr `shouldSatisfy`
                \case Left msg -> "mismatch" `isInfixOfStr` msg
                      _        -> False

    describe "Extra checkStmt Scenarios" $ do
        it "SetVar auto accepts any type" $ do
            let stmt = ASetVar (p "x") (p "auto") (AInteger (fitInteger 1))
            checkStmt emptyEnv stmt `shouldSatisfy`
                \case Right (_, envLocal) ->
                        case Map.lookup (p "x") (envVars envLocal) of
                            Just TyInt -> True
                            _ -> False
                      _ -> False

        it "SetVar incompatible type" $ do
            let stmt = ASetVar (p "x") (p "bool") (AInteger (fitInteger 1))
            checkStmt emptyEnv stmt `shouldSatisfy`
                \case Left msg -> "but assigned" `isInfixOfStr` msg
                      _        -> False

        it "SetVar compatible type" $ do
            let stmt = ASetVar (p "x") (p "int") (AInteger (fitInteger 1))
            checkStmt emptyEnv stmt `shouldSatisfy`
                \case Right (_, envLocal) ->
                        case Map.lookup (p "x") (envVars envLocal) of
                            Just TyInt -> True
                            _ -> False
                      _ -> False
    
    describe "Last check" $ do
        it "Fully evaluates undefined variable error message (concatenation)" $ do
            checkExpr env (ASymbol (p "my_missing_var")) `shouldSatisfy` \case
                Left msg -> "'my_missing_var'" `isInfixOfStr` msg
                _ -> False

        it "Uses env for AIf condition resolution" $ do
            let expr = AIf (ASymbol (p "b")) (AInteger (fitInteger 1)) (AInteger (fitInteger 1))
            checkExpr env expr `shouldSatisfy` \case Right TyInt -> True; _ -> False

        it "Fully evaluates AIf type mismatch error (closing parenthesis)" $ do
            let expr = AIf (ABool True) (AInteger (fitInteger 1)) (ABool True)
            checkExpr env expr `shouldSatisfy` \case
                Left msg -> ")" `isInfixOfStr` msg && "vs" `isInfixOfStr` msg
                _ -> False

        it "Preserves environment in fallback statement" $ do
            -- CHANGED: Utilisation de AVoid au lieu de AImport
            checkStmt env AVoid `shouldSatisfy` \case
                Right (_, resEnv) -> case Map.lookup (p "i") (envVars resEnv) of
                    Just TyInt -> True
                    _ -> False
                _ -> False
        
        it "Fully evaluates checkAst error propagation" $ do
             let ast = [ ASetVar (p "x") (p "bool") (AInteger (fitInteger 1)) ]
             checkAst ast `shouldSatisfy` \case
                Left err -> "assigned int" `isInfixOfStr` err
                _ -> False
    
    describe "Structures Management" $ do
        let pointFields = [(p "x", p "int"), (p "y", p "int")]
        let definePoint = ADefineStruct (p "Point") pointFields
        
        let envWithPoint = case checkStmt emptyEnv definePoint of
                Right (_, e) -> e
                Left _ -> error "Setup failed: DefineStruct"

        it "Defines a new struct successfully" $ do
            checkStmt emptyEnv definePoint `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Fails to redefine existing struct" $ do
            checkStmt envWithPoint definePoint `shouldSatisfy` \case 
                Left msg -> "already defined" `isInfixOfStr` msg
                _ -> False

        it "Instantiates a valid struct" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1)), (p "y", AInteger (fitInteger 2))]
            checkExpr envWithPoint inst `shouldSatisfy` \case Right (TyStruct name) -> name == p "Point"; _ -> False

        it "Fails instantiation: Undefined Struct" $ do
            let inst = ASetStruct (p "Alien") []
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Undefined struct" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Unknown Field" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1)), (p "z", AInteger (fitInteger 3))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Unknown field" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Missing Field" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Missing field" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Field Type Mismatch" $ do
            let inst = ASetStruct (p "Point") [(p "x", ABool True), (p "y", AInteger (fitInteger 2))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "expected int" `isInfixOfStr` msg
                _ -> False
        
        it "Instantiates using a variable (Forces env usage in validateField)" $ do
            let envWithVar = case checkStmt envWithPoint (ASetVar (p "myVal") (p "int") (AInteger (fitInteger 10))) of
                    Right (_, e) -> e
                    Left _ -> error "Setup failed: Var"
            
            let inst = ASetStruct (p "Point") [(p "x", ASymbol (p "myVal")), (p "y", AInteger (fitInteger 2))]
            
            checkExpr envWithVar inst `shouldSatisfy` \case Right _ -> True; _ -> False

        it "Fails instantiation: Undefined Struct" $ do
            let inst = ASetStruct (p "Alien") []
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Undefined struct" `isInfixOfStr` msg && "Alien" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Unknown Field" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1)), (p "z", AInteger (fitInteger 3))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Unknown field" `isInfixOfStr` msg && "'z'" `isInfixOfStr` msg && "Point" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Missing Field" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Missing field" `isInfixOfStr` msg && "'y'" `isInfixOfStr` msg && "Point" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Field Type Mismatch" $ do
            let inst = ASetStruct (p "Point") [(p "x", ABool True), (p "y", AInteger (fitInteger 2))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "expected int" `isInfixOfStr` msg
                _ -> False

        it "Defines a new struct and stores its name correctly" $ do
            checkStmt emptyEnv definePoint `shouldSatisfy` \case 
                Right (_, resEnv) -> 
                    case Map.lookup (p "Point") (envStructs resEnv) of
                        Just def -> structName def == p "Point"
                        Nothing -> False
                _ -> False

        it "Fails to redefine existing struct" $ do
            checkStmt envWithPoint definePoint `shouldSatisfy` \case 
                Left msg -> "already defined" `isInfixOfStr` msg && "Point" `isInfixOfStr` msg
                _ -> False

        it "Instantiates a valid struct (Success path)" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1)), (p "y", AInteger (fitInteger 2))]
            checkExpr envWithPoint inst `shouldSatisfy` \case Right (TyStruct name) -> name == p "Point"; _ -> False

        it "Instantiates using a variable (Forces env usage in validateField)" $ do
            let envWithVar = case checkStmt envWithPoint (ASetVar (p "myVal") (p "int") (AInteger (fitInteger 10))) of
                    Right (_, e) -> e
                    Left _ -> error "Setup failed: Var"
            let inst = ASetStruct (p "Point") [(p "x", ASymbol (p "myVal")), (p "y", AInteger (fitInteger 2))]
            checkExpr envWithVar inst `shouldSatisfy` \case Right _ -> True; _ -> False

        it "Fails instantiation: Undefined Struct (Checks closing quote)" $ do
            let inst = ASetStruct (p "Alien") []
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Undefined struct" `isInfixOfStr` msg && 
                            "Alien'" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Unknown Field (Checks closing quote)" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1)), (p "z", AInteger (fitInteger 3))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Unknown field" `isInfixOfStr` msg && 
                            "'z'" `isInfixOfStr` msg && 
                            "Point'" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Missing Field (Checks closing quote)" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "Missing field" `isInfixOfStr` msg && 
                            "'y'" `isInfixOfStr` msg && 
                            "Point'" `isInfixOfStr` msg
                _ -> False

        it "Fails instantiation: Field Type Mismatch (Checks actual type string)" $ do
            let inst = ASetStruct (p "Point") [(p "x", ABool True), (p "y", AInteger (fitInteger 2))]
            checkExpr envWithPoint inst `shouldSatisfy` \case 
                Left msg -> "expected int" `isInfixOfStr` msg &&
                            "but got bool" `isInfixOfStr` msg
                _ -> False

        it "Defines a new struct and stores its name correctly" $ do
            checkStmt emptyEnv definePoint `shouldSatisfy` \case 
                Right (_, resEnv) -> 
                    case Map.lookup (p "Point") (envStructs resEnv) of
                        Just def -> structName def == p "Point"
                        Nothing -> False
                _ -> False

        it "Instantiates a valid struct (Success path covers Right ())" $ do
            let inst = ASetStruct (p "Point") [(p "x", AInteger (fitInteger 1)), (p "y", AInteger (fitInteger 2))]
            checkExpr envWithPoint inst `shouldSatisfy` \case Right (TyStruct name) -> name == p "Point"; _ -> False

    describe "Function Calls & Operators Delegation" $ do

        it "Delegates ACall to checkCall (Binary Operator)" $ do
            let call = ACall (ASymbol (p "+")) [AInteger (fitInteger 1), AInteger (fitInteger 2)]
            checkExpr env call `shouldSatisfy` \case Right TyInt -> True; _ -> False

    describe "APos Error Handling (Line Numbers)" $ do
        it "Wraps error with line number when APos is encountered" $ do
            let expr = APos 10 5 (ASymbol (p "unknown_var"))
            checkExpr env expr `shouldSatisfy` \case
                Left err -> "Error line 10:" `isInfixOfStr` err && "unknown_var" `isInfixOfStr` err
                _ -> False

        it "Does not double-wrap error messages (Nested APos)" $ do
            let expr = APos 10 1 (APos 20 1 (ASymbol (p "unknown_var")))
            checkExpr env expr `shouldSatisfy` \case
                Left err -> "Error line 20:" `isInfixOfStr` err && not ("Error line 10:" `isInfixOfStr` err)
                _ -> False
        
        it "Propagates success correctly through APos" $ do
            let expr = APos 10 5 (AInteger (fitInteger 42))
            checkExpr env expr `shouldSatisfy` \case Right TyInt -> True; _ -> False

    describe "Loop Logic (checkFor via checkStmt)" $ do
        it "Validates For loop with Bool condition" $ do
            let stmt = AFor (ASetVar (p "x") (p "int") (AInteger (fitInteger 0))) 
                            (ABool True) 
                            AVoid 
                            AVoid 
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Validates For loop with Int condition (C-Style Support)" $ do
            let stmt = AFor AVoid 
                            (AInteger (fitInteger 1)) 
                            AVoid 
                            AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Rejects For loop with Invalid condition type" $ do
            let stmt = AFor AVoid 
                            AVoid 
                            AVoid 
                            AVoid
            checkStmt env stmt `shouldSatisfy` \case
                Left err -> "must be boolean or integer" `isInfixOfStr` err
                _ -> False

        it "Validates For loop initialization scope" $ do
            let stmt = AFor (ASetVar (p "loopVar") (p "int") (AInteger (fitInteger 0)))
                            (ASymbol (p "loopVar")) 
                            AVoid
                            AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

    describe "If Condition C-Style" $ do
        it "Accepts TyInt as condition and checks branches" $ do
            let expr = AIf (AInteger (fitInteger 1)) (AInteger (fitInteger 1)) (AInteger (fitInteger 1))
            checkExpr env expr `shouldSatisfy` \case Right TyInt -> True; _ -> False

    describe "Coverage: Loop Logic" $ do
        it "Validates While with TyBool" $ do
            let stmt = AWhile (ABool True) AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Validates While with TyInt" $ do
            let stmt = AWhile (AInteger (fitInteger 1)) AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Rejects While with TyVoid" $ do
            let stmt = AWhile AVoid AVoid
            checkStmt env stmt `shouldSatisfy` \case 
                Left err -> "'while' condition must be boolean or integer" `isInfixOfStr` err
                _ -> False

        it "Checks Body in While loop" $ do
            let stmt = AWhile (ABool True) (ASetVar (p "i") (p "bool") (AInteger (fitInteger 1)))
            checkStmt env stmt `shouldSatisfy` \case 
                Left err -> "but assigned" `isInfixOfStr` err
                _ -> False

    describe "Coverage: For Loop Logic" $ do
        it "Validates For condition TyBool" $ do
            let stmt = AFor AVoid (ABool True) AVoid AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Validates For condition TyInt" $ do
            let stmt = AFor AVoid (AInteger (fitInteger 1)) AVoid AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Rejects For condition TyVoid" $ do
            let stmt = AFor AVoid AVoid AVoid AVoid
            checkStmt env stmt `shouldSatisfy` \case 
                Left err -> "'for' condition must be boolean or integer" `isInfixOfStr` err
                _ -> False

        it "Checks Update statement" $ do
            let stmt = AFor AVoid (ABool True) (ASetVar (p "i") (p "bool") (AInteger (fitInteger 1))) AVoid
            checkStmt env stmt `shouldSatisfy` \case 
                Left err -> "but assigned" `isInfixOfStr` err
                _ -> False

        it "Checks Body statement" $ do
            let stmt = AFor AVoid (ABool True) AVoid (ASetVar (p "i") (p "bool") (AInteger (fitInteger 1)))
            checkStmt env stmt `shouldSatisfy` \case 
                Left err -> "but assigned" `isInfixOfStr` err
                _ -> False

    describe "Coverage: Environment Propagation" $ do
        it "Propagates env to branches when condition is Int" $ do
            let expr = AIf (AInteger (fitInteger 1)) (ASymbol (p "i")) (ASymbol (p "i"))
            checkExpr env expr `shouldSatisfy` \case Right TyInt -> True; _ -> False

        it "Propagates env to checkCall" $ do
            let call = ACall (ASymbol (p "f1")) [AInteger (fitInteger 1)]
            checkExpr env call `shouldSatisfy` \case Right TyVoid -> True; _ -> False
    
    describe "Function Definition" $ do
        it "Defines a function and adds it to environment" $ do
            let func = ADefineFunc (p "myFunc") [(p "x", p "int")] (p "int") (AReturn (ASymbol (p "x")))
            checkStmt env func `shouldSatisfy` \case
                Right (_, newEnv) -> 
                    case Map.lookup (p "myFunc") (envVars newEnv) of
                        Just (TyFunc [TyInt] TyInt) -> True
                        _ -> False
                _ -> False

        it "Validates function body using argument scope" $ do
            let body = ASetVar (p "a") (p "auto") (AInteger (fitInteger 10))
            let func = ADefineFunc (p "test") [(p "a", p "int")] (p "void") body
            checkStmt env func `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Fails if function body contains semantic error" $ do
            let body = ASetVar (p "local") (p "int") (ABool True)
            let func = ADefineFunc (p "fail") [] (p "void") body
            checkStmt env func `shouldSatisfy` \case
                Left err -> "assigned bool" `isInfixOfStr` err
                _ -> False

        it "Allows recursion (Function visible inside its own body)" $ do
            let callRec = ACall (ASymbol (p "rec")) []
            let func = ADefineFunc (p "rec") [] (p "void") callRec
            checkStmt env func `shouldSatisfy` \case Right (_, _) -> True; _ -> False

    describe "Statement Position Error Handling" $ do
        it "Wraps statement error with line number" $ do
            let stmt = APos 42 1 (ASetVar (p "x") (p "int") (ABool True))
            checkStmt emptyEnv stmt `shouldSatisfy` \case
                Left err -> "Error" `isInfixOfStr` err && "assigned bool" `isInfixOfStr` err
                _ -> False

        it "Does not double-wrap statement errors" $ do
            let stmt = APos 10 1 (APos 20 1 (ASetVar (p "x") (p "int") (ABool True)))
            checkStmt emptyEnv stmt `shouldSatisfy` \case
                Left err -> "Error" `isInfixOfStr` err && not ("Error :" `isInfixOfStr` err)
                _ -> False
        
        it "Propagates success correctly through APos Statement" $ do
            let stmt = APos 10 5 (ASetVar (p "x") (p "int") (AInteger (fitInteger 42)))
            checkStmt emptyEnv stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

    describe "checkLoop Environment Propagation" $ do
        it "Returns success when condition is TyBool (Scope is isolated)" $ do
            let body = ASetVar (p "x_bool") (p "int") (AInteger (fitInteger 10))
            let stmt = AWhile (ABool True) body
            checkStmt env stmt `shouldSatisfy` \case
                Right (_, _) -> True
                _ -> False

        it "Returns success when condition is TyInt (Scope is isolated)" $ do
            let body = ASetVar (p "y_int") (p "int") (AInteger (fitInteger 20))
            let stmt = AWhile (AInteger (fitInteger 1)) body
            checkStmt env stmt `shouldSatisfy` \case
                Right (_, _) -> True
                _ -> False

        it "Returns Left Error when condition is invalid" $ do
            let stmt = AWhile AVoid AVoid
            checkStmt env stmt `shouldSatisfy` \case
                Left err -> "'while' condition must be boolean or integer" `isInfixOfStr` err
                _ -> False
    
    describe "Loop Condition Check" $ do
        it "Propagates error" $ do
            let stmt = AWhile (ASymbol (p "unknown_var")) AVoid
            checkStmt env stmt `shouldSatisfy` \case 
                Left err -> "Undefined variable" `isInfixOfStr` err
                _ -> False
    
    describe "For Loop Scoping & Internals" $ do
        
        it "Verifies updateStmt sees variables from initStmt" $ do
            let initS = ASetVar (p "i_scope") (p "int") (AInteger (fitInteger 0))
            let condS = ACall (ASymbol (p "<")) [ASymbol (p "i_scope"), AInteger (fitInteger 10)]
            let updateS = ASetVar (p "i_scope") (p "auto") (AInteger (fitInteger 1))
            let bodyS = AVoid
            
            let stmt = AFor initS condS updateS bodyS
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Verifies body sees variables from initStmt" $ do
            let initS = ASetVar (p "j_scope") (p "int") (AInteger (fitInteger 0))
            let condS = ABool True
            let updateS = AVoid
            let bodyS = ASetVar (p "j_scope") (p "auto") (AInteger (fitInteger 1))
            let stmt = AFor initS condS updateS bodyS
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Verifies loop variables do NOT leak into outer scope" $ do

            let initS = ASetVar (p "leak_check") (p "int") (AInteger (fitInteger 0))
            let stmt = AFor initS (ABool True) AVoid AVoid
            
            checkStmt env stmt `shouldSatisfy` \case 
                Right (_, resEnv) -> 
                    case Map.lookup (p "leak_check") (envVars resEnv) of
                        Nothing -> True
                        Just _ -> False
                _ -> False

        it "Passes TyBool condition (Right ())" $ do
            let stmt = AFor AVoid (ABool True) AVoid AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

        it "Passes TyInt condition (Right ())" $ do
            let stmt = AFor AVoid (AInteger (fitInteger 1)) AVoid AVoid
            checkStmt env stmt `shouldSatisfy` \case Right (_, _) -> True; _ -> False

    describe "Struct Access Semantic Verification" $ do
        
        let pointFields = Map.fromList [(DT.pack "x", TyInt), (DT.pack "y", TyInt)]
        let pointDef = StructDef (DT.pack "Point") pointFields
        
        let testEnv = emptyEnv {
            envStructs = Map.insert (DT.pack "Point") pointDef (envStructs emptyEnv),
            envVars = Map.fromList [
                (DT.pack "p", TyStruct (DT.pack "Point")),
                (DT.pack "i", TyInt)
            ]
        }

        it "validates valid field access (p.x)" $ do
            let ast = AAccessStruct (ASymbol (DT.pack "p")) (DT.pack "x")
            checkExpr testEnv ast `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "validates valid field access (p.y)" $ do
            let ast = AAccessStruct (ASymbol (DT.pack "p")) (DT.pack "y")
            checkExpr testEnv ast `shouldSatisfy` \case
                Right TyInt -> True
                _ -> False

        it "returns error for unknown field (p.z)" $ do
            let ast = AAccessStruct (ASymbol (DT.pack "p")) (DT.pack "z")
            checkExpr testEnv ast `shouldSatisfy` \case
                Left err | err == "Field 'z' not found in struct 'Point'" -> True
                _ -> False

        it "returns error when accessing field on non-struct type (i.x)" $ do
            let ast = AAccessStruct (ASymbol (DT.pack "i")) (DT.pack "x")
            checkExpr testEnv ast `shouldSatisfy` \case
                Left err | err == "Cannot access field 'x' on non-struct type int" -> True
                _ -> False

    describe "Control Flow and Environment Verification" $ do
        
        let empty = emptyEnv

        it "checkFor: validates loop with boolean condition and scope (TyBool)" $ do
            let initS = ASetVar (DT.pack "i") (DT.pack "int") (AInteger (fitInteger 0))
            let condS = ACall (ASymbol (DT.pack "<")) [ASymbol (DT.pack "i"), AInteger (fitInteger 10)]
            let loop = AFor initS condS AVoid AVoid
            checkStmt empty loop `shouldSatisfy` \case
                Right (_, _) -> True
                _ -> False

        it "checkFor: validates loop with integer condition (TyInt)" $ do
            let initS = ASetVar (DT.pack "k") (DT.pack "int") (AInteger (fitInteger 1))
            let condS = ASymbol (DT.pack "k")
            let loop = AFor initS condS AVoid AVoid
            checkStmt empty loop `shouldSatisfy` \case
                Right (_, _) -> True
                _ -> False

        it "checkStmt: APos unwraps and returns updated environment" $ do
            let stmt = APos 1 1 (ASetVar (DT.pack "x") (DT.pack "int") (AInteger (fitInteger 42)))
            checkStmt empty stmt `shouldSatisfy` \case
                Right (_, newEnv) -> case Map.lookup (DT.pack "x") (envVars newEnv) of
                    Just TyInt -> True
                    _ -> False
                _ -> False

        it "checkStmt: returns original environment for non-modifying statements" $ do
            checkStmt empty AVoid `shouldSatisfy` \case
                Right (_, resEnv) -> Map.null (envVars resEnv)
                _ -> False

        it "checkFor: propagates envAfterInit and accepts Boolean condition" $ do
            let initS = ASetVar (DT.pack "i") (DT.pack "int") (AInteger (fitInteger 0))
            let condS = ACall (ASymbol (DT.pack "<")) [ASymbol (DT.pack "i"), AInteger (fitInteger 10)]
            let loop = AFor initS condS AVoid AVoid
            
            checkStmt empty loop `shouldSatisfy` \case
                Right (_, _) -> True
                _ -> False

        it "checkFor: accepts Integer condition (TyInt case)" $ do
            let initS = ASetVar (DT.pack "k") (DT.pack "int") (AInteger (fitInteger 1))
            let condS = ASymbol (DT.pack "k")
            let loop = AFor initS condS AVoid AVoid
            
            checkStmt empty loop `shouldSatisfy` \case
                Right (_, _) -> True 
                _ -> False

        it "defineFunc: envWithFunc allows recursion (function visible in body)" $ do
            let body = ACall (ASymbol (DT.pack "rec")) []
            let funcDef = ADefineFunc (DT.pack "rec") [] (DT.pack "void") body
            
            checkStmt empty funcDef `shouldSatisfy` \case
                Right (_, _) -> True
                _ -> False


        it "defineFunc: envForBody includes arguments (lambda insertion check)" $ do
            let args = [(DT.pack "x", DT.pack "int")]
            let body = AReturn (ASymbol (DT.pack "x"))
            let funcDef = ADefineFunc (DT.pack "getX") args (DT.pack "int") body
            
            checkStmt empty funcDef `shouldSatisfy` \case
                Right (_, _) -> True 
                _ -> False
