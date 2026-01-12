module Main (main) where

import Test.Hspec

import qualified Compiler.InstructionSpec
import qualified Compiler.PsInstructionSpec
import qualified Compiler.CompilerStateSpec
import qualified Compiler.SerializerSpec
import qualified Compiler.ResolveLabelsSpec
import qualified Compiler.ASM.CompilerMonadSpec
import qualified Compiler.ASM.AstToAsmSpec
import qualified Compiler.ASM.CompilerSpec

import qualified AST.AstSpec
import qualified AST.Semantics.TypeSpec
import qualified AST.Semantics.CheckSpec
import qualified AST.Semantics.CheckCallSpec
import qualified Parser.StatementSpec
import qualified Parser.LexerSpec
import qualified Parser.ExpressionSpec
import qualified Parser.ConditionsSpec
import qualified Compiler.Bytecode.EncoderSpec

import qualified Common.Type.IntegerSpec
import qualified VM.Bytecode.ReaderSpec
import qualified VM.Bytecode.RunnerSpec
import qualified VM.Instruction.ArithmeticSpec
import qualified VM.Instruction.FunctionSpec
import qualified VM.Instruction.IndexSpec
import qualified VM.Instruction.LogicSpec
import qualified VM.Instruction.StackSpec
import qualified VM.Instruction.SystemSpec
import qualified VM.Instruction.VariableSpec
import qualified VM.CallSnapshotSpec
import qualified VM.VMStateSpec
import qualified VM.VMStackSpec
import qualified VM.VMValueSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    AST.AstSpec.spec
    AST.Semantics.TypeSpec.spec
    AST.Semantics.CheckSpec.spec
    AST.Semantics.CheckCallSpec.spec
    Parser.StatementSpec.spec
    Parser.LexerSpec.spec
    Parser.ExpressionSpec.spec
    Parser.ConditionsSpec.spec
    Compiler.Bytecode.EncoderSpec.spec
    Compiler.InstructionSpec.spec
    Compiler.PsInstructionSpec.spec
    Compiler.CompilerStateSpec.spec
    Compiler.SerializerSpec.spec
    Compiler.ResolveLabelsSpec.spec
    Compiler.ASM.CompilerMonadSpec.spec
    Compiler.ASM.AstToAsmSpec.spec
    Compiler.ASM.CompilerSpec.spec
    Common.Type.IntegerSpec.spec
    VM.Bytecode.ReaderSpec.spec
    VM.Bytecode.RunnerSpec.spec
    VM.Instruction.ArithmeticSpec.spec
    VM.Instruction.FunctionSpec.spec
    VM.Instruction.IndexSpec.spec
    VM.Instruction.LogicSpec.spec
    VM.Instruction.StackSpec.spec
    VM.Instruction.SystemSpec.spec
    VM.Instruction.VariableSpec.spec
    VM.CallSnapshotSpec.spec
    VM.VMStateSpec.spec
    VM.VMStackSpec.spec
    VM.VMValueSpec.spec
