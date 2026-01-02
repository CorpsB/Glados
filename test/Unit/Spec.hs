module Main (main) where

import Test.Hspec

import qualified Compiler.InstructionSpec
import qualified Compiler.PsInstructionSpec
import qualified Compiler.CompilerStateSpec
import qualified Compiler.ASM.CompilerMonadSpec
import qualified Compiler.ASM.AstToAsmSpec
import qualified Compiler.ASM.CompilerSpec

import qualified AST.AstSpec
import qualified Parser.StatementSpec
import qualified Parser.LexerSpec
import qualified Parser.ExpressionSpec
import qualified Parser.ConditionsSpec
import qualified Compiler.Bytecode.EncoderSpec
import qualified Compiler.EncoderSpec
import qualified AstSpec

import qualified Z_old.Eval.BuiltinsSpec
import qualified Z_old.Eval.ConditionsSpec
import qualified Z_old.Eval.FunctionsSpec
import qualified Z_old.Eval.AstSpec
import qualified Z_old.Eval.RunSpec
import qualified Z_old.Parser.ParserISLTest
import qualified Z_old.Parser.AstSpec
import qualified Z_old.LispTest
import qualified Z_old.AstSpec
import qualified Z_old.Type.IntegerSpec
import qualified Z_old.Utils.ListSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    Z_old.Eval.BuiltinsSpec.spec
    Z_old.Eval.ConditionsSpec.spec
    Z_old.Eval.FunctionsSpec.spec
    Z_old.Eval.RunSpec.spec
    Z_old.Eval.AstSpec.spec
    Z_old.Parser.ParserISLTest.spec
    Z_old.Parser.AstSpec.spec
    Z_old.Type.IntegerSpec.spec
    Z_old.Utils.ListSpec.spec
    Z_old.LispTest.spec
    Z_old.AstSpec.spec
    AST.AstSpec.spec
    Parser.StatementSpec.spec
    Parser.LexerSpec.spec
    Parser.ExpressionSpec.spec
    Parser.ConditionsSpec.spec
    Compiler.Bytecode.EncoderSpec.spec
    Compiler.InstructionSpec.spec
    Compiler.PsInstructionSpec.spec
    Compiler.CompilerStateSpec.spec
    Compiler.ASM.CompilerMonadSpec.spec
    Compiler.ASM.AstToAsmSpec.spec
    Compiler.ASM.CompilerSpec.spec
    Compiler.EncoderSpec.spec
