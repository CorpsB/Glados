module Main (main) where

import Test.Hspec

import qualified Eval.BuiltinsSpec
import qualified Eval.ConditionsSpec
import qualified Eval.FunctionsSpec
import qualified AstSpec
import qualified Parser.ParserISLTest
import qualified Parser.AstSpec
import qualified LispTest
import qualified Utils.ListSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Eval.BuiltinsSpec.spec
  Eval.ConditionsSpec.spec
  Eval.FunctionsSpec.spec
  AstSpec.spec
  Parser.ParserISLTest.spec
  Parser.AstSpec.spec
  Utils.ListSpec.spec
  LispTest.spec
