module Main (main) where

import Test.Hspec

import qualified Eval.BuiltinsSpec
import qualified Eval.ConditionsSpec
import qualified AstSpec
import qualified Parser.ParserISLTest
import qualified LispTest

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Eval.BuiltinsSpec.spec
  Eval.ConditionsSpec.spec
  AstSpec.spec
  Parser.ParserISLTest.spec
  LispTest.spec
