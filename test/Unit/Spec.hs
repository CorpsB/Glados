module Main (main) where

import Test.Hspec
import qualified Parser.ParserISLTest
import qualified LispTest

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Parser.ParserISLTest.spec
  LispTest.spec
