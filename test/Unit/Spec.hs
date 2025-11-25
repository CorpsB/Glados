module Main (main) where

import Test.Hspec
import qualified Parser.ParserISLTest

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Parser.ParserISLTest.spec
