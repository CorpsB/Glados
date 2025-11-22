module Main (main) where

import Test.Hspec
import qualified CoucouSpec
import qualified Parser.CoucouSpec
import qualified Eval.CoucouSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  CoucouSpec.spec
  Parser.CoucouSpec.spec
  Eval.CoucouSpec.spec
