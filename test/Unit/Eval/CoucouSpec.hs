module Eval.CoucouSpec (spec) where

import Test.Hspec
import System.IO.Silently (capture_)
import qualified Eval.Coucou as Eval

spec :: Spec
spec = describe "Eval.Coucou.sayEval" $
  it "affiche le bon message" $ do
    out <- capture_ Eval.sayEval
    out `shouldBe` "Je compile bien la partie Eval (src/Eval/Coucou.hs) !\n"
