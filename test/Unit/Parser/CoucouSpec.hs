module Parser.CoucouSpec (spec) where

import Test.Hspec
import System.IO.Silently (capture_)
import qualified Parser.Coucou as Parser

spec :: Spec
spec = describe "Parser.Coucou.sayParser" $
  it "affiche le bon message" $ do
    out <- capture_ Parser.sayParser
    out `shouldBe` "Je compile bien la partie Parser (src/Parser/Coucou.hs)!\n"
