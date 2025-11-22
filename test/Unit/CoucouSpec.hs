module CoucouSpec (spec) where

import Test.Hspec
import System.IO.Silently (capture_)
import qualified Coucou

spec :: Spec
spec = describe "Coucou.sayRoot" $
  it "affiche le bon message" $ do
    out <- capture_ Coucou.sayRoot
    out `shouldBe` "Je compile bien la partie générale (src/Coucou.hs) !\n"
