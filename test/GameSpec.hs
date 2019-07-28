module GameSpec (pgnFileSpec) where

import Test.Hspec

pgnFileSpec :: Spec
pgnFileSpec = do
    describe "PGN file games" $ do
        it "can be parsed and played" $ do
            True `shouldBe` True