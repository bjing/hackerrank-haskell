module SimpleTest where

import Test.Hspec

spec :: Spec
spec = do
    describe "This test" $ do
        it "should pass" $ do
            True
