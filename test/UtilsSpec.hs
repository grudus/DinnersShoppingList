module UtilsSpec where


import Test.Hspec
import Utils


spec =
    describe "Add index prefixes to list" $ do
        it "Should return empty list for empty list" $
            addIndexPrefixes [] `shouldBe` []
        
        it "Should add numbers with order" $
            addIndexPrefixes ["A", "Be", "ceE"] `shouldBe` ["1. A", "2. Be", "3. ceE"]

        it "Should be able to aply order twice" $
            (addIndexPrefixes . addIndexPrefixes) ["A", "Be", "ceE"] `shouldBe` ["1. 1. A", "2. 2. Be", "3. 3. ceE"]