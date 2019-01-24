module PricingServiceSpec where


import Test.Hspec
import PricingService
import DinnerDomain
import PricingDomain


pricing :: [Pricing]
pricing = [Pricing "a" 5.0 Kg, Pricing "b" 3.0 Pack, Pricing "c" 10.5 Kg, Pricing "a" 1.0 Item]

spec =
    describe "Finding total price of ingredients" $ do
        it "Should return 0 when pricing or ingredient list is empty" $ do
            findPricingsForIngredients [] [] `shouldBe` 0
            findPricingsForIngredients pricing [] `shouldBe` 0
            findPricingsForIngredients [] [Ingredient "a" 1.0 Kg] `shouldBe` 0
        
        it "Should return 0 when no ingredients in pricing" $
            findPricingsForIngredients pricing [Ingredient "xx" 1.0 Item] `shouldBe` 0
        
        it "Should return 0 when name matches, but unit doesn't" $
            findPricingsForIngredients pricing [Ingredient "a" 1.0 Pack] `shouldBe` 0
        
        it "Should calculate correctly" $
            findPricingsForIngredients pricing [Ingredient "a" 1.0 Kg, Ingredient "b" 1.0 Pack] `shouldBe` 8.0

        it "Should calculate correctly when amount /= 1" $
            findPricingsForIngredients pricing [Ingredient "a" 0.1 Kg, Ingredient "b" 10.0 Pack] `shouldBe` 30.5

        it "Should calculate correctly when multiple identical ingredients" $
            findPricingsForIngredients pricing [Ingredient "c" 1 Kg, Ingredient "c" 2 Kg] `shouldBe` 31.5

        it "Should calculate correctly when multiple identical ingredients and no pricing available for some ingredients" $
            findPricingsForIngredients pricing ingredients `shouldBe` 14.0
                where
                    ingredients = [
                        Ingredient "a" 0.2 Kg, 
                        Ingredient "a" 2.0 Item, 
                        Ingredient "b" 2.0 Pack,
                        Ingredient "xxx" 1.0 Item, 
                        Ingredient "b" 1.0 Kg,
                        Ingredient "a" 1.0 Kg
                        ]
