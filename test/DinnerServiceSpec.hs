module DinnerServiceSpec where


import Test.Hspec
import DinnerDomain
import DinnerService


dinners = [
    Dinner "Pałki" [Ingredient "Pałki" 1.0 "kg"], 
    Dinner "Tortilla" [Ingredient "Ogórek" 1.0 "item", Ingredient "Żółtko" 3.0 "item"],
    Dinner "Kesadija" [Ingredient "Łabądź" 0.1 "pack", Ingredient "Ogórek" 3.5 "item"]
    ]

spec = do
    describe "Finding dinners selected by user" $ do
        it "Should return empty list when no input specified" $
            findDinnersSelectedByUser "" dinners `shouldBe` []

        it "Should return empty list when no dinners available" $
            findDinnersSelectedByUser "1 2 3" [] `shouldBe` []
        
        it "Should return filtered list when dinner ids passed" $
            findDinnersSelectedByUser "3 1" dinners `shouldBe` [ Dinner "Pałki" [Ingredient "Pałki" 1.0 "kg"], Dinner "Kesadija" [Ingredient "Łabądź" 0.1 "pack", Ingredient "Ogórek" 3.5 "item"]]

        it "Should return filtered single element for single user input" $
            findDinnersSelectedByUser "1" dinners `shouldBe` [head dinners]

        it "Should return filtered only element with valid id" $
            findDinnersSelectedByUser "1 5 12 65 321" dinners `shouldBe` [head dinners]

        it "Should return filtered list when dinner ids passed separated by multiple spaces" $
            findDinnersSelectedByUser "  3  1   2   " dinners `shouldBe` dinners

        it "Should return filtered list when dinner meal names passed" $
            findDinnersSelectedByUser "Pałki Kesadija" dinners `shouldBe` [ Dinner "Pałki" [Ingredient "Pałki" 1.0 "kg"], Dinner "Kesadija" [Ingredient "Łabądź" 0.1 "pack", Ingredient "Ogórek" 3.5 "item"]]

        it "Should return empty list when no valid meal names" $
            findDinnersSelectedByUser "Pałeczki Chleb" dinners `shouldBe` []

        it "Should return filtered list both ids and dinner meal names passed" $
            findDinnersSelectedByUser "Pałki 3" dinners `shouldBe` [ Dinner "Pałki" [Ingredient "Pałki" 1.0 "kg"], Dinner "Kesadija" [Ingredient "Łabądź" 0.1 "pack", Ingredient "Ogórek" 3.5 "item"]]
    
    describe "Mapping dinners to ordered string list" $ do
        it "Should return empty list for empty dinner list" $
            orderedMeals [] `shouldBe` []

        it "Should return single element for one-element dinners list" $
            orderedMeals [head dinners] `shouldBe` ["1. Pałki"]

        it "Should render ordered list properly" $
            orderedMeals dinners `shouldBe` ["1. Pałki", "2. Tortilla", "3. Kesadija"]
        
    describe "Getting list of ingredient names" $ do
        it "Should return empty list for dinner with no ingrediens" $
            getIngrediensNames (Dinner "meal" []) `shouldBe` []

        it "Should return it correctly" $
            getIngrediensNames (last dinners) `shouldBe` ["Łabądź", "Ogórek"]