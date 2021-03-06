module DinnerServiceSpec where


import Test.Hspec
import DinnerDomain
import DinnerService


dinners = [
    Dinner "Pałki" [Ingredient "Pałki" 1.0 Kg], 
    Dinner "Tortilla" [Ingredient "Ogórek" 1.0 Item, Ingredient "Żółtko" 3.0 Item],
    Dinner "Kesadija" [Ingredient "Łabądź" 0.1 Pack, Ingredient "Ogórek" 3.5 Item]
    ]

spec = do
    describe "Finding dinners selected by user" $ do
        it "Should return empty list when no input specified" $
            findDinnersSelectedByUser "" dinners `shouldBe` []

        it "Should return empty list when no dinners available" $
            findDinnersSelectedByUser "1 2 3" [] `shouldBe` []
        
        it "Should return filtered list when dinner ids passed" $
            findDinnersSelectedByUser "3 1" dinners `shouldBe` [ Dinner "Pałki" [Ingredient "Pałki" 1.0 Kg], Dinner "Kesadija" [Ingredient "Łabądź" 0.1 Pack, Ingredient "Ogórek" 3.5 Item]]

        it "Should return filtered single element for single user input" $
            findDinnersSelectedByUser "1" dinners `shouldBe` [head dinners]

        it "Should return filtered only element with valid id" $
            findDinnersSelectedByUser "1 5 12 65 321" dinners `shouldBe` [head dinners]

        it "Should return filtered list when dinner ids passed separated by multiple spaces" $
            findDinnersSelectedByUser "  3  1   2   " dinners `shouldBe` dinners

        it "Should return filtered list when dinner meal names passed" $
            findDinnersSelectedByUser "Pałki Kesadija" dinners `shouldBe` [ Dinner "Pałki" [Ingredient "Pałki" 1.0 Kg], Dinner "Kesadija" [Ingredient "Łabądź" 0.1 Pack, Ingredient "Ogórek" 3.5 Item]]

        it "Should return empty list when no valid meal names" $
            findDinnersSelectedByUser "Pałeczki Chleb" dinners `shouldBe` []

        it "Should return filtered list both ids and dinner meal names passed" $
            findDinnersSelectedByUser "Pałki 3" dinners `shouldBe` [ Dinner "Pałki" [Ingredient "Pałki" 1.0 Kg], Dinner "Kesadija" [Ingredient "Łabądź" 0.1 Pack, Ingredient "Ogórek" 3.5 Item]]
    
        it "Should return filtered result when prefix of name is fiven" $
            findDinnersSelectedByUser "Tort Pał kesa" dinners `shouldBe` dinners

    describe "Adding indexes to dinners" $ do
        it "Should return empty list for empty dinner list" $
            mealsWithIndex [] `shouldBe` []

        it "Should return single element for one-element dinners list" $
            mealsWithIndex [head dinners] `shouldBe` ["1. Pałki"]

        it "Should render ordered list properly" $
            mealsWithIndex dinners `shouldBe` ["1. Pałki", "2. Tortilla", "3. Kesadija"]
        
    describe "Getting list of ingredient names" $ do
        it "Should return empty list for dinner with no ingrediens" $
            getIngrediensNames (Dinner "meal" []) `shouldBe` []

        it "Should return it correctly" $
            getIngrediensNames (last dinners) `shouldBe` ["Łabądź", "Ogórek"]
    
    describe "Finding required ingredients" $ do
        it "Should return empty list for empty list" $
            findRequiredIngredients [] `shouldBe` []
        
        it "Should reteurn empty list for dinner without integredients" $
            findRequiredIngredients [Dinner "meal" []] `shouldBe` []
        
        it "Should sum duplicated elements" $ do
            aAmount `shouldBe` 4.5
            bAmount `shouldBe` 2.0
            length ingredients `shouldBe` 2
            where
                ingredients = findRequiredIngredients [Dinner "meal1" [Ingredient "a" 1.0 Kg, Ingredient "b" 2.0 Kg], Dinner "meal2" [Ingredient "a" 3.5 Kg]]
                getAmount (Ingredient _ am _) = am
                aAmount = getAmount $ head ingredients
                bAmount = getAmount $ last ingredients 
                    

                    