module PricingService (findPricingsForIngredients) where

import DinnerDomain
import PricingDomain
import qualified Data.List as List
import qualified Data.Maybe as May

findPricingsForIngredients ::  [Pricing] -> [Ingredient] -> Double
findPricingsForIngredients pricings ingredients =
    sum [x * amount | (Ingredient _ amount _, Just x) <- menu] 
        where
            menu = zip ingredients $ map findPrice ingredients
            findPrice (Ingredient iname _ iunit) = price <$> List.find (\(Pricing pname _ punit) -> pname == iname && punit == iunit) pricings


type MenuItem = (Ingredient, Maybe Double)