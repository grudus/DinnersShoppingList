module PricingService
    ( calculateTotalPrice
    )
where

import           DinnerDomain
import           PricingDomain
import qualified Data.List                     as List
import qualified Data.Maybe                    as May

calculateTotalPrice :: [Pricing] -> [Ingredient] -> Double
calculateTotalPrice pricings ingredients = sum
    [ x * amount | (Ingredient _ amount _, Just x) <- menu ]
  where
    menu :: [MenuItem]
    menu = zip ingredients $ map findPrice ingredients
    
    findPrice ingredient =
        price <$> List.find (equalPricing ingredient) pricings

    equalPricing (Ingredient iname _ iunit) (Pricing pname _ punit) =
        pname == iname && punit == iunit


type MenuItem = (Ingredient, Maybe Double)
