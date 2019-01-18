module DinnerService where


import DinnerDomain

getMeals :: [Dinner] -> [String]
getMeals = map meal

getIngrediensNames :: Dinner -> [String]
getIngrediensNames (Dinner _ ingrediens) = map name ingrediens