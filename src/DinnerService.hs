module DinnerService (getMeals, getIngrediensNames, orderedMeals, findDinnersSelectedByUser) where


import DinnerDomain
import qualified Text.Read as Text 

getMeals :: [Dinner] -> [String]
getMeals = map meal


getIngrediensNames :: Dinner -> [String]
getIngrediensNames (Dinner _ ingrediens) = map name ingrediens


orderedMeals :: [Dinner] -> [String]
orderedMeals = map (\(id, dinner) -> (show id) ++ ". " ++ meal dinner) . zip [1..]


data UserInput = Number Int | Word String  

parseUserInput :: String -> UserInput
parseUserInput word =
    case (Text.readMaybe word :: Maybe Int) of 
        Just a -> Number a
        Nothing -> Word word


findDinnersSelectedByUser :: String -> [Dinner] -> [Dinner]
findDinnersSelectedByUser "" _ = []
findDinnersSelectedByUser _ [] = []
findDinnersSelectedByUser input dinners = 
    map fst . filter userInputPredicate $ zip dinners [1..] 
    where
        parsedUserInput = map parseUserInput $ words input 
        userInputPredicate (dinner, index) = 
            any (\userInput -> 
                case userInput of 
                    Number a -> index == a
                    Word a -> meal dinner == a
                    ) parsedUserInput