module DinnerService
    ( getMeals
    , getIngrediensNames
    , orderedMeals
    , findDinnersSelectedByUser
    , findRequiredIngredients
    )
where

import qualified Data.List                     as List
import           DinnerDomain
import qualified Text.Read                     as Text
import qualified Data.Char                     as Char
import qualified Utils

getMeals :: [Dinner] -> [String]
getMeals = map meal

getIngrediensNames :: Dinner -> [String]
getIngrediensNames (Dinner _ ingrediens) = map name ingrediens

orderedMeals :: [Dinner] -> [String]
orderedMeals = Utils.orderedList . getMeals


data UserInput
  = Number Int
  | Word String

parseUserInput :: String -> UserInput
parseUserInput word = case (Text.readMaybe word :: Maybe Int) of
    Just a  -> Number a
    Nothing -> Word word

findDinnersSelectedByUser :: String -> [Dinner] -> [Dinner]
findDinnersSelectedByUser "" _  = []
findDinnersSelectedByUser _  [] = []
findDinnersSelectedByUser input dinners =
    map fst . filter userInputPredicate $ zip dinners [1 ..]
  where
    parsedUserInput = map parseUserInput $ words input
    userInputPredicate (dinner, index) = any
        (\userInput -> case userInput of
            Number a -> index == a
            Word   a -> map Char.toLower a
                `List.isPrefixOf` map Char.toLower (meal dinner)
        )
        parsedUserInput

findRequiredIngredients :: [Dinner] -> [Ingredient]
findRequiredIngredients []      = []
findRequiredIngredients dinners = List.map sumIngredients
    $ List.group sortedIngredients
  where
    sumIngredients = foldl1 add
    sortedIngredients =
        List.sort . concatMap (\(Dinner _ ingrediens) -> ingrediens) $ dinners
