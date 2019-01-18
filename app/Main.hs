
module Main where

import JsonFileDinnerReader
import DinnerDomain
import DinnerService 


main ::  IO ()
main = do
  eitherDinners <- parseFromJsonFile "res/dinners.json" :: IO (Either String [Dinner])
  case eitherDinners of
    Left error -> putStrLn $ "error: " ++ error
    Right dinners -> do
      print $ getMeals dinners
      print $ getIngrediensNames $ head dinners