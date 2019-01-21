
module Main where

import JsonFileDinnerReader
import UserInterfaceLoop
import DinnerDomain

main ::  IO ()
main = do
  eitherDinners <- parseFromJsonFile "res/dinners.json" :: IO (Either String [Dinner])
  case eitherDinners of
    Left error -> putStrLn $ "Error while reading json file: " ++ error
    Right dinners -> mainLoop dinners
