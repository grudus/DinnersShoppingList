
module Main where

import JsonFileDinnerReader
import DinnerDomain


main ::  IO ()
main = do
  eitherDinners <- parseFromJsonFile "res/dinners.json" :: IO (Either String [Dinner])
  case eitherDinners of
    Left error -> putStrLn $ "error: " ++ error
    Right dinners ->
      print dinners