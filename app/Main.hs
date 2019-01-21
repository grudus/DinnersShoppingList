
module Main where

import JsonFileDinnerReader
import DinnerDomain
import DinnerService 
import qualified ConsoleLogger as Logger


main ::  IO ()
main = do
  eitherDinners <- parseFromJsonFile "res/dinners.json" :: IO (Either String [Dinner])
  case eitherDinners of
    Left error -> putStrLn $ "error: " ++ error
    Right dinners -> do
      Logger.log . getIngrediensNames . head $ dinners