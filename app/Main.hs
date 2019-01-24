
module Main where

import           JsonFileReader
import           UserInterfaceLoop
import           DinnerDomain
import           PricingDomain


parseDinners :: FilePath -> IO (Either String [Dinner])
parseDinners = parseFromJsonFile

parsePricing :: FilePath -> IO (Either String [Pricing])
parsePricing = parseFromJsonFile

main :: IO ()
main = do
  dinnersEither <- parseDinners "res/dinners.json"
  pricingEither <- parsePricing "res/pricing.json"
  
  case (,) <$> dinnersEither <*> pricingEither of
    Left  error   -> putStrLn $ "Error while json file: " ++ error
    Right (dinners, pricing) -> mainLoop dinners pricing
