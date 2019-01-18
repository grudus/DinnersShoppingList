{-# LANGUAGE 
  OverloadedStrings
, DeriveGeneric
#-}

module Main where

import Prelude as P
import Data.Aeson
import Data.Maybe
import Text.Read
import GHC.Generics
import qualified Data.ByteString.Lazy as L

data Ingredient = Ingredient {name :: String, amount :: Double, unit :: String} deriving (Eq, Show, Generic)
data Dinner = Dinner {meal :: String, ingrediens :: [Ingredient]} deriving (Eq, Show, Generic)

instance FromJSON Ingredient
instance FromJSON Dinner

jsonFile :: FilePath
jsonFile = "res/dinners.json"


getJSON :: IO L.ByteString
getJSON = L.readFile jsonFile


main ::  IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Dinner])
  case d of
    Left err -> putStrLn $ "error: " ++ err
    Right cs -> do
      print $ cs