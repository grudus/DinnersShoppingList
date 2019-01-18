{-# LANGUAGE 
  OverloadedStrings
, DeriveGeneric
#-}

module DinnerDomain (Ingredient(Ingredient), Dinner(Dinner), meal, name) where

import Data.Aeson
import GHC.Generics

data Ingredient = Ingredient {name :: String, amount :: Double, unit :: String} deriving (Eq, Show, Generic)
data Dinner = Dinner {meal :: String, ingrediens :: [Ingredient]} deriving (Eq, Show, Generic)

instance FromJSON Ingredient
instance FromJSON Dinner