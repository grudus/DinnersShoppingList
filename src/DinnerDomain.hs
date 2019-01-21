{-# LANGUAGE 
  OverloadedStrings
, DeriveGeneric
#-}

module DinnerDomain (Ingredient(Ingredient), Dinner(Dinner), meal, name, add) where

import Data.Aeson
import GHC.Generics

data Ingredient = Ingredient {name :: String, amount :: Double, unit :: String} deriving (Show, Generic)
data Dinner = Dinner {meal :: String, ingrediens :: [Ingredient]} deriving (Eq, Show, Generic)

instance FromJSON Ingredient
instance FromJSON Dinner

instance Eq Ingredient where
  (Ingredient name1 _ _) == (Ingredient name2 _ _) = name1 == name2

instance Ord Ingredient where
  (Ingredient name1 _ _) `compare` (Ingredient name2 _ _) = name1 `compare` name2


add :: Ingredient -> Ingredient -> Ingredient
add acc@(Ingredient _ amount1 _) (Ingredient _ amount2 _) = acc { amount = (amount1 + amount2) }