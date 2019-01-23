{-# LANGUAGE 
  OverloadedStrings
, DeriveGeneric
#-}

module DinnerDomain
  ( Ingredient(Ingredient)
  , Dinner(Dinner)
  , Unit(..)
  , meal
  , name
  , add
  , detailedInfo
  )
where

import           Data.Aeson
import           GHC.Generics
import qualified Text.Printf                   as Printf

data Unit = Kg | Pack | Item deriving (Eq, Show, Read, Generic)
data Ingredient = Ingredient {name :: String, amount :: Double, unit :: Unit} deriving (Show, Generic)
data Dinner = Dinner {meal :: String, ingrediens :: [Ingredient]} deriving (Eq, Show, Generic)


instance FromJSON Unit
instance FromJSON Ingredient
instance FromJSON Dinner

instance Eq Ingredient where
  (Ingredient name1 _ _) == (Ingredient name2 _ _) = name1 == name2

instance Ord Ingredient where
  (Ingredient name1 _ _) `compare` (Ingredient name2 _ _) =
    name1 `compare` name2


unitString :: Unit -> String
unitString (Kg  ) = "kg"
unitString (Pack) = "paczk"
unitString (Item) = "szt"

detailedInfo :: Int -> Ingredient -> String
detailedInfo maxMealLength (Ingredient name amount unit) =
  Printf.printf ("%-" ++ (show maxMealLength) ++ "s | %.1f %s") name amount (unitString unit)

add :: Ingredient -> Ingredient -> Ingredient
add acc@(Ingredient name1 amount1 unit1) (Ingredient name2 amount2 unit2) =
  if name1 /= name2 || unit1 /= unit2
    then error "Names and units must match"
    else acc { amount = (amount1 + amount2) }
