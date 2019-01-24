{-# LANGUAGE 
  OverloadedStrings
, DeriveGeneric
#-}

module PricingDomain
  ( Pricing(..)
  )
where

import           DinnerDomain
import           Data.Aeson
import           GHC.Generics

data Pricing = Pricing {name :: String, price :: Double, unit :: Unit} deriving (Show, Generic)

instance FromJSON Pricing

instance Eq Pricing where
  (Pricing name1 _ _) == (Pricing name2 _ _) = name1 == name2

instance Ord Pricing where
  (Pricing name1 _ _) `compare` (Pricing name2 _ _) = name1 `compare` name2


