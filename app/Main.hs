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


data DupaData = DupaConstruct
  { dupa :: Int
  } deriving (Show, Generic)


instance FromJSON DupaData


jsonFile :: FilePath
jsonFile = "res/dinners.json"


getJSON :: IO L.ByteString
getJSON = L.readFile jsonFile


main ::  IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String DupaData)
  case d of
    Left err -> putStrLn $ "error: " ++ err
    Right cs -> do
      print $ cs