module JsonFileReader
    ( parseFromJsonFile
    )
where

import           DinnerDomain
import           Data.Aeson
import qualified Data.ByteString.Lazy          as L


parseFromJsonFile :: FromJSON a => FilePath -> IO (Either String a)
parseFromJsonFile path = do
    jsonFileContent <- L.readFile path
    return $ eitherDecode jsonFileContent