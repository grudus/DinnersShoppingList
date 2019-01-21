module JsonFileDinnerReader (parseFromJsonFile) where

import DinnerDomain
import FileReader
import Data.Aeson


parseFromJsonFile :: FromJSON a => FilePath -> IO (Either String a)
parseFromJsonFile path = do
    json <- readFile' path
    return $ eitherDecode json
