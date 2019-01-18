module FileReader (
    readFile'
    ) where

import qualified Data.ByteString.Lazy as L

readFile' :: FilePath -> IO L.ByteString
readFile' = L.readFile