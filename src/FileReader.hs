module FileReader (
    readFile'
    ) where

import qualified Data.ByteString.Lazy as L

readFile' :: FilePath -> IO L.ByteString
readFile' path = L.readFile path