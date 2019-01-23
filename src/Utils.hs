module Utils (orderedList) where

orderedList :: [String] -> [String]
orderedList = map (\(id, item) -> show id ++ ". " ++ item) . zip [1 ..]