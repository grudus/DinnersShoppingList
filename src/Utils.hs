module Utils
    ( addIndexPrefixes
    )
where

addIndexPrefixes :: [String] -> [String]
addIndexPrefixes = map (\(id, item) -> show id ++ ". " ++ item) . zip [1 ..]
