valid :: String -> Bool
valid s = ' ' `elem` s

split :: String -> (String, Int)
split s = (a, read $ tail b)
   where
       (a, b) = span (/= ' ') s

parse :: String -> Maybe (String, Int)
parse s | valid s = Just (split s)
        | otherwise = Nothing

list2Pair :: [String] -> Maybe [(String, Int)]
list2Pair = mapM parse