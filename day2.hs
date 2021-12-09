import Data.List

main :: IO ()
main = do 
    input <- readFile "input2.txt"
    let directions = lines input
    let finalpos = part1 directions
    print finalpos



parse :: String -> (Char, Int)
parse line = (head dir, read steps)
   where (dir:steps:_) = words line

-- movement :: (Int, Int) -> (Char, Int) -> (Int, Int)
-- movement (h,d) (c, t)
--    | (h, d) ('f', t) = (h+t, d)
--    | (h, d) ('u', t) = (h, d-t)
--    | (h, d) ('d', t) = (h, d+t)
--    | otherwise   = (h,d)

--move :: [(Char, Int)] -> (Int, Int)
--move list = (h,d)
--   where
--       (h,d) = foldl' movement (0,0) list

part1 :: [String] -> Int
part1 list = h*d
   where
       (h,d) = foldl' go (0,0) (parse <$> list)
       go (h,d) ('f', x) = (h+x,d)
       go (h,d) ('u', x) = (h, d-x)
       go (h,d) ('d', x) = (h, d+x)