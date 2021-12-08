throuple :: [Int] -> [(Int, Int, Int)]
throuple [] = []
throuple [x] = []
throuple [x,y] = []
throuple (x:y:z:xs) = (x,y,z) : throuple (y:z:xs)


slidingWindow :: [Int] -> [(Int, Int)]
slidingWindow [] = []
slidingWindow [a] = []
slidingWindow (x:y:xs) = (x,y) : slidingWindow (y:xs)

measurementsLarger :: [Int] -> Int
measurementsLarger depths = sum [if x>y then 1 else 0 | (y,x) <- slidingWindow depths]

sonarSum :: [Int] -> [Int]
sonarSum depths = [x+y+z | (x,y,z) <- throuple depths]

readDepths :: IO [Int]
readDepths = do
   input <- readFile "input1.txt"
   let depths = read <$> lines input :: [Int]
   return depths

main :: IO ()
main = do
   depths <- readDepths
   print $ measurementsLarger depths
   print $ measurementsLarger $ sonarSum depths

