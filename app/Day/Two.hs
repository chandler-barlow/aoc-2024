{-# LANGUAGE TypeApplications #-}
module Day.Two where

testPath :: String
testPath = "/home/cbarlow/Projects/aoc-2024/inputs/Two/input.txt"

toInts :: String -> [[Int]]
toInts = map (map (read @Int) . words) . lines

diffNeighbors :: Num a => [a] -> [a]
diffNeighbors xs = zipWith (-) xs (tail xs)

sign :: Int -> Int
sign n = (abs n) `div` n

isSafe :: [Int] -> Bool
isSafe xs = all (uncurry check) $ zip (head xs' : xs') xs'
  where xs' = diffNeighbors xs
        check prev cur = slowChange cur && signEqual prev cur 
        signEqual x y = sign x == sign y 
        slowChange c = 
          let c' = abs c 
          in c' <= 3 && c' >= 1         

allPossible :: [a] -> [[a]]
allPossible (x:xs) = xs : go [x] xs
  where go :: [a] -> [a] -> [[a]]
        go ys [] = [ys]
        go ys (x:xs) = (ys ++ xs) : go (ys ++ [x]) xs

partOne :: String -> Int
partOne = length . filter isSafe . toInts  

-- | Tries a different combination when one fails  
partTwo :: String -> Int 
partTwo = length 
        . filter (any isSafe) 
        . reverse 
        . map allPossible 
        -- ^ we generate all possible options
        . toInts
