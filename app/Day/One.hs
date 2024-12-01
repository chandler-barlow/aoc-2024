{-# LANGUAGE TypeApplications #-}
module Day.One where

import Data.List (sort)
import Data.Bifunctor (bimap)
import Data.Function ((&))
import qualified Data.Map as M
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

path1 :: String
path1 = "/home/cbarlow/Projects/aoc-2024/inputs/One/p1.txt"

toInts :: String -> [[Int]]
toInts = map (map (read @Int) . words) . lines

twoColumns :: [[a]] -> ([a], [a])
twoColumns = foldr f ([], [])
  where f [x, y] (xs, ys) = (x:xs, y:ys)
        -- ^ will fail if the input is malformed

euclidDistance :: Num a =>  a -> a -> a
euclidDistance x y = abs $ x - y

totalDistance :: ([Int], [Int]) -> Int
totalDistance = sum . uncurry (zipWith euclidDistance) 

partOne :: String -> Int
partOne input = toInts input
              & twoColumns
              & bimap sort sort        
              & totalDistance
       
newtype FreqTable a = FreqTable (M.Map a Int)

freqTable :: Ord a => [a] -> FreqTable a
freqTable = FreqTable . foldr (M.alter f) M.empty
  where f v = fmap (+1) v <|> (Just 1)

lookupFreq :: Ord a => a -> FreqTable a -> Int
lookupFreq v (FreqTable table) = fromMaybe 0 $ M.lookup v table  

partTwo :: String -> Int
partTwo input =
  let (xs, ys) = twoColumns $ toInts input
      table = freqTable ys
      counts = map (`lookupFreq` table) xs
  in sum $ zipWith (*) xs counts
            
