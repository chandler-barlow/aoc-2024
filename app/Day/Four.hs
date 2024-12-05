module Day.Four where

type Square a = ((Int, Int), a)
type Board a = [((Int, Int), a)]

data Step = Step (Int, Int) [Step] deriving Show

index :: Step -> (Int, Int)
index (Step (x,y) _) = (x,y)

children :: Step -> [Step]
children (Step _ xs) = xs

-- | This generates all possibles paths that can be taken on a game board
-- Includes loops and cycles
allSteps :: Int -> Int -> Step
allSteps n m = go n m 0 0
  where steps = [subtract 1, (+ 1), id]
        inBounds (x,y)
          | x < 0 || y < 0 = False
          | x > n || y > m = False
          | otherwise = True
        go n m i j = 
          let validSteps = filter inBounds next
              next = do 
                i' <- steps <*> pure i
                j' <- steps <*> pure j
                pure (i', j')
              xs = map (uncurry $ go n m) validSteps          
          in Step (i, j) xs

type Cols = Int
type Rows = Int 
type Depth = Int

-- | Never walks the same ground twice
finite :: Step -> Step
finite (Step x xs) = Step x $ go [index x] xs
  where go seen xs = 
          let valid = filter (\x -> index x `notElem` seen) xs
              

-- All non backtracking walks of depth n
walkBoard :: Rows -> Cols -> Depth -> Step -> [[Step]] 
walkBoard n m depth start = go [] depth start 
 where  go :: [(Int, Int)] -> Depth -> Step -> [[Step]]
        go seen n (Step x xs)
          | (index x) `elem` seen 
          |  n == 1 = []
          | otherwise = 
              let x' = index x
                  next = go (n - 1) (x':seen) xs
              in map (x:) next

-- allCombos :: Int -> Board a -> [[a]]
-- allCombos depth = undefined

-- parse :: String -> [[Char]]
-- parse input = 
--   let board = lines input
--       n = length . head $ board
--       m = length board
      

-- partOne :: String -> Int
-- partOne input = undefined
