module Day.Four where

type Square a = ((Int, Int), a)
type Board a = [((Int, Int), a)]

data Step = Step (Int, Int) [Step] deriving Show

type Cols = Int
type Rows = Int 
type Depth = Int

index :: Step -> (Int, Int)
index (Step (x,y) _) = (x,y)

children :: Step -> [Step]
children (Step _ xs) = xs

-- | This generates all possibles paths that can be taken on a game board
-- Includes loops and cycles
steps :: Rows -> Cols -> Coord -> Step
steps n m coord = go n m coord 
  where steps = [subtract 1, (+ 1), id]
        inBounds (x,y)
          | x < 0 || y < 0 = False
          | x > n || y > m = False
          | otherwise = True
        go n m (i, j) = 
          let validSteps = filter inBounds next
              next = do 
                i' <- steps <*> pure i
                j' <- steps <*> pure j
                pure (i', j')
              xs = map (go n m) validSteps          
          in Step (i, j) xs

allSteps :: Rows -> Cols -> [Step]
allSteps r c = do
    i <- [0..r]
    j <- [0..c]
    pure $ steps r c (i, j)

-- | Limit a step tree to just non repeating walks at depth n
limitSteps :: Depth -> Step -> [Step] 
limitSteps depth start = go [] depth start 
 where  go :: [(Int, Int)] -> Depth -> Step -> [Step]
        go seen n (Step x xs)
          | x `elem` seen = []
          | n == 1 = [Step x []]
          | otherwise = 
              let next = go (x:seen) (n - 1) <$> xs
              in Step x <$> next

type Coord = (Int, Int)

type Board a = M.Map Coord a

-- | Apply a path to a board
takeWalk :: Board a -> Step -> [[a]]
takeWalk board (Step _ []) = []
takeWalk board (Step x xs) = (board ! x) : (takeWalk board <$> xs)

-- | Look through a grid for a path that finds the sequence
wordSearch :: Eq a => Board a -> [a] -> [[Coord]]
wordSearch board word = undefined

-- parse :: String -> [[Char]]
-- parse input = 
--   let board = lines input
--       n = length . head $ board
--       m = length board
      

-- partOne :: String -> Int
-- partOne input = undefined
