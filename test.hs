-- Each step is a rose tree
data Step = Step (Int, Int) [Step] deriving Show

index :: Step -> (Int, Int)
index (Step (x,y) _) = (x,y)

children :: Step -> [Step]
children (Step _ xs) = xs


-- | This generates all possibles paths that can be taken on a game board
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

-- | Limit a step tree to just non repeating walks at depth n
limitSteps :: Depth -> Step -> [Step] 
limitSteps depth start = go [] depth start 
 where  go :: [(Int, Int)] -> Depth -> Step -> [Step]
        go seen n s@(Step x xs)
          | x `elem` seen = []
          | n == 1 = [Step x []]
          | otherwise = 
              let next = go (x:seen) (n - 1) <$> xs
              in Step x <$> next
