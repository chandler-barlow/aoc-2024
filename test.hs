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

