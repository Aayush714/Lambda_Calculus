takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs) 
	| f x         = takeUntil f []
	| otherwise   = x: takeUntil f xs

allSquares :: [Int]
allSquares = map (^2) [1..]
