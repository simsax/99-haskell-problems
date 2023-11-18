repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli (x:xs) n = replicate n x ++ repli xs n 

repli' xs n = concatMap (replicate n) xs
