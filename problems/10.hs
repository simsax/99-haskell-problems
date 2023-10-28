pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (==x) xs
                in (x : first) : pack rest

encode :: Eq a => [a] ->  [(Int, a)]
encode [] = []
encode xs = map (\xs' -> (length xs', head xs')) $ pack xs

encode' :: Eq a => [a] -> [(Int, a)]
encode' xs = [(length xs', head xs') | xs' <- pack xs]
