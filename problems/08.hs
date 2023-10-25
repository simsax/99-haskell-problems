consumeWhile :: (a -> Bool) -> [a] -> [a]
consumeWhile p [] = []
consumeWhile p (x:xs)
    | p x = consumeWhile p xs
    | otherwise = x:xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (consumeWhile (== x) xs)
