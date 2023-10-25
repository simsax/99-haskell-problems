mySpan :: (a -> Bool) -> [a] -> ([a],[a])
mySpan _ [] = ([], [])
mySpan p (x:xs)
    | p x = (x:ys,zs)
    | otherwise = ([], x:xs)
    where
        (ys,zs) = mySpan p xs

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = ls : pack rs
    where
        (ls, rs) = mySpan (==x) (x:xs)