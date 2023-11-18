rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
    | n > 0 = rotate (end ++ h) (n - 1)
    | n < 0 = rotate (t ++ beg) (n + 1)
    | otherwise = xs
    where
        h = [head xs]
        t = [last xs]
        beg = init xs
        end = tail xs

