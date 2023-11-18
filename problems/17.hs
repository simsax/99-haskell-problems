split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = f [] xs n
    where
        f first rest@(x:xs) n
            | n == 0 = (first, rest)
            | otherwise = f (first ++ [x]) xs (n - 1)
