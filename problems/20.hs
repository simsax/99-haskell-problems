removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
    | n > 0 && n <= length xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
    | otherwise = error "Index out of bounds"
