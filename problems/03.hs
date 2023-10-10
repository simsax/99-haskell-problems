
elementAt :: [a] -> Int -> a
elementAt xs i = if i > 0 && i <= length xs then xs !! (i - 1) else error "Index out of bounds"

elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "Index out of bounds"
elementAt' (x:_) 1 = x
elementAt' (_:xs) i
    | i < 1 = error "Index out of bounds"
    | otherwise = elementAt' xs (i - 1)
