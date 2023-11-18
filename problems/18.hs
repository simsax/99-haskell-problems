slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs s e = take (e - s + 1) $ drop (s - 1) xs
