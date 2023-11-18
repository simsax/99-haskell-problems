data EncodedListItem a = Single a | Multiple Int a
    deriving (Show)

encodeHead :: Eq a => a -> [a] -> (EncodedListItem a, [a])
encodeHead h xs = encode h xs 1
    where
        encode :: Eq a => a -> [a] -> Int -> (EncodedListItem a, [a])
        encode h [] n = evaluate h [] n
        encode h rest@(x:xs) n 
            | h == x = encode h xs (n + 1)
            | otherwise = evaluate h rest n

        evaluate h rest n
            | n == 1 = (Single h, rest)
            | otherwise = (Multiple n h, rest)

encodeDirect :: Eq a => [a] -> [EncodedListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = let (result, rest) = encodeHead x xs
                    in result : encodeDirect rest



encodeDirect' :: (Eq a) => [a] -> [EncodedListItem a]
encodeDirect' [] = []
encodeDirect' (x:xs) = encodeDirectAcc 1 x xs
    where 
        encodeDirectAcc n y [] = [encodeElement n y]
        encodeDirectAcc n y (x:xs) 
            | y == x    = encodeDirectAcc (n+1) y xs
            | otherwise = encodeElement n y : encodeDirectAcc 1 x xs

        encodeElement 1 y = Single y
        encodeElement n y = Multiple n y
