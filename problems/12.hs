data EncodedListItem a = Single a | Multiple Int a
    deriving (Show)

decodeItem :: EncodedListItem a -> [a]
decodeItem (Single x) = [x]
decodeItem (Multiple n x) = replicate n x

decodeModified :: Eq a => [EncodedListItem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeItem x ++ decodeModified xs

decodeModified' :: Eq a => [EncodedListItem a] -> [a]
decodeModified' = foldr ((++) . decodeItem) []

decodeModified'' :: Eq a => [EncodedListItem a] -> [a]
decodeModified'' = concatMap decodeItem

