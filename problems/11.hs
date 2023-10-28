data EncodedListItem a = Single a | Multiple Int a
    deriving (Show)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (==x) xs
                in (x : first) : pack rest

encodeModified :: Eq a => [a] -> [EncodedListItem a]
encodeModified xs = [if l > 1 then Multiple l x else Single x
    | xs' <- pack xs, let l = length xs', let x = head xs']
