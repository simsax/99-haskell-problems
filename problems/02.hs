myButLast :: [a] -> a
myButLast [x, y] = x
myButLast(x:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' xs = reverse xs !! 1

myButLast'' :: [a] -> a
myButLast'' xs = head (tail (reverse xs))
