myLast :: [a] -> a
myLast [x] = x
myLast(x:xs) = myLast xs

myLast' :: [a] -> a
myLast' xs = head (reverse xs)

myLast'' :: [a] -> a
myLast'' [x] = x
myLast'' xs = myLast (tail xs)

myLast''' :: [a] -> a
myLast''' xs = xs !! (length xs - 1)
