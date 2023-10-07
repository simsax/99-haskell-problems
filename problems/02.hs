myButLast [x, y] = x
myButLast(x:xs) = myButLast xs

myButLast' xs = reverse xs !! 1