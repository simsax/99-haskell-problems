myLast [x] = x
myLast(x:xs) = myLast xs

myLast' xs = head (reverse xs)

myLast'' [x] = x
myLast'' xs = myLast (tail xs)

myLast''' xs = xs !! (length xs - 1)
