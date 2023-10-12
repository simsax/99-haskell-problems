myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = myLengthTail xs 0
    where
        myLengthTail [] acc = acc
        myLengthTail (_:xs) acc = myLengthTail xs (acc + 1)