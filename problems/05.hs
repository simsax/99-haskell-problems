myReverse :: [a] -> [a]
myReverse xs = myReverseAcc xs []
  where
    myReverseAcc [] rs = rs
    myReverseAcc (x : xs) rs = myReverseAcc xs (x : rs)