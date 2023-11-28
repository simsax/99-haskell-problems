import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n to = diffSelect' n [1..to]
    where
        diffSelect' 0 _  = return []
        diffSelect' _ [] = error "too few elements to choose from"
        diffSelect' n xs = do 
            r <- randomRIO (0, length xs - 1)
            let remaining = take r xs ++ drop (r+1) xs
            rest <- diffSelect' (n-1) remaining
            return $ (xs !! r) : rest
