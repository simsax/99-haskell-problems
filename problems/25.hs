import System.Random

rndPermu :: [a] -> IO [a]
rndPermu xs = diffSelect' (length xs) xs
    where
        diffSelect' 0 _  = return []
        diffSelect' n xs = do 
            r <- randomRIO (0, length xs - 1)
            let remaining = take r xs ++ drop (r+1) xs
            rest <- diffSelect' (n-1) remaining
            return $ (xs !! r) : rest
