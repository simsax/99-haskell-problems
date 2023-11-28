import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
   let pureGen = mkStdGen 69 
   return $ take n [xs !! i | i <- randomRs (0, length xs - 1) pureGen]
