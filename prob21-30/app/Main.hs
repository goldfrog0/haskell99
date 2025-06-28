module Main where
import System.Random (randomRIO)


--Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect [] _ = return []
rndSelect xs num =
    sequence $ replicate num $
    randomRIO (0, length xs - 1)
    >>= \idx -> return $ xs !! idx

--Problem 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect count num =
    sequence $ replicate count $
    randomRIO (1, num)


main :: IO ()
main = do
    result <- rndSelect "abcdefgh" 3
    putStrLn result
