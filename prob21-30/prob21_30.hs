-- |

module Twenties where

import System.Random
--Problem 21
--Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt a xs pos = fst splitten ++ [a] ++ snd splitten
  where splitten = splitAt pos xs


--Problem 21
-- Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range a b = [a..b]
