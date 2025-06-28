-- |

module Stuff where

--dependency for prob 10
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x:takeWhile (==x) xs): pack (dropWhile (==x) xs)

--problem 10
rlEncode :: Eq a => [a] -> [(Int, a)]
rlEncode list = map (\x -> (length x, head x) ) $ pack list

--problem 11
-- Modify the result of problem 10 in such a way
--that if an element has no duplicates it is simply copied into the result list.
--Only elements with duplicates are transferred as (N E) lists.
data NestedList a = Elem a | List [NestedList a]

data RlEncoding a = Single a | Multiple Int a
  deriving(Eq, Ord, Show)

trueRlEncode :: Eq a => [a] -> [RlEncoding a]
trueRlEncode a = map go $ rlEncode a
  where go (1, a) = Single a
        go (mult, a) = Multiple mult a

-- Problem 12
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

rlDecode :: Eq a => [RlEncoding a] -> [a]
rlDecode [Multiple count a] = replicate count a
rlDecode [Single a]         = [a]
rlDecode (x:xs) = rlDecode [x] ++ rlDecode xs

-- Problem 13 Done


--Problem 14
-- (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x : x : acc) []

--Problem 15
-- (**) Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli ls num = foldr (\x acc -> replicate num x ++ acc) [] ls


--Problem 16
-- (**) Drop every N'th element from a list.

myDrop :: Int -> [a] -> [a]
myDrop num xs = aux xs num num
  where
    aux [] _ _ = []
    aux (x:xs) num1 1 = aux xs num1 num1
    aux (x:xs) num1 n = x : aux xs num1 (n-1)

--Problem 17
-- (*) Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> [[a]]
split xs num = [take num xs, drop num xs]

--Problem 18
-- (**) Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
slice xs a b = drop (a-1) $ take b xs

--Problem 19
-- (**) Rotate a list N places to the left. Solutions

rotate :: [a] -> Int -> [a]
rotate xs num =  drop modNum  xs ++ take modNum xs
  where modNum = mod num $ length xs


-- Problem 20
--(*) Remove the K'th element from a list.


removeAt :: Int -> [a] -> ([a],[a])
removeAt num xs = (drop num $ take (num + 1) xs, take num xs ++ drop (num + 1) xs)
