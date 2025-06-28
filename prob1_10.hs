module Problems1_10 where

--1
-- find the last element of a list
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--2
--  Find the last-but-one (or second-last) element of a list.
myButLast :: [a] -> a
myButLast []       = error "empty list"
myButLast (x:[])   = error "single element list"
myButLast (a:b:[]) = a
myButLast (x:xs)   = myButLast xs

--3
-- (*) Find the K'th element of a list.
elementAt :: [a] -> Int -> a
elementAt [] _   = error "tried to index empty list"
elementAt list k
  | k > length list = error "index out of bounds"
  | otherwise = list !! (k - 1)

--4
--(*) Find the number of elements in a list.
--not using length
myLength :: [a] -> Int
myLength list = aux list 0
  where
    aux [] acc = 0
    aux (x:[]) acc = acc + 1
    aux (x:xs) acc = aux xs (acc + 1) 

--5
--(*) Reverse a list.
--reverse 
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]


--6
--(*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = (\x -> x == myReverse x)

--7
--(**) Flatten a nested list structure.
--helped
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a)      = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List [])     = []

--8
--(**) Eliminate consecutive duplicates of list elements.

compress ::(Eq a) => [a] -> [a]
compress []     = []
compress (a:[]) = [a]
compress (a:b:xs)
  | a == b      = compress (a:xs)
  | otherwise   = a:compress (b:xs)

--9
--(**) Pack consecutive duplicates of list elements into sublists.
--helped
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x:takeWhile (==x) xs): pack (dropWhile (==x) xs)

--10
--(*) Run-length encoding of a list.

rlEncode :: Eq a => [a] -> [(Int, a)]
rlEncode list = map (\x -> (length x, head x) ) $ pack list 
