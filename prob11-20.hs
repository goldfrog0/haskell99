--11
--(*) Modified run-length encoding.

data RLencode a = Single a | Multiple Int a
  deriving(Show)

pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x:takeWhile (==x) xs): pack (dropWhile (==x) xs)


rlEncode :: Eq a => [a] -> [(Int, a)]
rlEncode list = map (\x -> (length x, head x) ) $ pack list 

rlEncodeM :: (Eq a) => [a] -> [RLencode a]
rlEncodeM = map  tupToRL .  rlEncode 

tupToRL :: (Int, a) -> RLencode a
tupToRL (num, a)
  | num == 1 = Single a
  | otherwise = Multiple num a

--12
--(**) Decode a run-length encoded list.
--data RLencode a = Single a | Multiple Int a
--  deriving(Show)

rlDecode :: [RLencode a] -> [a]
rlDecode  = concat .  map rlDecodeSingle  


rlDecodeSingle :: RLencode a -> [a]
rlDecodeSingle (Single a) = [a]
rlDecodeSingle (Multiple int a) = replicate int a

--13
--(**) Run-length encoding of a list (direct solution).
--decodeDirect :: Eq a => [a] -> [RLencode a]
--decodeDirect (x:xs) = undefined 

--elementEncoder :: Eq a => a -> RLencode a
--elementEncoder 1 a = Single a
--elementEncoder n a = Multiple n a 

--14
--(*) Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs


--15
--(**) Replicate the elements of a list a given number of times.
repli :: Int -> [a] -> [a]
repli _ []       = []
repli num (x:xs) = replicate num x ++ repli num xs

--16
--(**) Drop every N'th element from a list.
dropEvery :: Int -> [a] -> [a]
--dropEvery _ [] = []
dropEvery num list = aux num list num
  where
    aux num [] acc = []
    aux num (x:xs) acc
      | acc == 1       = aux num xs num
      | otherwise      = x:aux num xs (acc - 1)  
