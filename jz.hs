-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast _ = error "myLast called on empty list"

-- Problem 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "myButLast called on too short a list"

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs $ k - 1
elementAt _ _ = error "Index out of range"

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ (flatten $ List xs)
flatten (List []) = []

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress (x:y:xs)
    | x == y = compress $ y:xs
    | otherwise = x:(compress $ y:xs)
compress xs = xs

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack xs =
    let split :: (Eq a) => [a] -> ([a], [a])
        split [] = ([], [])
        split (x:[]) = ([x], [])
        split (x:y:xs)
            | x == y = (x:p, r)
            | otherwise = ([x], y:xs)
            where (p, r) = split $ y:xs
        (p, r) = split xs
    in [p] ++ pack r

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (myLength x, head x)) $ pack xs

-- Problem 11
data RLE a = Multiple Int a | Single a deriving (Show)
rle :: (Eq a) => (Int, a) -> RLE a
rle (n, x)
    | n == 1 = Single x
    | n > 1 = Multiple n x
    | otherwise = error "Impossible RLE spec"

encodeModified :: (Eq a) => [a] -> [RLE a]
encodeModified = map rle . encode

-- Problem 12
decodeModified :: [RLE a] -> [a]
decodeModified = concatMap decodeRLE 
    where decodeRLE :: RLE a -> [a]
          decodeRLE (Single a) = [a]
          decodeRLE (Multiple n a) = take n (repeat a)

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [RLE a]
encodeDirect [] = []
encodeDirect (x:[]) = [Single x]
encodeDirect xs =
    let count :: (Eq a) => [a] -> (Int, a, [a])
        count [x] = (1, x, [])
        count (x:y:xs)
            | x == y = (1 + c, x, r)
            | otherwise = (1, x, y:xs)
            where (c, _, r) = count $ y:xs 
        (c, ch, r) = count xs
    in rle (c, ch):(encodeDirect r)

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs n = concatMap (take n . repeat) xs

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs k = dropOn xs 1 k
    where dropOn :: [a] -> Int -> Int -> [a]
          dropOn [] _ _ = []
          dropOn (x:xs) i k
              | i `mod` k == 0 = dropOn xs (i+1) k
              | otherwise = x:(dropOn xs (i+1) k)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = splitTake [] xs 0 n
    where splitTake :: [a] -> [a] -> Int -> Int -> ([a], [a])
          splitTake s [] _ _ = (s, [])
          splitTake s (x:xs) i n
              | i == n = (s, x:xs)
              | otherwise = splitTake (s ++ [x]) xs (i+1) n

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs u v = take (v-u+1) r
    where (_, r) = split xs (u-1)
