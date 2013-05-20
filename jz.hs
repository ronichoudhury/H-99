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
encodeModified :: (Eq a) => [a] -> [RLE a]
encodeModified = map rle . encode
    where rle :: (Eq a) => (Int, a) -> RLE a
          rle (n, x)
              | n == 1 = Single x
              | n > 1 = Multiple n x
              | otherwise = error "Impossible RLE spec"

-- Problem 12
decodeModified :: [RLE a] -> [a]
decodeModified = concatMap decodeRLE 
    where decodeRLE :: RLE a -> [a]
          decodeRLE (Single a) = [a]
          decodeRLE (Multiple n a) = take n (repeat a)
