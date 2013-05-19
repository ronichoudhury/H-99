myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast _ = error "myLast called on empty list"

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "myButLast called on too short a list"

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs $ k - 1
elementAt _ _ = error "Index out of range"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = 
    let sameList :: (Eq a) => [a] -> [a] -> Bool
        sameList [] [] = True
        sameList (x:xs) (y:ys) = x == y && sameList xs ys
    in sameList xs $ myReverse xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ (flatten $ List xs)
flatten (List []) = []

compress :: (Eq a) => [a] -> [a]
compress (x:y:xs)
    | x == y = compress $ y:xs
    | otherwise = x:(compress $ y:xs)
compress xs = xs

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

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (myLength x, head x)) $ pack xs
