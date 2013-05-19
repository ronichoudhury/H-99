myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs
myLast _ = error "myLast called on empty list"

myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
myButLast _ = error "myButLast called on too short a list"

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs $ k - 1
elementAt _ _ = error "Index out of range"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
