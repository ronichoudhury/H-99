myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs
myLast _ = error "myLast called on empty list"

myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
myButLast _ = error "myButLast called on too short a list"
