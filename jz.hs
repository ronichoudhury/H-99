myLast :: [a] -> a
myLast [] = error "myLast called on empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs
