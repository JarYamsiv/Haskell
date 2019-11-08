lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"   

factorial :: (Integral a) => a-> a
factorial 0 = 1
factorial n = n * factorial (n-1)

-- this methode also works
fact_test :: Int -> Int
fact_test 0 = 1
fact_test n = n* fact_test (n-1)

--syntax capture
addVectors :: (Num a)=> (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2 , y1+y2)

head' :: [a] -> a
head' [] = error "no head for empty list"
head' (x:_) = x

--pattern matching for single element list
tail' :: [a] -> a
tail' [] = error "No tail for empty list"
tail' [a] = a
tail' (_:xs) = tail' (xs)

--I dont know what tey are called 
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

--special pattern matching
--this captures the pattern twice
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

--Guards
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

--Inverse of let in end block from Standard ML
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / (height ^ 2 ) 
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

--let blocks
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

--case beware of the spaces
--they have to match exactly unlike SML cannot use | here
headc :: [a] -> a  
headc xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  


--pattern matching non-tedious way
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
