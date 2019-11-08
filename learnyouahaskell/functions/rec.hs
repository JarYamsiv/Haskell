-- we can replace the maxtail by (maximum' xs)
maximum' []  = error "No max for empty list"
maximum' [x] = x
maximum' (x:xs)
 | x > maxtail = x
 | otherwise = maxtail
 where maxtail =  maximum xs

-- In the above kind of splitting usig | whatever comes after the | should be a boolean expr


-- still no idea what that Ord a is about
-- I think it is to generalise this function to whoever is an
-- instance to the ord class
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- $ and dot
-- $ is used to force right associativity;  
-- like SML haskell is left assoiciative
-- eg
fun_a :: (Integral a) => a -> a
fun_a x = x+1

fun_b :: (Integral a) => a -> a
fun_b x = x^2

fun_c :: (Integral a) => a -> a
fun_c = fun_a.fun_b -- means a(b(x))

-- but $ has different meaning
a = fun_b $ 1+1 -- creates 2^2 rather that 1^2+1

-- using fn k=> is haskell
addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z

--which can be done with
addThree' :: (Num a) => a -> a -> a -> a  
addThree' = \x -> \y -> \z -> x + y + z   

-- fn x=> fn y=> can also be written as \x y

--higher order function and foldl operator
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0  

-- foldl1 and foldr1 needn't require and extra value it will take it from the list
-- similiarly scanl will appy the operator with all values starting from left
a1 = scanl (+) 0 [0,3,4,5]
-- will yeild [0,0,3,7,12]