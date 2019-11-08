-- do is used to glue statements together.. maybe like how I used the 
-- let blocks in SML

-- wow this works 
read_ = do
    name <- getLine
    return name

main = do  
    putStrLn "Hello, what's your name?"  
    let fun ()  = "5" -- in case you want function defined inside

    let read_new = do
        name <- getLine
        let k = fun () -- and this format if the function doesn't have any do blocks
        putStrLn $ "yo" ++ name ++ k
        return name

    name <- getLine  
    a <- read_
    new_name <- read_new -- function that have a do inside can be called like this

    putStrLn ("Hey " ++ strong name ++ a ++ ", you rock!")  
    where strong x = x --the string that we got from IO can be passed to function.. (simple stuff)
