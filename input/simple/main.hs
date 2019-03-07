main =  do
  putStrLn "enter value for x: "
  input1 <- getLine
  putStrLn "enter value for y: " 
  input2 <- getLine 
  input3 <- getLine

  let x = (read input1 :: Int)
  let y = (read input2 :: Int)

  

  return ()


