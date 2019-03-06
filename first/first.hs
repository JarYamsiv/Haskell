--Functions and variable names should start with lowecase letteres
--Constructor names should start with uppercase letters

double x = x+x

factorial 0 = 1
factorial n = n * factorial (n-1)

data Shape = Circle Float | Square Float | Yeet [Char]

area (Circle x) = x*x*3.14
area (Square x) = x*x

main = do 
   let var1 = 2 
   let var2 = 3 
   let var3 = area(Circle 3.0)
   let var4 = area(Square 2.0)
   let someVar = (Yeet "meow")
   -- IF we type putStrLn then it will add a newline after the string printed
   putStr "The addition of the two numbers is:" 
   print(var1 + double var2) 
   putStr "The areas are:"
   print(var3,var4)
