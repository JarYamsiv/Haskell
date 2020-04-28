import Graphics.UI.GLUT

-- now the points can be created from a function
myPoints ::Float -> [(GLfloat,GLfloat,GLfloat)]
myPoints x = [ (sin (2*pi*k/x), cos (2*pi*k/x), 0) | k <- [1..x] ]

main :: IO ()
main = do
  putStrLn "enter the number of points"
  input1 <- getLine
  let x = (read input1 :: Float)
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display x
  mainLoop

display ::Float -> DisplayCallback
display x = do 
  clear [ColorBuffer]
  renderPrimitive Polygon $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ pts
  flush
  where pts = myPoints x
