import Graphics.UI.GLUT

-- now the points can be created from a function
myPoints ::Float -> [(GLfloat,GLfloat,GLfloat)]
myPoints x = [ (sin (2*pi*k/x), cos (2*pi*k/x), 0) | k <- [1..x] ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive Polygon $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ pts
  flush
  where pts = myPoints 100
