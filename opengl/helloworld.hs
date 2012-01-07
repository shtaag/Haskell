import Graphics.UI.GLUT

main = do
  getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= clear [ ColorBuffer ]
  mainLoop