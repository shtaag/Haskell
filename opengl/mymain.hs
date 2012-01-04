import Graphics.UI.GLUT
import Bindings
import Data.IORef

-- myPoints :: [(GLfloat, GLfloat, GLfloat)]
-- myPoints = map (\k -> (sin(2*pi*k/12), cos(2*pi*k/12), 0.0)) [1..12]

main :: IO()
main
 = do getArgsAndInitialize
      --initialWindowSize $= Size (floor $ width test1) (floor $height test1)
      --initialWindowPosition $= Position 100 100
      --initialDisplayMode $= [DoubleBuffered , RGBMode]

      createWindow "Gragh with Haskell"

      -- displayCallback $= drawGragh test1
      reshapeCallback $= Just reshape

      angle <- newIORef (0.0::GLfloat)
      delta <- newIORef (0.1::GLfloat)
      position <- newIORef (0.0::GLfloat, 0.0)
      keyboardMouseCallback $= Just (keyboardMouse delta position)
      idleCallback $= Just (idle angle delta)
      displayCallback $= (display angle position)
      mainLoop

-- myDisp = do
--   clear [ColorBuffer]
--   renderPrimitive Points $ mapM_ (\(x,y,z) -> vertex $Vertex3 x y z) myPoints
  
--  flush