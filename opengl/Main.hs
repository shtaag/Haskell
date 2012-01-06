import Graphics.UI.GLUT
import Bindings
import Cylinder
import Data.IORef

data WindowInfo 
     = WindowInfo {
         wwid :: Int,
         whei :: Int,
         wpos_x :: Int,
         wpos_y :: Int
         }

window1 :: WindowInfo
window1 = WindowInfo {
  wwid = 600, whei = 400, wpos_x = 100, wpos_y = 100
  }
  
cylinder1 :: CylinderInfo
cylinder1 = 
  CylinderInfo { 
    wid = 500, hei = 600, delta = 0.5, num_dots = 150, dot_size = 5.0, dot_color = Color3 0.2 0.1 0.9
    }
  
main :: IO()
main =
  do getArgsAndInitialize
     
     initialWindowSize $= Size (fromIntegral $ wwid window1) (fromIntegral $ whei window1)
     initialWindowPosition $= Position (fromIntegral $ wpos_x window1) (fromIntegral $ wpos_y window1)
     initialDisplayMode $= [DoubleBuffered, RGBMode]
     
     createWindow "UllmanCylinder"
     reshapeCallback $= Just reshapeWindow -- omissible
     
     angle <- newIORef (0.0::GLfloat)
     delta <- newIORef (realToFrac $ delta cylinder1)
     idleCallback $= Just (idle angle delta)
     
     displayCallback $= draw cylinder1 angle
     
     mainLoop
  
reshapeWindow :: Size -> IO ()
reshapeWindow size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    -- ortho2D left right top bottom -- near::-1 far::1
    lookAt (Vertex3 0.0 0.0 (-1.0)) -- 視点がどこか
           (Vertex3 0.0 0.0 1.0) -- どこを見るか
           (Vector3 0.0 1.0 0.0) -- どちらが上か
    -- translate $ Vector3 0.0 0.0 (-2.0::GLfloat) 
    matrixMode $= Modelview 0
      where
         left   = (-(fromIntegral w)/640):: GLdouble
         right  = ( (fromIntegral w)/640):: GLdouble
         top    = (-(fromIntegral h)/480):: GLdouble
         bottom = ( (fromIntegral h)/480):: GLdouble
