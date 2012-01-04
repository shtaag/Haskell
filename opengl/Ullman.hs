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
window1 = WindowInfo { wwid = 600, whei = 400, wpos_x = 100, wpos_y = 100}
  
cylinder1 :: CylinderInfo
cylinder1 = 
  CylinderInfo { 
    wid = 300,
    hei = 200,
    speed = 10,
    direction = Righ,
    num_dots = 1,
    dot_size = 3,
    dot_color = Color3 0 1.0 0.2
    }
  
main :: IO()
main =
  do getArgsAndInitialize
     
     -- initialWindowSize $= Size (width window1) (height window1)
     initialWindowSize $= Size 600 400
     -- initialWindowPosition $= Position (wpos_x window1) (wpos_y window1)
     initialWindowPosition $= Position 100 100
     initialDisplayMode $= [DoubleBuffered, RGBMode]
     
     createWindow "UllmanCylinder"
     
     displayCallback $= draw cylinder1
     
     mainLoop
  