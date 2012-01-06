module Cylinder where

import Graphics.UI.GLUT hiding (rect)
import Points
import Cube
import Rect
import Data.IORef

data CylinderInfo =
  CylinderInfo {
    wid :: Float,
    hei :: Float,
    delta :: Float,
    num_dots :: Int,
    dot_size :: Int,
    dot_color :: Color3 GLdouble
  }

draw info angle = do 
  clear [ColorBuffer]
  loadIdentity
  -- (x,y,z) <- get position
  -- translate $ Vector3 x y z
  -- preservingMatrix $
  --   do 
  scale 0.001 0.001 (0.001::GLfloat)
  mapM_ (\(x,y,z) -> preservingMatrix $ do 
            matrixMode $= Modelview 0
            translate $ Vector3 x y z
            color $ Color3 0.2 0.2 (0.4::GLfloat)
              
            -- rotate the world
            matrixMode $= Projection
            a <- get angle
            rotate a $ Vector3 0 (0.5::GLfloat) 0
            cube (5.0::GLfloat)
            loadIdentity
        ) $ cylpoints 500 400 150
  swapBuffers
     
idle angle delta = do 
  a <- get angle
  d <- get delta
  angle $=! (a+d)
  postRedisplay Nothing