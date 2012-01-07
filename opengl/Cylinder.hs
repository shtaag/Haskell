module Cylinder where

import Graphics.UI.GLUT
import Points
import Cube

data CylinderInfo =
  CylinderInfo {
    wid :: Int,
    hei :: Int,
    delta :: Float,
    num_dots :: Int,
    dot_size :: Float,
    dot_color :: Color3 GLfloat
  }

draw info angle = do 
  clear [ColorBuffer]
  loadIdentity
  -- 描画倍率の指定
  scale 0.001 0.001 (0.001::GLfloat)
  mapM_ (\(x,y,z) -> preservingMatrix $ do 
            matrixMode $= Modelview 0
            translate $ Vector3 x y z
            color $ dot_color info
            
            -- rotate the world
            matrixMode $= Projection
            a <- get angle
            rotate a $ Vector3 0.0 (1.0::GLfloat) 0.0
            
            cube ((realToFrac $ dot_size info)::GLfloat)
            loadIdentity -- necessary to calc each cube
        ) $ cylpoints (wid info) (hei info) (num_dots info)
  swapBuffers
     
idle angle delta = do 
  a <- get angle
  d <- get delta
  angle $=! (a+d)
  postRedisplay Nothing
  