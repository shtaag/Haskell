module Cylinder where

import Graphics.UI.GLUT

data RotateDirection = Lef | Righ

data CylinderInfo =
  CylinderInfo {
    wid :: Float,
    hei :: Float,
    speed :: Float,
    direction :: RotateDirection,
    num_dots :: Int,
    dot_size :: Int,
    dot_color :: Color3 GLdouble
  }

draw :: CylinderInfo -> IO()
draw info = 
  do 
     clear [ColorBuffer]
     flush