module Bindings (reshape, keyboardMouse) where
import Graphics.UI.GLUT
import Data.IORef


reshape s@(Size w h) =
  do viewport $= (Position 0 0, s)

keyboardAct a p (Char ' ') Down =
  do a' <- get a
     a $= -a'

keyboardAct a p (Char '+') Down =
  do a' <- get a
     a $= 2*a'

keyboardAct a p (Char '-') Down =
  do a' <- get a
     a $= a'/2

keyboardAct a p (SpecialKey KeyLeft) Down =
  do (x,y) <- get p
     p $= (x-0.1, y)

keyboardAct a p (SpecialKey KeyRight) Down =
  do (x,y) <- get p
     p $= (x+0.1, y)

keyboardAct a p (SpecialKey KeyUp) Down =
  do (x,y) <- get p
     p $= (x, y+0.1)
     
keyboardAct a p (SpecialKey KeyDown) Down =
  do (x,y) <- get p
     p $= (x, y-0.1)

keyboardAct _ _ _ _ = return()

-- example from OpenGLtutrial2
keyboardMouse angle pos key state modifiers position = 
  do keyboardAct angle pos key state